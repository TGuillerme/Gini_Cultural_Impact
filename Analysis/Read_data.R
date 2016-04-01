if(!require(plyr)) install.packages("plyr")
if(!require(geosphere)) install.packages("geosphere")
library(plyr); library(geosphere)
source("Function/geodesic.matrix.R")
source("Function/match.data.R")

## Data input script

######################
## Get the countries variables
######################
country_variables <- read.csv("../Data/Country_variables.csv", stringsAsFactors = FALSE)


######################
## Get the Gini index
######################
ginis <- read.csv("../Data/Gini_index.csv", stringsAsFactors = FALSE)
## Remove the NAs
ginis <- ginis[-which(is.na(ginis$Gini)),]

######################
## Get the language tree
######################
tree <- read.nexus("../Data/Bouckaert.et.al.2012-103t-2MCC.tre")
# Drop the fossil (extinct) languages
tree <- lapply(tree, drop.fossil)
# Collapse clade with multiple languages (i.e. old, etc...)
# Albanian
tree <- lapply(tree, drop.tip, c("Albanian_G","Albanian_K","Albanian_Top"))
tree[[1]]$tip.label[match("Albanian_C",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Albanian_C",tree[[2]]$tip.label)] <- "Albanian"
# Armenian
tree <- lapply(tree, drop.tip, c("Armenian_Mod"))
tree[[1]]$tip.label[match("Armenian_List",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Armenian_List",tree[[2]]$tip.label)] <- "Armenian"
# Breton
tree <- lapply(tree, drop.tip, c("Breton_SE", "Breton_ST"))
tree[[1]]$tip.label[match("Breton_List",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Breton_List",tree[[2]]$tip.label)] <- "Breton"
# Czech
tree <- lapply(tree, drop.tip, c("Czech_E"))
# Dutch (just renaming)
tree[[1]]$tip.label[match("Dutch_List",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Dutch_List",tree[[2]]$tip.label)] <- "Dutch"
# English (just renaming)
tree[[1]]$tip.label[match("English_ST",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("English_ST",tree[[2]]$tip.label)] <- "English"
# German (just renaming)
tree[[1]]$tip.label[match("German_ST",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("German_ST",tree[[2]]$tip.label)] <- "German"
# Greek
tree <- lapply(tree, drop.tip, c("Greek_Mod"))
tree[[1]]$tip.label[match("Greek_ML",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Greek_ML",tree[[2]]$tip.label)] <- "Greek"
# Icelandic (just renaming)
tree[[1]]$tip.label[match("Icelandic_ST",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Icelandic_ST",tree[[2]]$tip.label)] <- "Icelandic"
# Irish (just renaming)
tree[[1]]$tip.label[match("Irish_A",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Irish_A",tree[[2]]$tip.label)] <- "Irish"
# Lithuanian (just renaming)
tree[[1]]$tip.label[match("Lithuanian_ST",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Lithuanian_ST",tree[[2]]$tip.label)] <- "Lithuanian"
# Lusatian
tree <- lapply(tree, drop.tip, c("Lusatian_U"))
tree[[1]]$tip.label[match("Lusatian_L",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Lusatian_L",tree[[2]]$tip.label)] <- "Lusatian"
# Nepali (just renaming)
tree[[1]]$tip.label[match("Nepali_List",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Nepali_List",tree[[2]]$tip.label)] <- "Nepali"
# Ossetic
tree <- lapply(tree, drop.tip, c("Iron_Ossetic"))
tree[[1]]$tip.label[match("Digor_Ossetic",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Digor_Ossetic",tree[[2]]$tip.label)] <- "Ossetic"
# Persian (just renaming)
tree[[1]]$tip.label[match("Persian_List",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Persian_List",tree[[2]]$tip.label)] <- "Persian"
# Portuguese (just renaming)
tree[[1]]$tip.label[match("Portuguese_ST",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Portuguese_ST",tree[[2]]$tip.label)] <- "Portuguese"
# Romanian (just renaming)
tree[[1]]$tip.label[match("Romanian_List",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Romanian_List",tree[[2]]$tip.label)] <- "Romanian"
# Sardinian
tree <- lapply(tree, drop.tip, c("Sardinian_N","Sardinian_C"))
tree[[1]]$tip.label[match("Sardinian_L",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Sardinian_L",tree[[2]]$tip.label)] <- "Sardinian"
# Swedish
tree <- lapply(tree, drop.tip, c("Swedish_Up","Swedish_VL"))
tree[[1]]$tip.label[match("Swedish_List",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Swedish_List",tree[[2]]$tip.label)] <- "Swedish"
# Welsh
tree <- lapply(tree, drop.tip, c("Welsh_N"))
tree[[1]]$tip.label[match("Welsh_C",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Welsh_C",tree[[2]]$tip.label)] <- "Welsh"
# Renaming Riksmal to Bokmal
tree[[1]]$tip.label[match("Riksmal",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Riksmal",tree[[2]]$tip.label)] <- "Bokmal"
# Renaming Byelorussian to Belarussian
tree[[1]]$tip.label[match("Byelorussian",tree[[1]]$tip.label)] <- tree[[2]]$tip.label[match("Byelorussian",tree[[2]]$tip.label)] <- "Belarussian"

#Extracting the languages
languages <- tree[[1]]$tip.label

#####################
# Create the most complete dataset (countries with gini + language + captial)
#####################

# 1 - Get the countries that have a language represented in the tree
countries_language <- country_variables[-which(is.na(match(country_variables[,5], languages))), ]$Country

# 2 - Select the which of these countries have gini indices
ginis_language <- ginis[-which(is.na(match(ginis[,1], countries_language))), ]
countries_language_gini <- unique(ginis_language$country)

# 3 - Find the year with the most ginis indices for these countries
counts_per_year <- count(ginis_language$year)
year_with_most_data <- head(counts_per_year[with(counts_per_year, order(-freq)),] )[1,1]

# 4 - Out put the saved data
# Select the ginis for that year
gini_indices <- ginis_language[which(ginis_language$year == year_with_most_data),]
# Add the languages to the data
gini_indices$language <- unlist(lapply(as.list(gini_indices$country), match.country.language, country_variables[,c(1,5)], languages))

cat(paste(head(counts_per_year[with(counts_per_year, order(-freq)),] )[1,2], "gini indices saved for the year", year_with_most_data, "in 'gini_indices'"))
