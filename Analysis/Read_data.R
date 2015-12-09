library(plyr)
library(geosphere)
source("Function/geodesic.matrix.R")
source("Function/match.data.R")

## Data input script


######################
## Get the Gini index
######################
ginis <- read.csv("../Data/Gini_index.csv")
## Remove the NAs
ginis <- ginis[-which(is.na(ginis$Gini)),]

## Selecting the years with the most ginis (i.e. the most countries)
counts_per_year <- count(ginis$year)
head(counts_per_year[with(counts_per_year, order(-freq)),] )

## We'll have to chose the best strategy for getting this analysis through time going...
## For now, I'll just chose the year 2005 since it has the most data.class
ginis_2005 <- ginis[which(ginis$year == 2005),]

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



######################
## Get the Countries variable
######################
variables <- read.csv("../Data/Country_variables.csv")
#long_lat <- variables[!duplicated(variables[,1:4]), 3:4]
#row.names(long_lat) <- as.character(variables[!duplicated(variables[,1:4]), 1])

## Creating the country distance matrix (in meters) (takes approx 10 sec)
#capital_distances <- geodesic.matrix(long_lat)

#####################
# Create the most complete dataset
#####################

# 1 - Select the countries with both language and capital distance
matching <- match(variables[,5], languages)
no_matching <- which(is.na(matching))
sub_variables <- variables[-no_matching,]

# 2 - From these countries, select only the countries with ginis
matching <- match(as.character(sub_variables[,1]), levels(ginis[,1]))
no_matching <- which(is.na(matching))
sub_variables <- sub_variables[-no_matching,]
# Ending up with 144 countries
list_of_countries <- as.character(unique(sub_variables[,1]))

# 3 - Select the years with the most ginis for these countries
matching <- match(as.character(ginis[,1]), list_of_countries)
no_matching <- which(is.na(matching))
sub_ginis <- ginis[-no_matching,]
counts_per_year <- count(sub_ginis$year)
head(counts_per_year[with(counts_per_year, order(-freq)),] )
# Let's just select the year 2005 for now that has 71 countries


ginis_2005 <- match.data(sub_variables, tree, sub_ginis, 2005)

