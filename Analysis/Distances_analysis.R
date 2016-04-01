#Getting the data
if(!require(picante)) install.packages("picante")
if(!require(vegan)) install.packages("vegan")
library(vegan) ; library(picante)
source("Read_data.R")

#Getting the data for year 2005
ginis_2005 <- match.data(sub_variables, tree, sub_ginis, 2005)

#Capital distances
capital_distances <- ginis_2005[[2]]

#Language distances
language_distances <- cophenetic(ginis_2005[[3]][[1]]) # gives the exact same distances with the second tree

#Ginis scores for 2005
gini <- as.data.frame(ginis_2005[[1]][,3])
colnames(gini) <- "Gini" ; rownames(gini) <- ginis_2005[[1]][,1]

#Calculating the relation between ginis scores and the distance between capitals

#dummy variable
dummy_var <- as.data.frame(rnorm(nrow(gini))) ; rownames(dummy_var) <- ginis_2005[[1]][,1]

model <- lm(gini[,1]~dummy_var[,1])

plot(dummy_var[,1], gini[,1])
abline(model)

######
#
# Need a question to test the effect: for example, what is the relation between Gini and (ideally) some independent continuous variable such as per capita beer consumption.
# Then we can look at (1) the phylogenetic signal and (2) the capital distance signal and see how, when including this signal does the per capita beer consumption actually relates to the Gini.
#
#######

# Calculating the importance of the phylogenetic signal in the Gini scores



#picante::Kcalc
#picante::phylosignal


gini <- as.data.frame(ginis_2005[[1]][,3])
colnames(gini) <- "Gini" ; rownames(gini) <- ginis_2005[[1]][,1]

model_capital <- adonis(capital_distances~Gini, data=gini, method="euclidean", permutation=1000)

summary(model_capital)

model_capital <- adonis(gini~capital_distances, method="euclidean", permutation=1000)



tre <- rtree(15, tip.label =LETTERS[1:15])
data <- data.frame(tips=c(LETTERS[1:15], sample(LETTERS[1:15],15, replace=TRUE)), val=rnorm(30))

phylosignal(data, tree, reps = 999, checkdata=TRUE)
