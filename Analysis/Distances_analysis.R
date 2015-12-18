#Getting the data
library(picante)
library(vegan)
source("Read_data.R")

#Getting the data for year 2005
ginis_2005 <- match.data(sub_variables, tree, sub_ginis, 2005)

#Capital distances
capital_distances <- ginis_2005[[2]]

#Language distances
language_distances <- cophenetic(ginis_2005[[3]][[1]]) # gives the exact same distances with the second tree


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
