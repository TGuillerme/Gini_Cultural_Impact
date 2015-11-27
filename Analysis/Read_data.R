library(plyr)

## Data input script

## Get the Gini index
ginis <- read.csv("../Data/Gini_index.csv")
## Remove the NAs
ginis <- ginis[-which(is.na(ginis$Gini)),]

## Selecting the years with the most ginis (i.e. the most countries)
counts_per_year <- count(ginis$year)
head(counts_per_year[with(counts_per_year, order(-freq)),] )

## We'll have to chose the best strategy for getting this analysis through time going...
## For now, I'll just chose the year 2005 since it has the most data