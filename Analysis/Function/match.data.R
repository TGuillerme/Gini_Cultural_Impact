#' @title Matching the data for anlysis
#'
#' @description Prepares the distance matrix and the tree for a particular year
#'
#' @param variables A matrix containing the lattitude longitude coordinates and the languages.
#' @param tree One or multiple language trees to matchs.
#' @param ginis A table contianing ginis indices.
#' @param year Which year to consider
#' @param ... Optional arguments to be passed to \code{\link[geosphere]{distGeo}}.
#'
#' @author Thomas Guillerme

match.data <- function(variables, tree, ginis, year, ...) {

    #Get the ginis with the from the right year
    matching <- match(ginis$year, year)
    no_matching <- which(is.na(matching))
    ginis_out <- ginis[-no_matching,]

    #Getting the list of countries
    countries_out <- as.character(ginis_out$country)

    #Getting the list of variables
    matching <- match(variables[,1], countries_out)
    no_matching <- which(is.na(matching))
    variables_out <- variables[-no_matching,]

    #Creating the distance matrix
    long_lat <- variables_out[!duplicated(variables_out[,1:4]), 3:4]
    row.names(long_lat) <- as.character(variables_out[!duplicated(variables_out[,1:4]), 1])
    capital_distances <- geodesic.matrix(long_lat)

    #Creating the sub_language tree
    tree.match <- function(tree, variables) {
        matching <- match(tree$tip.label, unique(as.character(variables)))    
        no_matching <- which(is.na(matching))
        tree_out <- drop.tip(tree, tree$tip.label[no_matching])
        return(tree_out)
    }
    if(class(tree) != "phylo") {
        tree_out <- lapply(tree, tree.match, variables = sub_variables[,5])
        class(tree_out) <- "multiPhylo"
    } else {
        tree_out <- tree.match(tree, variables = sub_variables[,5])
    }

    #Getting the language for each country saved


    # Select the ginis for that year
    gini_indices <- ginis_language[which(ginis_language$year == year_with_most_data),]
    # Add the languages to the data
    gini_indices$language <- unlist(lapply(as.list(gini_indices$country), match.country.language, country_variables[,c(1,5)], languages))

    cat(paste(head(counts_per_year[with(counts_per_year, order(-freq)),] )[1,2], "gini indices saved for the year", year_with_most_data, "in 'gini_indices'"))




    #Results output
    list_out <- list(ginis_out, capital_distances, tree_out)
    names(list_out) <- c(paste("Ginis_", year, sep=""), "Capital_distances", "Language_tree")
    return(list_out)
}