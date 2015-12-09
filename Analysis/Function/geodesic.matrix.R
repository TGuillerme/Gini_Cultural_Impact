#' @title Caculate the geodesic distance matrix
#'
#' @description Calculates the geodesic distances between points using a lattitude/longitude matrix
#'
#' @param matrix A matrix containing the lattitude longitude coordinates and the points names as rownames.
#' @param ... Optional arguments to be passed to \code{\link[geosphere]{distGeo}}.
#'
#' @author Thomas Guillerme

geodesic.matrix <- function(matrix, ...) {

    require(geosphere)

    #CACLULATING THE PAIRWISE DISTANCES
    #Getting the pairs of distances
    pair_series <- combn(1:nrow(matrix), 2) 
    pair_series <- unlist(apply(pair_series, 2, list), recursive=FALSE)

    #lapply wrapper
    distGeo.lapply <- function(list_of_pairs, matrix){
        return(distGeo(matrix[list_of_pairs[1],], matrix[list_of_pairs[2],]))
    }

    #Calculating the distances
    pair_distances <- lapply(pair_series, distGeo.lapply, matrix)

    #Saving the results
    matrix_out <- matrix(NA, ncol=nrow(matrix), nrow=nrow(matrix))
    rownames(matrix_out) <- colnames(matrix_out) <- rownames(matrix)
    #Set the diagonal to 0
    diag(matrix_out) <- 0
    #Get the pair distances
    matrix_out[lower.tri(matrix_out)] <- unlist(pair_distances)
    matrix_out <- as.matrix(as.dist(matrix_out, diag=TRUE))

    #Output
    return(matrix_out)
}