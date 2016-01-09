calc.dist.matrix <- function(sp, nc){
    require(doParallel)
    require(sp)
    registerDoParallel(cores = nc)
    centroids_coords <- coordinates(sp)
    dist_matrix  <- foreach(i = 1:nrow(centroids_coords), .combine = "cbind") %dopar% 
        calc.inv.dist(centroids_coords, i)
    
}
########################################
##Input a matrix with two columns of coordinates x,y and an index i and return the vector of the inverted distnces 
##of the point i with all the other points
##It is used to build an inverted distance matrix
calc.inv.dist <- function(coords, i){
    x1 <- coords[i,1]
    y1 <- coords[i, 2]
    1/sqrt((x1-coords[,1])^2 + (y1-coords[,2])^2)
}

################################################################################
################################################################################