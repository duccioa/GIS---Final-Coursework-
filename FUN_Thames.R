################################################
####### REMOVE THE POLYGONS ON THE THAMES ######
################################################

##### From a list of coordinates created with locator, draw a line ######
thames.line <- function(ls){
    require(sp)
    df <- data.frame(x = ls[[1]], y = ls[[2]])
    list_c <- list()
    for(i in 1:(nrow(df)-1)){
        c1 <- cbind(df[i,1], df[i,2])
        c2 <- cbind(df[i+1,1], df[i+1,2])
        c <- rbind(c1, c2)
        c <- Line(c)
        list_c[[i]] <- c
    }
    Ls <- Lines(list_c, ID = "a")
    Ls <- SpatialLines(list(Ls))
}

########################################################################
###### Create a bounding box from a list of 4 points to zoom in ########

box <- function(ls){
    require(sp)
    ls$x[3] <- ls$x[2]
    ls$x[4] <- ls$x[1]
    ls$y[2] <- ls$y[1]
    ls$y[4] <- ls$y[3]
    c <- cbind(ls[[1]], ls[[2]])
    c <- rbind(c, c[1,])
    c <- Polygon(c)
    c <- Polygons(list(c), "c1")
    c <- SpatialPolygons(list(c), 1:1)
    return(c)
}
########################################################################
######## Return polygons' IDs from a SpatialPolygonDataFrame ###########

return.IDs <- function(spdf){
    require(sp)
    IDs <- NULL
    n <- length(spdf@polygons)
    for(i in 1:n){
        IDs <- append(IDs, spdf@polygons[[i]]@ID, after = length(IDs))
    }
    return(as.numeric(IDs))
}

########################################################################
######## Remove polygons by ID #########################################
#Deconstruct and reconstruct a SPDF removing IDs obtained from a vector
#ids is a character vector of numeric IDs
#TO BE FINISHED

remove.polygons <- function(spdf, ids){
    require(sp)
    ls_pol <- spdf@polygons
    n <- length(ls_pol)
    names(ls_pol) <- return.IDs(spdf)
    ls_pol[which(names(ls_pol) %in% ids)] <- NULL
    sp <- SpatialPolygons(ls_pol)
    new_ids <- return.IDs(sp)
    new_ids <- data.frame(IDs = new_ids, row.names = new_ids)
    spdf <- SpatialPolygonsDataFrame(sp, new_ids)
}






