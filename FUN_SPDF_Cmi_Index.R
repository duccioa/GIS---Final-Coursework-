################################################################################
############# CALCULATE Cmi INDEX OF A SPATIAL POLYGON DATA FRAME ##############
################################################################################


##Add a column of C_mi indexes to the polygon dataframe
SPDF.Cmi_Index <- function(spdf){
    require(sp)
    if(!exists("Polygon.Cmi_index", mode="function")) {
        source("./FUN_Polygon_Cmi.R")
        }
    n <- length(spdf@polygons)#fetch total number of polygons
    polygons.areas <- extract.areas(spdf)
    Cmi_indexes <- rep(0, n)#create an empty vector
    for(i in 1:n){
        #calculate the index of compactnes for each polygon
        Cmi_indexes[i] <- Polygon.Cmi_Index(spdf, i)
    }
    #add the column with the index to the polygon's
    spdf@data$C_mi <- Cmi_indexes 
    return(spdf)
}


