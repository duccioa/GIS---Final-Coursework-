################################################################################
############# CALCULATE Cmi INDEX OF A SPATIAL POLYGON DATA FRAME ##############
################################################################################


##Add a column of C_mi indexes to the polygon dataframe
SPDF.Cmi_Index <- function(spdf, num_cores = 1){
    require(sp)
    require(doParallel)
    source("./FUN_Polygon_Cmi.R")}
    
    n <- length(spdf@polygons)#fetch total number of polygons
    registerDoParallel(cores = num_cores)#set number of cores
    #Calculate the index of compactness C_mi in parallel
    Cmi_indexes <- foreach(i = 1:n, .combine = "c") %dopar% Polygon.Cmi_Index(spdf, i)
    #add the column with the index to the polygon's
    spdf@data$C_mi <- Cmi_indexes 
    return(spdf)
}


