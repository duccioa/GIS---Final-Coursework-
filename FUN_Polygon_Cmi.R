######################################################################
########### SUB-FUNCTIONS FOR SPDF.Cmi_Index #########################
######################################################################

require(sp)

#### Given a SpatialPolygonDataFrame and a polygon's ID
#### return the Cmi index of the polygon

Polygon.Cmi_Index <- function(sp, id){
    require(sp)
    Area_polygon <- sp@polygons[id][[1]]@Polygons[[1]]@area
    V_lines <- fetch.lines(sp, id)
    V_dataframe <- create.df(V_lines)
    C_mi <- calc.Cmi_index(V_dataframe, Area_polygon)
    return(C_mi)
}

################################################################################
######## SUB-FUNCTIONS for Polygon.Cmi_index ###################################

#Input a SpatialPolygonDataFrame and extract all the polygons' areas
extract.areas <- function(sp){
    require(sp)
    n <- length(sp@polygons)
    areas <- rep(0, n)
    for(i in 1:n){
        areas[i] <- sp@polygons[i][[1]]@Polygons[[1]]@area
    }
    return(areas)
}
################################################################################
#Calculate the area of the polygon based on its coordinates
calc.A_polygon <- function(sp, id){
    require(sp)
    coords <- sp@polygons[id][[1]]@Polygons[[1]]@coords
    area <- 0
    j <- nrow(coords)-1
    for(i in 1:(nrow(coords)-1)){
        area <- area + (coords[j,1]+coords[i,1])*(coords[j,2]-coords[i,2])
        j <- i
    }
    return(area/2)
}
################################################################################
#creates the matrix V with coordinates x1,y1,x2,y2 
#of each segment of the polygon
fetch.lines <- function(sp, id){
    require(sp)
    #extract the coordinates of the vertex
    coords <- sp@polygons[id][[1]]@Polygons[[1]]@coords
    n <- nrow(coords)
    #if the area of the polygon from the coordinates is negative, 
    #invert the coordinates
    if(calc.A_polygon(sp, id) < 0){coords <- coords[n:1, ]}
    v <- cbind(coords[1:(n-1),], coords[2:n,])
    return(v)
}
################################################################################
#Given one vector with coordinates x1,y1,x2,y2 the function return
#Areas and MI of the triangle and rectangle for the line between the two vertex
#v[1]=x1
#v[2]=y1
#v[3]=x2
#v[4]=y2
#To be applied to matrix V
#Caclulate area
calc.A_tr <- function(v){
    A_tr <- (v[3]-v[1])*(v[4]-v[2])/2#area of the triangle
    return(A_tr)
}
calc.A_rec <- function(v){
    A_rec <- (v[3]-v[1])*v[2]#area of the rectangle
    return(A_rec)
}
################################################################################
#calculate x_g and y_g coordinates
#specify the required coordinate "x" or "y"
#specify the shape "tri" or "rec"
calc.coords_g <- function(v, x_or_y = "x", shape = "tri"){
    if(x_or_y == "x"){
        if(shape == "tri"){return((v[1]+2*v[3])/3)}#centroid's x_gt of the triangle
        if(shape == "rec"){return((v[1]+v[3])/2)}#centroid's x_gt of the rectangle    
    }
    if(x_or_y == "y"){
        if(shape == "tri"){return((2*v[2]+v[4])/3)}#centroid's y_gr of the triangle
        if(shape == "rec"){return(v[2]/2)}#centroid's y_gr of the rectangle
    }
}
################################################################################
#calculate MI
calc.I_tr <- function(v){v[5]*((v[3]-v[1])^2 + (v[4]-v[2])^2)/18}#I_tr

calc.I_rec <- function(v){v[6]*((v[3]-v[1])^2 + v[2]^2)/12}#I_rec
################################################################################

#Create dataframe to df_MI used to perform the final calculation of the MIg
create.df <- function(V){
    A_tr <- apply(V, 1, calc.A_tr)#Area triangle
    A_rec <- apply(V, 1, calc.A_rec)#Area rectangle
    #x coords of the triangle's centroid
    x_gt <- apply(V, 1, calc.coords_g, x_or_y = "x", shape = "tri")
    y_gt <- apply(V, 1, calc.coords_g, x_or_y = "y", shape = "tri")
    x_gr <- apply(V, 1, calc.coords_g, x_or_y = "x", shape = "rec")
    y_gr <- apply(V, 1, calc.coords_g, x_or_y = "y", shape = "rec")
    V2 <- cbind(V, A_tr, A_rec)
    I_tr <- apply(V2, 1, calc.I_tr)#MI of the triangle about its centroid
    I_rec <- apply(V2, 1, calc.I_rec)
    V_df <- data.frame(line_ID = seq(1, nrow(V)), 
                       cbind(V, x_gt, y_gt, A_tr, 
                             I_tr, x_gr, y_gr, A_rec, I_rec))
    names(V_df)[2:5] <- c("x1", "y1", "x2", "y2")
    A <- sum(A_tr + A_rec)#total area triangle and rectangle
    #Polygon's centroid
    V_df$x_g_partial <- (x_gt*A_tr + x_gr*A_rec)/A
    V_df$y_g_partial <- (y_gt*A_tr + y_gr*A_rec)/A
    x_g <- sum(V_df$x_g_partial)#x coords of the polygon's centroid
    y_g <- sum(V_df$y_g_partial)
    #
    
    V_df$Dist_gt <- sqrt((x_g - x_gt)^2 + (y_g - y_gt)^2)#distance 
    V_df$dist2_A_tr <- (V_df$Dist_gt^2)*A_tr
    V_df$Dist_gr <- sqrt((x_g - x_gr)^2 + (y_g - y_gr)^2)
    V_df$dist2_A_rec <- (V_df$Dist_gr^2)*A_rec
    V_df$Ig_partial <- with(V_df, I_tr + dist2_A_tr + I_rec + dist2_A_rec)
    return(V_df)
}
################################################################################
calc.Cmi_index <- function(V_df, A_pol){
    Ig <- sum(V_df$Ig_partial)
    C_mi <- (A_pol^2)/(2*pi*Ig) 
    return(C_mi)
}
################################################################################
################################################################################
################################################################################




