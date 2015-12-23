library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
library(polyclip)
library(plyr)
library(PBSmapping)

roads <- readOGR("./shapes/ile-de-france-latest-shape", "roads")
proj4string(roads) <- CRS("+init=epsg:4326")
create.rect <- function(x_min, x_max, y_min, y_max){
    crds <- cbind(x=c(x_min, x_max, x_max, x_min, x_min), y=c(y_min, y_min, y_max, y_max, y_min))
    Pl <- Polygon(crds)
    ID <- "Crop"
    Pls <- Polygons(list(Pl), ID=ID)
    SPls <- SpatialPolygons(list(Pls))
    df <- data.frame(value=1, row.names=ID)
    return(SpatialPolygonsDataFrame(SPls, df))
}
##For the coordinates of the bounding box, see openstreetmap website in the export
#extract of 15th arr.
x_min <- 2.2673
x_max <- 2.3003
y_min <- 48.8263
y_max <- 48.8503
#extract Paris
x_min <- 2.3250
x_max <- 2.3808
y_min <- 48.8413
y_max <- 48.8657

bb <- create.rect(x_min, x_max, y_min, y_max)
proj4string(bb) <- CRS("+init=epsg:4326")
roads_crop <- roads[bb,]



primary <- roads_crop[roads_crop@data$type == "primary",]
secondary <- roads_crop[roads_crop@data$type == "secondary",]
tertiary <- roads_crop[roads_crop@data$type == "tertiary",]
residential <- roads_crop[roads_crop@data$type == "residential",]
plot(roads_crop, col = "grey", lwd = 1)
plot(primary, col = "red", lwd = 5, add = T)
plot(secondary, col = "blue", lwd = 3,  add = T)
plot(tertiary, col = "green",lwd = 2,  add = T)
plot(residential, col = "orange", lwd = 2, add = T)
test <- gUnion(primary, secondary)
test <- gUnion(test, tertiary)
test <- gUnion(test, residential)
sldf <- SpatialLinesDataFrame(test, data.frame(name ="ashape"), match.ID = FALSE)
writeOGR(sldf, "./", "paris_roads", driver = "ESRI Shapefile")


test <- SpatialLines2PolySet(test)
test <- PolySet2SpatialPolygons(test, close_polys=F)
plot(test, col = "red")


crop <- function(shape, h_cor = 0.5, v_cor = 0.5, x_shift = 0, y_shift = 0){
    require(sp)
    bb <- bbox(shape)
    x_min <- bb[1,1]#*(1+h_cor/4) + x_shift
    x_max <- bb[1,2]#*(h_cor/4) + x_shift
    y_min <- bb[2,1]#*(1+v_cor/4) + y_shift
    y_max <- bb[2,2]#*(v_cor/4) + y_shift
    crds <- cbind(x=c(x_min, x_max, x_max, x_min, x_min), y=c(y_min, y_min, y_max, y_max, y_min))
    # str(crds)
    Pl <- Polygon(crds)
    # str(Pl)
    ID <- "Crop"
    Pls <- Polygons(list(Pl), ID=ID)
    # str(Pls)
    SPls <- SpatialPolygons(list(Pls))
    # str(SPls)
    df <- data.frame(value=1, row.names=ID)
    # str(df)
    return(SpatialPolygonsDataFrame(SPls, df))
    # str(SPDF)
}




sldf <- SpatialLinesDataFrame(test, data.frame(name ="ashape"), match.ID = FALSE)
writeOGR(sldf, "./", "paris_roads", driver = "ESRI Shapefile")



