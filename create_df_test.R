library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
library(plyr)
x_coords <- c(1,2,3,4,4.5,2.5,3.5,1.5,1.5)
y_coords <- c(2,1,3,2.5,4.5,4,6,6,4)
#x_g <- mean(x_coords)
#y_g <- mean(y_coords)
coords1 <- data.frame(line_id = seq(1,8), x1 = x_coords[1:8], y1 = y_coords[1:8])
coords2 <- data.frame(line_id = seq(1,8), x2 = x_coords[2:9], y2 = y_coords[2:9])
df <- join(coords1, coords2)
#df$I_tr <- 0
#df$I_re <- 0
#df$I_g <- 0

#given a data.frame with line_ID, x1, y1, x2, y2, returns a dataframe with vertex IDs and coords
extract_coords <- function(DF){
    n <- nrow(DF)
    ids <- seq(1,n+1)
    x <- c(DF[,2], DF[n,4])
    y <- c(DF[,3], DF[n,5])
    data.frame(vertex_ID = ids, x = x, y = y)
}
