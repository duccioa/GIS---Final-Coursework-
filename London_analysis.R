library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
add.alpha <- function(col, alpha=1){
    if(missing(col))
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
              rgb(x[1], x[2], x[3], alpha=alpha))  
}
london_pol40 <- readOGR("../shapes/london/clean", "london_clean_40")
plot(london_pol40, col = "red", border = "white")
plot(london_boundaries, col = "black", add = TRUE)


#debug of the analysis
##NEGATIVE VALUES
areas <- extract.areas(london_pol40)
london_Cmi <- SPDF.Cmi_Index(london_pol40)
negative_values <- london_Cmi[london_Cmi@data$C_mi < 0,]
plot(negative_values, lwd = .01, border = "cornsilk3", col = "red")
areas_negative <- extract.areas(negative_values)
coords_neg_id15 <- london_Cmi@polygons[15][[1]]@Polygons[[1]]@coords
Sr1 <- Polygon(coords_neg_id15)
Sr1 <- Polygons(list(Sr1), "s1")
Sr1 <- SpatialPolygons(list(Sr1), 1:1)
spdf <- SpatialPolygonsDataFrame(Sr1,
                                 data=data.frame(x=1, y=1, row.names=row.names(Sr1)))
plot(spdf)
Polygon.Cmi_index(spdf)###It looks like the polygon is different from the one fetched subsetting the map
spdf <- london_Cmi[london_Cmi@data$id == 15,]
##INFINITE VALUES
inf_values <- london_Cmi[london_Cmi@data$C_mi == Inf,]
plot(inf_values, lwd = .01, border = "cornsilk3", col = "blue")
head(inf_values@data, 30)
inf_values@polygons[90][[1]]@Polygons[[1]]@coords
areas_inf <- extract.areas(inf_values)
hist(areas_inf, breaks = 50)
summary(areas_inf)


#Intermediate values
int_values <- london_Cmi[london_Cmi@data$C_mi != Inf & london_Cmi@data$C_mi >= 0,]
summary(int_values@data$C_mi)
int_values <- int_values[int_values@data$C_mi < 1,]
hist(int_values@data$C_mi, breaks = 100)
plot_C_mi(int_values, plot_variable = "C_mi", n = 8, col_pal = "YlGnBu", style_intervals = "quantile")
plot_C_mi(int_values, plot_variable = "C_mi", n = 4, col_pal = "YlGnBu", style_intervals = "quantile")
