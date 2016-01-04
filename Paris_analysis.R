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
paris_pol40 <- readOGR("../shapes/paris/clean", "paris_clean_40")
paris_boundaries <- readOGR("./shapes/paris/boundaries", "paris_boundaries")
plot(paris_pol40, col = "red", border = "white")
plot(paris_boundaries, col = "black", add = TRUE)

paris_cmi <- SPDF.Cmi_Index(paris_pol40)#Calculate compactness
summary(paris_cmi@data$C_mi)
paris_int_values <- paris_cmi[paris_cmi@data$C_mi != Inf & paris_cmi@data$C_mi >= 0,]
summary(paris_int_values@data$C_mi)
paris_int_values <- paris_int_values[paris_int_values@data$C_mi < 1,]
hist(paris_int_values@data$C_mi, breaks = 100)
plot_C_mi(paris_int_values, plot_variable = "C_mi", n = 4, col_pal = "YlGnBu", style_intervals = "quantile")



