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
paris_pol40 <- readOGR("./shapes/paris/clean", "paris_clean_40")
paris_boundaries <- readOGR("./shapes/paris/boundaries", "paris_boundaries")
plot(paris_pol40, col = "red", border = "white")
plot(paris_boundaries, col = "black", add = TRUE)

alpha_col <- add.alpha("red", 0.5)
paris_cmi <- SPDF.Cmi_Index(paris_pol40)#Calculate compactness
#Analysis of the sign
paris_cmi@data$col <- "green"
upper_limit <- 0.98
lower_limit <- 0.02
paris_cmi@data$col[paris_cmi@data$C_mi >= quantile(paris_cmi@data$C_mi, upper_limit)] <- "blue"
paris_cmi@data$col[paris_cmi@data$C_mi <= quantile(paris_cmi@data$C_mi, lower_limit)] <- "yellow"
paris_cmi@data$col[paris_cmi@data$C_mi>0] <- "red"
plot(paris_cmi, col = paris_cmi@data$col)




