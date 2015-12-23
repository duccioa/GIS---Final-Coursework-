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
london_pol40 <- readOGR("./shapes/london/clean", "london_clean_40")
plot(london_pol40, col = "red", border = "white")
plot(london_boundaries, col = "black", add = TRUE)

alpha_col <- add.alpha("red", 0.5)
london_cmi <- SPDF.Cmi_Index(london_pol40)#Calculate compactness
#Analysis of the sign
london_cmi@data$col <- "green"
upper_limit <- 0.98
lower_limit <- 0.02
london_cmi@data$col[london_cmi@data$C_mi >= quantile(london_cmi@data$C_mi, upper_limit)] <- "blue"
london_cmi@data$col[london_cmi@data$C_mi <= quantile(london_cmi@data$C_mi, lower_limit)] <- "yellow"
london_cmi@data$col[london_cmi@data$C_mi>0] <- "red"
plot(london_cmi, col = london_cmi@data$col)




