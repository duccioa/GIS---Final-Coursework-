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

alpha_col <- add.alpha("red", 0.5)
london_pol40 <- SPDF.Cmi_Index(london_pol40)#Calculate compactness
london_pol40@data$C_mi <- abs(london_pol40@data$C_mi)
#Analysis of the sign
london_pol40@data$col <- "coral"
london_pol40@data$col[london_pol40@data$C_mi < 0] <- "blue"
upper_limit <- 0.98
lower_limit <- 0.02
london_pol40@data$col[london_pol40@data$C_mi >= quantile(london_pol40@data$C_mi, 0.10)] <- "coral"
london_pol40@data$col[london_pol40@data$C_mi >= quantile(london_pol40@data$C_mi, 0.30)] <- "coral2"
london_pol40@data$col[london_pol40@data$C_mi >= quantile(london_pol40@data$C_mi, 0.50)] <- "brown1"
london_pol40@data$col[london_pol40@data$C_mi >= quantile(london_pol40@data$C_mi, 0.70)] <- "brown3"
london_pol40@data$col[london_pol40@data$C_mi >= quantile(london_pol40@data$C_mi, 0.99)] <- "firebrick"
london_pol40@data$col[london_pol40@data$C_mi >= quantile(london_pol40@data$C_mi, 0.995)] <- "darkred"


plot(london_pol40, col = london_pol40@data$col, lwd = 0.05)




