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

london_pol40 <- SPDF.Cmi_Index(london_pol40)#Calculate compactness
london_pol40@data$C_mi <- abs(london_pol40@data$C_mi)

alpha_col <- add.alpha("blue", 0.5)
plot(london_pol40@data$id, london_pol40@data$C_mi, 
     ylim = c(-0.000005,0.000005), col = alpha_col, pch = 20, cex = 1)


#debug of the analysis
areas <- extract.areas(london_pol40)
london_Cmi <- SPDF.Cmi_Index(london_pol40)
