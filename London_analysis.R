library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
library(GISTools)
library(spdep)

if(!exists("SPDF.Cmi_Index", mode="function")) source("./FUN_SPDF_Cmi_Index.R")
if(!exists("plot_spdf_byClassInt", mode="function")) source("./FUN_Plot_map_byClassInt.R")
if(!exists("creat.bbox", mode="function")) source("./FUN_Create_Bbox.R")

london <- readOGR("../shapes/london/clean", "london")
#proj4string(london) <- CRS("+init=epsg:27700")#set the reference system in British National Grid
#london <- spTransform(london, CRS("+init=epsg:4326"))#Convert to WGS84
london_Cmi <- SPDF.Cmi_Index(london, 6)
london_Cmi <- london_Cmi[london_Cmi@data$C_mi > 0 & london_Cmi@data$C_mi < 1,]
summary(london_Cmi@data$C_mi)
areas <- extract.areas(london_Cmi)
london_Cmi@data <- cbind(london_Cmi@data, areas)

hist(london_Cmi@data$C_mi, breaks = 100, col = "azure4", border = "white")
hist(log(london_Cmi@data$areas), breaks = 100, col = "gray32", border = "white")

plot_spdf_byClassInt(london_Cmi, plot_variable = "C_mi", n = 8, col_pal = "YlGnBu", style_intervals = "quantile")
plot_spdf_byClassInt(london_Cmi, plot_variable = "C_mi", n = 3, col_pal = "YlGnBu", style_intervals = "quantile")
plot_spdf_byClassInt(london_Cmi, plot_variable = "areas", n = 5, col_pal = "Greys", style_intervals = "quantile", border_col = "white")

#Reduce the map
list_c <- locator()
lnd_crop <- create.bbox(list_c)
lnd_crop <- london_Cmi[lnd_crop,]
london_lw <- nb2listw(poly2nb(lnd_crop), zero.policy=TRUE)
london_lI <- localmoran(lnd_crop@data$C_mi, london_lw, na.action = "na.pass")
london_lI[is.na(london_lI[,1]),1] <- 0
london_lI[is.na(london_lI[,4]),4] <- 0
london_lI[is.na(london_lI[,5]),5] <- 0
cmi_shades <- auto.shading(c(london_lI[,1], -london_lI[,1]), cols = brewer.pal(3, "Reds"))
choropleth(lnd_crop, london_lI[,1], shading = cmi_shades)



