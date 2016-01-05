library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)

if(!exists("SPDF.Cmi_Index", mode="function")) source("./FUN_SPDF_Cmi_Index.R")
if(!exists("plot_spdf_byClassInt", mode="function")) source("./FUN_Plot_map_byClassInt.R")

london <- readOGR("../shapes/london/clean", "london")

london_Cmi <- SPDF.Cmi_Index(london)
london_Cmi <- london_Cmi[london_Cmi@data$C_mi > 0 & london_Cmi@data$C_mi < 1,]
summary(london_Cmi@data$C_mi)
areas <- extract.areas(london_Cmi)
london_Cmi@data <- cbind(london_Cmi@data, areas)

hist(london_Cmi@data$C_mi, breaks = 100, col = "azure4", border = "white")
hist(log(london_Cmi@data$areas), breaks = 100, col = "gray32", border = "white")

plot_spdf_byClassInt(london_Cmi, plot_variable = "C_mi", n = 8, col_pal = "YlGnBu", style_intervals = "quantile")
plot_spdf_byClassInt(london_Cmi, plot_variable = "C_mi", n = 3, col_pal = "YlGnBu", style_intervals = "quantile")
plot_spdf_byClassInt(london_Cmi, plot_variable = "areas", n = 5, col_pal = "Greys", style_intervals = "quantile", border_col = "white")
