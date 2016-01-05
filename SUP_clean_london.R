####CLEAN THE LONDON FILE#####
source("FUN_thames.R")
london <- readOGR("../shapes/london/clean", "london_clean_40")
thames_coords <- read.csv2("./coords_thames.csv")
thames_coords <- list(x = thames_coords[,2], y = thames_coords[,3])
crop_coords <- locator()
write.csv2(crop_coords, "./coords_thames.csv")
crop <- box(crop_coords)
lnd_clsup <- london[crop,]
plot(lnd_clsup, col = "gray")
thames_coords <- locator()
thames <- thames.line(thames_coords)
plot(thames, col = "red", add = TRUE)
thames <- lnd_clsup[thames,]
plot(thames, col = "yellow", add = TRUE)

IDs_2B_RMVED <- return.IDs(thames)
london_clean <- remove.polygons(london, IDs_2B_RMVED)
plot(london_clean, col = "red")
writeOGR(london_clean, dsn = "../shapes/london/clean/", layer = "london", driver = "ESRI Shapefile")
