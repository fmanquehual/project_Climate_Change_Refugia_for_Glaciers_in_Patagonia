library(raster)
library(rgdal)

setwd("C:/Users/Francisco/Downloads/")

wgs84 <- "+proj=longlat +ellps=WGS84" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84



#r <- raster("IDAHO_EPSCOR_TERRACLIMATE_temp_max.tif")
img <- stack("LANDSAT_LE07_C01_T1_RT_TOA_marzo_2001.tif")


setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

lim <- readOGR(".", "polygon_cuenca_baker_chile_grass_19s")  
# dem <- raster("dem_baker_grande_19s.tif")
# dem

# plot(r)
# plot(lim, add=T)

plot(img, 1)
plot(lim, add=T)
plotRGB(img, 3,2,1, stretch="lin")
