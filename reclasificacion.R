library(raster)


wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

# orientacion ----

aspect <- raster("aspect_res_760.tif")
summary(aspect)
plot(aspect)

# (begin, end, value ...)
v1 <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)

m1 = c(NA, NA, 0,
       v1[1], v1[2], 1,
       v1[2], v1[3], 2,
       v1[3], v1[4], 3,
       v1[4], v1[5], 4,
       v1[5], v1[6], 5,
       v1[6], v1[7], 6,
       v1[7], v1[8], 7,
       v1[8], v1[9], 8,
       v1[9], v1[10], 1)

rclmat.1 = matrix(m1, ncol=3, byrow=TRUE)
reclas.aspect <- reclassify(aspect, rclmat.1)
plot(reclas.aspect, col=bpy.colors(10))

#writeRaster(reclas.aspect, filename="aspect_res_760_reclas.tif", format="GTiff", overwrite=TRUE)
# fin




# pendiente ----

slope <- raster("slope_res_760.tif")
summary(slope)
setMinMax(slope)
plot(slope)

# (begin, end, value ...)
v2 <- c(-0.1, 0.2, 0.5, 1, 2, 5, 10, 15, 30, 60, maxValue(slope)+1)

m2 = c(v2[1], v2[2], 1,
       v2[2], v2[3], 2,
       v2[3], v2[4], 3,
       v2[4], v2[5], 4,
       v2[5], v2[6], 5,
       v2[6], v2[7], 6,
       v2[7], v2[8], 7,
       v2[8], v2[9], 8,
       v2[9], v2[10], 9,
       v2[10], v2[11], 10)

rclmat.2 = matrix(m2, ncol=3, byrow=TRUE)
reclas.slope <- reclassify(slope, rclmat.2)
plot(reclas.slope)

#writeRaster(reclas.slope, filename="slope_res_760_reclas.tif", format="GTiff", overwrite=TRUE)
# fin