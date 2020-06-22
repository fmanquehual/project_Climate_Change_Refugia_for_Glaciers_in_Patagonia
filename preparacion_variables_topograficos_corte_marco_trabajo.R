library('raster')
library('rgdal')
library('rgeos')

rm(list=ls())
dev.off()

# # Parte 1 ----
# 
# # lectura coberturas ----
# 
# setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
# 
# marco <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')
# 
# setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/topografico/')
# 
# dem <- stack('dem_res_30m_nueva_area_estudio_geo.tif')
# 
# setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/2050_2070_CSIRO/')
# r.obj <- raster('CSIRO_pp_winter_mean_rcp45_2050.TIF')
# 
# # fin ---
# 
# 
# 
# 
# 
# # clip y tamanho pixel ----
# 
# dem.clip.pre <- crop(dem, marco)
# dem.clip.pre <- stack(dem.clip.pre)
# r.obj.clip <- crop(r.obj, marco)
# 
# dem.clip <- resample(dem.clip.pre, r.obj.clip, method='bilinear')
# dem.clip.mask <- mask(dem.clip, r.obj.clip)
# plot(dem.clip.mask)
# 
# setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/topografico/clip/")
# # writeRaster(dem.clip.mask, filename="clip_dem_res_30m_nueva_area_estudio_geo.tif", format="GTiff", overwrite=TRUE)
# 
# # AHORA CALCULAS PENDIENTE (%) Y ORIENTACION EN QGIS
# 
# # Referencia: resample o aggregate?
# # https://gis.stackexchange.com/questions/255150/using-resample-vs-aggregate-extend-in-r-to-have-rasters-of-matching-resolutio
# 
# # fin ---










# Parte 2 ----

# reclasificacion ----

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/topografico_preliminar/')

topo <- stack(dir())
plot(topo)

# dem 

dem <- topo[[1]]


# orientacion

v1 <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)

m1 = c(v1[1], v1[2], as.integer(1),
       v1[2], v1[3], as.integer(2),
       v1[3], v1[4], as.integer(3),
       v1[4], v1[5], as.integer(4),
       v1[5], v1[6], as.integer(5),
       v1[6], v1[7], as.integer(6),
       v1[7], v1[8], as.integer(7),
       v1[8], v1[9], as.integer(8),
       v1[9], v1[10], as.integer(1))

rclmat.1 = matrix(m1, ncol=3, byrow=TRUE)
exposicion <- reclassify(topo[[2]], rclmat.1, include.lowest=TRUE)
plot(exposicion, col=bpy.colors(10))

unique(exposicion[])

# pendiente

pendiente <- topo[[3]]

# fin ---


setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/topografico/")

writeRaster(dem, filename="clip_dem_res_30m_nueva_area_estudio_geo.tif", format="GTiff", overwrite=TRUE)
writeRaster(exposicion, filename="clip_orientacion_res_30m_nueva_area_estudio_geo.tif", format="GTiff", overwrite=TRUE)
writeRaster(pendiente, filename="clip_pendiente_res_30m_nueva_area_estudio_geo.tif", format="GTiff", overwrite=TRUE)
