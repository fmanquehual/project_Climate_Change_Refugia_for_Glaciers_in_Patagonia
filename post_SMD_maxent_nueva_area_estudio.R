library('rgdal')
library('raster')

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/predicciones_maxent/')

# predicciones ----
r01 <- raster('prediccion_periodo_referencia_sur.tif')
r02 <- raster('prediccion_periodo_referencia_austral.tif')

r1 <- raster('prediccion_csiro_2050_rcp45_zona_sur.tif')
r2 <- raster('prediccion_csiro_2050_rcp45_zona_austral.tif')

r3 <- raster('prediccion_csiro_2050_rcp85_zona_sur.tif')
r4 <- raster('prediccion_csiro_2050_rcp85_zona_austral.tif')

r5 <- raster('prediccion_csiro_2070_rcp45_zona_sur.tif')
r6 <- raster('prediccion_csiro_2070_rcp45_zona_austral.tif')

r7 <- raster('prediccion_csiro_2070_rcp85_zona_sur.tif')
r8 <- raster('prediccion_csiro_2070_rcp85_zona_austral.tif')


sur <- stack(r01, r1, r3, r5, r7)
austral <- stack(r02, r2, r4, r6, r8)

plot(sur)

stack.i <- sur
# (begin, end, value ...)
v1 <- c(0, 0.462, 1)

m1 = c(v1[1], v1[2], NA,
       v1[2], v1[3], 1)

rclmat = matrix(m1, ncol=3, byrow=TRUE)
reclas <- reclassify(stack.i, rclmat)
plot(reclas, col=rev(heat.colors(2)))


stack.j <- austral
# (begin, end, value ...)
v2 <- c(0, 0.421, 1)

m2 = c(v2[1], v2[2], NA,
       v2[2], v2[3], 1)

rclmat2 = matrix(m2, ncol=3, byrow=TRUE)
reclas2 <- reclassify(stack.j, rclmat2)
plot(reclas2, col=rev(heat.colors(2)))

u <- merge(reclas, reclas2)
names(u) <- c('prediccion_referencia', 'prediccion_csiro_2050_rcp45', 'prediccion_csiro_2050_rcp85',
              'prediccion_csiro_2070_rcp45', 'prediccion_csiro_2070_rcp85')
plot(u)

setwd("C:/Users/Usuario/Documents/Francisco/predicciones_maxent/")

# writeRaster(u[['prediccion_referencia']], filename='prediccion_referencia.tif', format="GTiff", overwrite=TRUE)
# writeRaster(u[['prediccion_csiro_2050_rcp45']], filename='prediccion_csiro_2050_rcp45.tif', format="GTiff", overwrite=TRUE)
# writeRaster(u[['prediccion_csiro_2050_rcp85']], filename='prediccion_csiro_2050_rcp85.tif', format="GTiff", overwrite=TRUE)
# writeRaster(u[['prediccion_csiro_2070_rcp45']], filename='prediccion_csiro_2070_rcp45.tif', format="GTiff", overwrite=TRUE)
# writeRaster(u[['prediccion_csiro_2070_rcp85']], filename='prediccion_csiro_2070_rcp85.tif', format="GTiff", overwrite=TRUE)


# FIN ---



# LECTURAS DE RASTER ----
setwd("C:/Users/Usuario/Documents/Francisco/predicciones_maxent/")

ref <- raster('prediccion_referencia.tif')
pre_2050_rcp45 <- raster('prediccion_csiro_2050_rcp45.tif')
pre_2050_rcp85 <- raster('prediccion_csiro_2050_rcp85.tif')
pre_2070_rcp45 <- raster('prediccion_csiro_2070_rcp45.tif')
pre_2070_rcp85 <- raster('prediccion_csiro_2070_rcp85.tif')

# FIN ---


stack.k <- stack(ref, pre_2050_rcp45, pre_2050_rcp85, pre_2070_rcp45, pre_2070_rcp85)
plot(stack.k)

# OVERLAP ----

rcc_2050_rcp45 <- overlay(pre_2050_rcp45, ref, fun=function(x,y){return(x*y)})
plot(rcc_2050_rcp45)

rcc_2050_rcp85 <- overlay(pre_2050_rcp85, ref, fun=function(x,y){return(x*y)})
plot(rcc_2050_rcp85)

rcc_2070_rcp45 <- overlay(pre_2070_rcp45, ref, fun=function(x,y){return(x*y)})
plot(rcc_2070_rcp45)

rcc_2070_rcp85 <- overlay(pre_2070_rcp85, ref, fun=function(x,y){return(x*y)})
plot(rcc_2070_rcp85)

# fin ---




# raster to polygon ----

poly_ref <- rasterToPolygons(ref, dissolve = TRUE)
poly_rcc_2050_rcp45 <- rasterToPolygons(rcc_2050_rcp45, dissolve = TRUE)
poly_rcc_2050_rcp85 <- rasterToPolygons(rcc_2050_rcp85, dissolve = TRUE)
poly_rcc_2070_rcp45 <- rasterToPolygons(rcc_2070_rcp45, dissolve = TRUE)
poly_rcc_2070_rcp85 <- rasterToPolygons(rcc_2070_rcp85, dissolve = TRUE)

plot(poly_ref, axes=TRUE)
plot(poly_rcc_2050_rcp45, axes=TRUE)
plot(poly_rcc_2050_rcp85, axes=TRUE)
plot(poly_rcc_2070_rcp45, axes=TRUE)
plot(poly_rcc_2070_rcp85, axes=TRUE)

setwd("C:/Users/Usuario/Documents/Francisco/predicciones_maxent/refugiosCC/")

# writeOGR(poly_ref, ".", 'polygon_referencia', driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(poly_rcc_2050_rcp45, ".", 'polygon_refugioCC_csiro_2050_rcp45', driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(poly_rcc_2050_rcp85, ".", 'polygon_refugioCC_csiro_2050_rcp85', driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(poly_rcc_2070_rcp45, ".", 'polygon_refugioCC_csiro_2070_rcp45', driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(poly_rcc_2070_rcp85, ".", 'polygon_refugioCC_csiro_2070_rcp85', driver="ESRI Shapefile", overwrite_layer = TRUE)
