library('rgdal')
library('rgeos')
library('raster')
library('rasterVis')
library('ggplot2')

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")


# limite cuenca y marco trabajo ----

lim <- readOGR(".", "polygon_cuenca_baker_chile_grass_19s")
marco <- readOGR(".", "polygon_marco_trabajo_19s")
g <- readOGR(".", "polygon_glaciares_cuenca_baker_19s")

# FIN ---






# predicciones ----
setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/resultados_prediccion/')

csiro <- stack('prediccion_csiro_6var.tif')
names(csiro) <- c('referencia', 'prediccion_2050_rcp60', 'prediccion_2050_rcp85', 'prediccion_2070_rcp60', 'prediccion_2070_rcp85')
plot(csiro)

stack.i <- csiro
# (begin, end, value ...)
v1 <- c(0, 0.5, 1)

m1 = c(v1[1], v1[2], NA,
       v1[2], v1[3], 1)

rclmat.1 = matrix(m1, ncol=3, byrow=TRUE)
reclas.csiro <- reclassify(stack.i, rclmat.1)
plot(reclas.csiro, col=rev(heat.colors(2)))


setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/")

writeRaster(reclas.csiro[['referencia']], filename='prediccion_csiro_referencia.tif', format="GTiff", overwrite=TRUE)
writeRaster(reclas.csiro[['prediccion_2050_rcp60']], filename='prediccion_csiro_2050_rcp60.tif', format="GTiff", overwrite=TRUE)
writeRaster(reclas.csiro[['prediccion_2050_rcp85']], filename='prediccion_csiro_2050_rcp85.tif', format="GTiff", overwrite=TRUE)
writeRaster(reclas.csiro[['prediccion_2070_rcp60']], filename='prediccion_csiro_2070_rcp60.tif', format="GTiff", overwrite=TRUE)
writeRaster(reclas.csiro[['prediccion_2070_rcp85']], filename='prediccion_csiro_2070_rcp85.tif', format="GTiff", overwrite=TRUE)

# FIN ---


# clip ----
csiro.clip <- mask(reclas.csiro, lim)
plot(csiro.clip, col='red')

# fin ---




# LECTURAS DE RASTER ----
#setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/resultados_prediccion/")

# ref <- raster('prediccion_csiro_referencia_clip.tif')
# pre_2050_rcp60 <- raster('prediccion_csiro_2050_rcp60_clip.tif')
# pre_2050_rcp85 <- raster('prediccion_csiro_2050_rcp85_clip.tif')
# pre_2070_rcp60 <- raster('prediccion_csiro_2070_rcp60_clip.tif')
# pre_2070_rcp85 <- raster('prediccion_csiro_2070_rcp85_clip.tif')
# FIN ---

ref <- csiro.clip[['referencia']]
pre_2050_rcp60 <- csiro.clip[['prediccion_2050_rcp60']]
pre_2050_rcp85 <- csiro.clip[['prediccion_2050_rcp85']]
pre_2070_rcp60 <- csiro.clip[['prediccion_2070_rcp60']]
pre_2070_rcp85 <- csiro.clip[['prediccion_2070_rcp85']]

# RASTERIZACION DE GLACIARES ----
g@data$presencia <- 1
head(g@data)

levels(g@data$FUENTE_FEC)
levels(g@data$INVENT_FEC)

# Rasterizando columna de presencia ----
g.rasterizado <- rasterize(g, ref, field="presencia", fun="last", background=NA)
g.rasterizado

plot(lim)
plot(g.rasterizado, add=TRUE)
# FIN ---


stack.k <- stack(g.rasterizado, ref, pre_2050_rcp60, pre_2050_rcp85, pre_2070_rcp60, pre_2070_rcp85)
plot(stack.k)




# OVERLAP ----

rcc_2050_rcp60 <- overlay(pre_2050_rcp60, g.rasterizado, fun=function(x,y){return(x*y)})
plot(rcc_2050_rcp60)
plot(lim, add=TRUE)

rcc_2050_rcp85 <- overlay(pre_2050_rcp85, g.rasterizado, fun=function(x,y){return(x*y)})
plot(rcc_2050_rcp85)
plot(lim, add=TRUE)

rcc_2070_rcp60 <- overlay(pre_2070_rcp60, g.rasterizado, fun=function(x,y){return(x*y)})
plot(rcc_2070_rcp60)
plot(lim, add=TRUE)

rcc_2070_rcp85 <- overlay(pre_2070_rcp85, g.rasterizado, fun=function(x,y){return(x*y)})
plot(rcc_2070_rcp85)
plot(lim, add=TRUE)

# fin ---




# raster to polygon ----

poly_rcc_2050_rcp60 <- rasterToPolygons(rcc_2050_rcp60, dissolve = TRUE)
poly_rcc_2050_rcp85 <- rasterToPolygons(rcc_2050_rcp85, dissolve = TRUE)
poly_rcc_2070_rcp60 <- rasterToPolygons(rcc_2070_rcp60, dissolve = TRUE)
poly_rcc_2070_rcp85 <- rasterToPolygons(rcc_2070_rcp85, dissolve = TRUE)

plot(poly_rcc_2050_rcp60, axes=TRUE)
plot(poly_rcc_2050_rcp85, axes=TRUE)
plot(poly_rcc_2070_rcp60, axes=TRUE)
plot(poly_rcc_2070_rcp85, axes=TRUE)

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/resultados_prediccion/")
writeOGR(poly_rcc_2050_rcp60, ".", 'polygon_refugioCC_csiro_2050_rcp60', driver="ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(poly_rcc_2050_rcp85, ".", 'polygon_refugioCC_csiro_2050_rcp85', driver="ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(poly_rcc_2070_rcp60, ".", 'polygon_refugioCC_csiro_2070_rcp60', driver="ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(poly_rcc_2070_rcp85, ".", 'polygon_refugioCC_csiro_2070_rcp85', driver="ESRI Shapefile", overwrite_layer = TRUE)
