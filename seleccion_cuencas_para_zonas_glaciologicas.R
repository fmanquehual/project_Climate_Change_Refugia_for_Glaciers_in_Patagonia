library('rgdal')
library('rgeos')
library('raster')

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')

p1 <- readOGR('.', 'polygon_subcuencas_chile_geo')
marco <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')

plot(marco, border='red')
plot(p1, add=TRUE)

p2 <- crop(p1, marco)
plot(p2)

head(p2@data)
p2@data$COD_CUEN <- as.character(p2@data$COD_CUEN)
p2@data$COD_CUEN <- as.numeric(p2@data$COD_CUEN)
unique(p2@data$COD_CUEN)


zona_sur <- c(103:113)
zona_austral <- c(114:118)

idx <- which(p2@data$COD_CUEN%in%zona_sur)
idy <- which(p2@data$COD_CUEN%in%zona_austral)

zona_sur_2 <- p2[idx,]
zona_austral_2 <- p2[idy,]

zona_sur_3 <- crop(marco, zona_sur_2)
zona_austral_3 <- crop(marco, zona_austral_2)

plot(p2, axes=TRUE)
plot(zona_sur_3, add=TRUE, border='red', lwd=2)
plot(zona_austral_3, add=TRUE, border='red', lwd=2)

setwd("C:/Users/Usuario/Documents/Francisco/coberturas/")
# writeOGR(zona_sur_3, ".", "polygon_zona_glaciologica_sur_geo", driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(zona_austral_3, ".", "polygon_zona_glaciologica_austral_geo", driver="ESRI Shapefile", overwrite_layer = TRUE)
