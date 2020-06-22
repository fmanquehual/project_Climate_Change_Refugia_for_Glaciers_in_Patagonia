library(dismo)
library(rgdal)
library(rgeos)
library(raster)
library(rJava)
library(ggplot2)
library("PerformanceAnalytics")

wgs84 <- "+proj=longlat +ellps=WGS84" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

# Variables predictoras referencia ----
aspect <- raster("aspect_res_760.tif")
slope <- raster("slope_res_760.tif")
dem <- raster("dem_res_760.tif")

var.topo <- stack(aspect, slope, dem)

tn.s <- raster("tmin_summer_mean_1980_2010_19s.tif")
tn.w <- raster("tmin_winter_mean_1980_2010_19s.tif")
tx.s <- raster("tmax_summer_mean_1980_2010_19s.tif")
tx.w <- raster("tmax_winter_mean_1980_2010_19s.tif")
pp.s <- raster("pp_summer_mean_1980_2010_19s.tif")
pp.w <- raster("pp_winter_mean_1980_2010_19s.tif")


var.clim <- stack(tn.s, tn.w, tx.s, tx.w, pp.s, pp.w)

var.topo.ok <- resample(var.topo, var.clim, method='ngb')

aspect2 <- var.topo.ok[[1]]
summary(aspect2)
plot(aspect2)

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
reclas.aspect <- reclassify(aspect2, rclmat.1)
plot(reclas.aspect, col=bpy.colors(10))

#writeRaster(reclas.aspect, filename="aspect_res_760_reclas.tif", format="GTiff", overwrite=TRUE)


slope2 <- var.topo.ok[[2]]
summary(slope2)
plot(slope2)

# (begin, end, value ...)
v2 <- c(-0.1, 0.2, 0.5, 1, 2, 5, 10, 15, 30, 60, maxValue(slope2)+1)

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
reclas.slope <- reclassify(slope2, rclmat.2)
plot(reclas.slope)

#writeRaster(reclas.slope, filename="slope_res_760_reclas.tif", format="GTiff", overwrite=TRUE)



dem2 <- var.topo.ok[[3]]
summary(dem2)
plot(dem2)

# (begin, end, value ...)
v3 <- c(minValue(dem2)-1, 500, 1000, 1500, 2000, 2500, 3000, 3500, maxValue(dem2))

m3 = c(v3[1], v3[2], 1,
       v3[2], v3[3], 2,
       v3[3], v3[4], 3,
       v3[4], v3[5], 4,
       v3[5], v3[6], 5,
       v3[6], v3[7], 6,
       v3[7], v3[8], 7,
       v3[8], v3[9], 8)

rclmat.3 = matrix(m3, ncol=3, byrow=TRUE)
reclas.dem <- reclassify(dem2, rclmat.3)
plot(reclas.dem)

#writeRaster(reclas.dem, filename="dem_res_760_reclas.tif", format="GTiff", overwrite=TRUE)

var.topo.ok2 <- stack(reclas.aspect, reclas.slope, reclas.dem)
var.predict.pre <- stack(var.clim, var.topo.ok2)


lim <- readOGR(".", "polygon_cuenca_baker_chile_grass_19s")
marco <- readOGR(".", "polygon_marco_trabajo_19s")


var.predict.pre <- crop(var.predict.pre, marco)
var.predict <- stack(var.predict.pre)

plot(var.predict)

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_predict/")
#writeRaster(var.predict, filename="var_predict_1980_2010.tif", format="GTiff", overwrite=TRUE)









# correlacion ----
# db <- as.data.frame(var.predict)
# head(db)
# 
# clim <- db[,c(1:6)]
# head(clim)
# 
# names(clim) <- c("tmin_verano", "tmin_invierno", "tmax_verano", "tmax_invierno", "Pp_verano", "Pp_invierno")
# head(clim)
# 
# topo <- db[,c(7:9)]
# head(topo)
# 
# names(topo) <- c("orientacion", "pendiente", "altitud")
# head(topo)
# 
# setwd("c:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
# 
# #png("matriz_cor_var_predic_clim.png")
# #chart.Correlation(clim, histogram = F, pch = 19)
# dev.off()
# 
# #png("matriz_cor_var_predic_topo.png")
# #topo.plot <- chart.Correlation(topo, histogram = F, pch = 19)
# dev.off()
# 
# #-The distribution of each variable is shown on the diagonal.
# #-On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
# #-On the top of the diagonal : the value of the correlation plus the significance level as stars
# #-Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols("***", "**", "*", ".", " ")

# fin ---












# Variables predictoras futuro ----

#setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_futuro/CCSM4/2070/85/")
setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_futuro/CCSM4/2070/60/")

directory.wc <- function(modelo.i, anho.i, rcp.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"
  worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_futuro/"
  result <- paste(worldclim, modelo.i, anho.i, rcp.i, sep = "/")
  return(result)
}
# fin ---

## WORLDCLIME 2.0 ------------------------------------------------------------------------------------------------

modelo.j <- "CSIRO"
anho.j <- "2070"
rcp.j <- "85"

# Lectura de datos pp ----

setwd(directory.wc(modelo.i = modelo.j, anho.i = anho.j, rcp.i = rcp.j)) 
dir()


tn.s.futuro <- raster( paste(modelo.j, "tmin", "mean", "summer", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") ) ; paste(modelo.j, "tmin", "mean", "summer", anho.j, "rcp", rcp.j, "19s.tif", sep = "_")
tn.w.futuro <- raster( paste(modelo.j, "tmin", "mean", "winter", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") ) ; paste(modelo.j, "tmin", "mean", "winter", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") 
tx.s.futuro <- raster( paste(modelo.j, "tmax", "mean", "summer", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") ) ; paste(modelo.j, "tmax", "mean", "summer", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") 
tx.w.futuro <- raster( paste(modelo.j, "tmax", "mean", "winter", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") ) ; paste(modelo.j, "tmax", "mean", "winter", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") 
pp.s.futuro <- raster( paste(modelo.j, "pp", "mean", "summer", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") ) ; paste(modelo.j, "pp", "mean", "summer", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") 
pp.w.futuro <- raster( paste(modelo.j, "pp", "mean", "winter", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") ) ; paste(modelo.j, "pp", "mean", "winter", anho.j, "rcp", rcp.j, "19s.tif", sep = "_") 

var.clim.futuro <- stack(tn.s.futuro, tn.w.futuro, tx.s.futuro, tx.w.futuro, pp.s.futuro, pp.w.futuro)
var.clim.futuro.clip <- crop(var.clim.futuro, marco)
var.clim.futuro.clip <- stack(var.clim.futuro.clip)

var.clim.futuro.ok <- resample(var.clim.futuro.clip, var.clim, method='ngb')
var.predict.futuro <- stack(var.clim.futuro.ok, var.topo.ok2)

plot(var.predict.futuro)

names(var.predict.futuro)[1] <- names(var.predict)[1]
names(var.predict.futuro)[2] <- names(var.predict)[2]
names(var.predict.futuro)[3] <- names(var.predict)[3]
names(var.predict.futuro)[4] <- names(var.predict)[4]
names(var.predict.futuro)[5] <- names(var.predict)[5]
names(var.predict.futuro)[6] <- names(var.predict)[6]
names(var.predict.futuro)[7] <- names(var.predict)[7]
names(var.predict.futuro)[8] <- names(var.predict)[8]
names(var.predict.futuro)[9] <- names(var.predict)[9]

plot(var.predict.futuro)

# fin ---



# generador de presencia ----
#
setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

#g <- readOGR(".", "polygon_glaciares_cuenca_baker_sin_CHN_19s")
g <- readOGR(".", "polygon_glaciares_cuenca_baker_para_presencia_19s")
#g <- readOGR(".", "polygon_glaciares_cuenca_baker_19s")

# Columna de presencia ----
g@data$presencia <- 1
head(g@data)

levels(g@data$FUENTE_FEC)
levels(g@data$INVENT_FEC)

idx <- which(g@data$INVENT_FEC=="2011")
g2 <- g[idx,]
table(g2@data$NOMB_CUEN)

plot(lim)
plot(g2, add=TRUE)

# Rasterizando columna de presencia ----
g.rasterizado <- rasterize(g2, var.predict[[1]], field="presencia", fun="last", background=NA)
g.rasterizado
# 
plot(g.rasterizado)
plot(lim, add=TRUE)

#writeRaster(g.rasterizado, filename="presencia_glaciares.tif", format="GTiff", overwrite=TRUE)


# Raster a puntos de presencia ----
g.presencia <- rasterToPoints(g.rasterizado)
class(g.presencia)
head(g.presencia)

g.presencia.db <- as.data.frame(g.presencia)
g.presencia.db$id <- 1:nrow(g.presencia.db)
head(g.presencia.db)
dim(g.presencia.db)
table(g.presencia.db$layer)

g.pre.shp <- SpatialPoints(c(g.presencia.db[1],g.presencia.db[2]), proj4string = CRS(utm19))
g.shp.db <- data.frame(id= c(1:nrow(g.presencia.db)), presencia = g.presencia.db$layer, x=coordinates(g.pre.shp)[,1], y=coordinates(g.pre.shp)[,2])
g.shp <- SpatialPointsDataFrame(g.pre.shp, data = g.shp.db, match.ID = TRUE)

id.x <- which(g.shp@data$presencia==1)
g.presencia.shp <- g.shp[id.x,]
table(g.presencia.shp@data$presencia)
plot(g.presencia.shp)

#writeOGR(g.presencia.shp, ".", "points_presencia_glaciares_sin_CHN", driver="ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(g.presencia.shp, ".", "points_presencia_glaciares", driver="ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(g.presencia.shp, ".", "points_presencia_glaciares_dentro_cuenca", driver="ESRI Shapefile", overwrite_layer = TRUE)

# fin ---



# MAXENT ----
setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

#g.presencia.shp <- readOGR(".", "points_presencia_glaciares_sin_CHN")
g.presencia.shp <- readOGR(".", "points_presencia_glaciares")
#g.presencia.shp <- readOGR(".", "points_presencia_glaciares_dentro_cuenca")

head(g.presencia.shp@data)
dim(g.presencia.shp)

presvals <- extract(var.predict, g.presencia.shp)
colnames(presvals)

set.seed(0)
backgr <- randomPoints(var.predict, (10000),p = g.presencia.shp, excludep = T) # 10000 puntos random del fondo, excluyendo puntos de presencia


group <- kfold(presvals, 5)
pres_train <- g.presencia.shp[group != 1, ]
pres_test <- g.presencia.shp[group == 1, ]


group <- kfold(backgr, 5)
backgr_train <- backgr[group != 1, ]
backgr_test <- backgr[group == 1, ]

dev.off()
plot(var.predict[[1]])
plot(lim, add=TRUE, lwd=2)
points(pres_train, pch= '+', col='red', cex=0.5)
points(pres_test, pch='+', col='blue', cex=0.5)
points(backgr_train, pch= '.', col='black', cex=4)
points(backgr_test, pch='.', col='white', cex=4)






# plot(var.predict)
# 
# var.predict2 <- var.predict[[c(-5,-8)]]
# plot(var.predict2)
# 
# var.predict.2050.85_2 <- var.predict.2050.85[[c(-5,-8)]]
# plot(var.predict.2050.85_2)




m1 <- maxent(var.predict, pres_train, factors=c('aspect_res_760', 'slope_res_760', 'dem_res_760'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'defaultprevalence=0.1', #'outputformat=raw',
                    'linear=TRUE',
                    'quadratic=TRUE',
                    'product=TRUE',
                    'threshold=TRUE',
                    'hinge=TRUE'))
m1
plot(m1)
response(m1)

# m2 <- maxent(var.predict, pres_train, args=c('betamultiplier=1'))
# response(m2)
# 
m3 <- maxent(var.predict, pres_train, factors=c('aspect_res_760', 'slope_res_760'), args=c('betamultiplier=1.5'))
plot(m3)
response(m3)

m4 <- maxent(var.predict, pres_train, factors=c('aspect_res_760', 'slope_res_760'), args=c('betamultiplier=10'))
plot(m4)
response(m4)

m5 <- maxent(var.predict2, pres_train, factors=c('aspect_res_760'), args=c('jackknife=TRUE', 'responsecurves=TRUE'))
m5
plot(m5)
response(m5)



# ej0 <- predict(x = var.predict, object = m1, args=c("outputformat=raw"))
# plot(ej0)

ej <- predict(x = var.predict, object = m1) # logistic output
plot(ej)
# Compute niche overlap from predictions of species distributions with the 'I' or 'D' similarity 
# statistic of Warren et al. (2009). The statistic ranges from 0 (no overlap) to 1 (the distributions are identical).
nicheOverlap(g.rasterizado, ej, stat='I', mask=TRUE, checkNegatives=TRUE)


e_test <- evaluate(pres_test, backgr_test, m1, var.predict)
e_test

tr_test <- threshold(e_test, 'spec_sens')
plot(ej > tr_test, main='presence/background')

# e_train <- evaluate(pres_train, backgr_train, m1, var.predict)
# e_train
# 
# tr_train <- threshold(e_train, 'spec_sens')
# plot(ej > tr_train, main='presence/background')


dev.off()

#setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")

#png("prediccion_2011_glaciares_fuera_cuenca.png", width = 1000, height = 750, units = "px")
#png("prediccion_2011_glaciares_fuera_cuenca_threshold.png", width = 1000, height = 750, units = "px")
#png("prediccion_2011_glaciares_dentro_cuenca.png", width = 1000, height = 750, units = "px")
#png("prediccion_2011_glaciares_dentro_cuenca_threshold.png", width = 1000, height = 750, units = "px")

par(mfrow=c(2,2))
plot(ej, main="Predicción MAXENT 2011")
plot(lim, add=TRUE)

plot(ej > tr_test, main='presence/background threshold test')
plot(lim, add=TRUE)

# plot(ej > tr_train, main='presence/background threshold train')
# plot(lim, add=TRUE)

plot(lim, axes=TRUE, main="Glaciares Inventariados 2011 (DGA)")
plot(g.rasterizado, add=TRUE, col="green", border="green")
plot(lim, add=TRUE)

#dev.off()

# setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
# writeRaster(ej, filename="ej_maxent_output", format="GTiff", overwrite=TRUE)
# fin ---



# prediccion futuro ----
dev.off()

#setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")

#png("prediccion_ccsm4_2070_rcp85.png", width = 1000, height = 750, units = "px")

ej2 <- predict(x = var.predict.futuro, object = m1)
plot(ej2)
plot(lim, add=TRUE)

#dev.off()

ej3 <- predict(x = var.predict.2050.85, object = m3)
plot(ej3)
plot(lim, add=TRUE)

ej4 <- predict(x = var.predict.2050.85, object = m4)
plot(ej4)
plot(lim, add=TRUE)

ej5 <- predict(x = var.predict.2050.85_2, object = m5)
plot(ej5)
plot(lim, add=TRUE)
