library('rgdal')
library('rgeos')
library('raster')



# funciones ----

directory.wc <- function(modelo.i, anho.i, rcp.i){ # permite generar nuevos directorios de trabajo desde una carpeta de trabajo
  worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_futuro/"
  result <- paste(worldclim, modelo.i, anho.i, rcp.i, sep = "/")
  return(result)
}

eval.stack <- function(stack.i){
  stack.j <- stack.i
  
  r <- gsub('stack', '_', names(stack.j))
  f <- gsub('_', ' ', r)
  name.pre.j <- substr(f, 3, 7)
  name.j <- gsub(' ', '', name.pre.j)
  names(stack.j) <- name.j
  
  mean.i <- cellStats(stack.j, stat = 'mean', na.rm=TRUE)  
  sd.i <- cellStats(stack.j, stat = 'sd', na.rm=TRUE)  
  min.i <- cellStats(stack.j, stat = 'min', na.rm=TRUE)
  max.i <- cellStats(stack.j, stat = 'max', na.rm=TRUE)
  
  label.i <- paste('SD = ', round(sd.i, 2), sep = '') ; label.i
  
  boxplot(stack.j, ylab = label.y, xlab = 'GCMs', main = title.j)
  points(mean.i, col='red', pch=16)
  text(x=1, y=(max(max.i)+0.5), labels=label.i[1], cex=0.8)
  text(x=2, y=(max(max.i)+0.5), labels=label.i[2], cex=0.8)
  text(x=3, y=(max(max.i)+0.5), labels=label.i[3], cex=0.8)
  text(x=4, y=(max(max.i)+0.5), labels=label.i[4], cex=0.8)
  
  
  print( paste(name.j, 'min =', round(min.i, 2), sep = ' ') )
  print( paste(name.j, 'max =', round(max.i, 2), sep = ' ') )
  print( paste(name.j, 'mean =', round(mean.i, 2), sep = ' ') )
  print( paste(name.j, 'sd =', round(sd.i, 2), sep = ' ') )
}

# fin ---

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")


# limite cuenca y marco trabajo ----

lim <- readOGR(".", "polygon_cuenca_baker_chile_grass_19s")
marco <- readOGR(".", "polygon_marco_trabajo_19s")
g.rasterizado <- raster("presencia_glaciares.tif")

# fin ---






setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_predict/")

# VP referencia ----
var.predict <- stack("var_predict_1980_2010.tif")
plot(var.predict)

# fin ---







# VP futuro ----

# listos: CSIRO, CCSM4, MIROC, 
#IPSL

# modelo.j: puede ser "IPSL", "MIROC","CCSM4" o "CSIRO" (sensibilidad: alta extrema, alta moderada, baja moderada y baja extrema, respectivamente)
# anho.j: puede ser "2050" o "2070"
# rcp.j: puede ser "60" o "85"

modelo.j <- "MIROC"
anho.j <- "2070"
rcp.j <- "85"

name <- paste('stack', '_', modelo.j, '_', anho.j, '_', rcp.j, '.tif', sep = '') ; name
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

var.clim.futuro.ok <- resample(var.clim.futuro.clip, var.predict, method='ngb')
var.predict.futuro <- stack(var.clim.futuro.ok, var.predict[[7]], var.predict[[8]], var.predict[[9]])

#plot(var.predict.futuro)

names(var.predict.futuro)[1] <- names(var.predict)[1]
names(var.predict.futuro)[2] <- names(var.predict)[2]
names(var.predict.futuro)[3] <- names(var.predict)[3]
names(var.predict.futuro)[4] <- names(var.predict)[4]
names(var.predict.futuro)[5] <- names(var.predict)[5]
names(var.predict.futuro)[6] <- names(var.predict)[6]
names(var.predict.futuro)[7] <- names(var.predict)[7]
names(var.predict.futuro)[8] <- names(var.predict)[8]
names(var.predict.futuro)[9] <- names(var.predict)[9]

#plot(var.predict.futuro)

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_futuro/')

writeRaster(var.predict.futuro, filename=name, format="GTiff", overwrite=TRUE)
# fin ---




# revision ----

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_futuro/')

# 2050 rcp60 ----
ipsl50_60 <- stack('stack_IPSL_2050_60.tif')
miroc50_60 <- stack('stack_MIROC_2050_60.tif')
ccsm450_60 <- stack('stack_CCSM4_2050_60.tif')
csiro50_60 <- stack('stack_CSIRO_2050_60.tif')
# fin ---

# 2050 rcp85 ----
ipsl50_85 <- stack('stack_IPSL_2050_85.tif')
miroc50_85 <- stack('stack_MIROC_2050_85.tif')
ccsm450_85 <- stack('stack_CCSM4_2050_85.tif')
csiro50_85 <- stack('stack_CSIRO_2050_85.tif')
# fin ---

# 2050 rcp60 ----
ipsl70_60 <- stack('stack_IPSL_2070_60.tif')
miroc70_60 <- stack('stack_MIROC_2070_60.tif')
ccsm470_60 <- stack('stack_CCSM4_2070_60.tif')
csiro70_60 <- stack('stack_CSIRO_2070_60.tif')
# fin ---

# 2050 rcp60 ----
ipsl70_85 <- stack('stack_IPSL_2070_85.tif')
miroc70_85 <- stack('stack_MIROC_2070_85.tif')
ccsm470_85 <- stack('stack_CCSM4_2070_85.tif')
csiro70_85 <- stack('stack_CSIRO_2070_85.tif')
# fin ---

# listos: 50_60, 50_85, 70_60, 70_85
ipsl <-  ipsl70_85
miroc <- miroc70_85
ccsm4 <- ccsm470_85
csiro <- csiro70_85






anho.i <- '2070'
rcp.i <- '8.5'


# referencia ---
title.j <- 'Referencia' ; title.j

label.y <- 'Temperatura (C°)'
var.predict2 <- stack(var.predict[[1]], var.predict[[2]], var.predict[[3]], var.predict[[4]])
eval.stack(var.predict2)


 # summer ---
tmin_summer <- stack(ipsl[[1]], miroc[[1]], ccsm4[[1]], csiro[[1]])
tmax_summer <- stack(ipsl[[3]], miroc[[3]], ccsm4[[3]], csiro[[3]])
pp_summer <- stack(ipsl[[5]], miroc[[5]], ccsm4[[5]], csiro[[5]])

  # winter ---
tmin_winter <- stack(ipsl[[2]], miroc[[2]], ccsm4[[2]], csiro[[2]])
tmax_winter <- stack(ipsl[[4]], miroc[[4]], ccsm4[[4]], csiro[[4]])
pp_winter <- stack(ipsl[[6]], miroc[[6]], ccsm4[[6]], csiro[[6]])



# summer ---
title.j <- paste('Verano Año', anho.i, 'RCP', rcp.i, sep = ' ') ; title.j

label.y <- 'Temperatura minima (C°)'
eval.stack(tmin_summer)

label.y <- 'Temperatura maxima (C°)'
eval.stack(tmax_summer)

label.y <- 'Precipitacion (mm)'
eval.stack(pp_summer)

# winter ---
title.j <- paste('Invierno Año', anho.i, 'RCP', rcp.i, sep = ' ') ; title.j

label.y <- 'Temperatura minima (C°)'
eval.stack(tmin_winter)

label.y <- 'Temperatura maxima (C°)'
eval.stack(tmax_winter)

label.y <- 'Precipitacion (mm)'
eval.stack(pp_winter)
