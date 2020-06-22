library('dismo')
library('rgdal')
library('rgeos')
library('raster')
library('rJava')

rm(list=ls())
dev.off()

# funciones ----

# f1
var.referencia <- function(){
  
  # variables de pendiente referencia ---
  
  folder.i <- 'historico'
  
  directory.pre.i <- 'C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok'
  directory.i <- paste(directory.pre.i, folder.i, sep = '/')
  
  setwd(directory.i)
  
  stack.clim.r <- stack( dir() )
  
  # variables de pendiente referencia ---
  
  folder.i <- 'sen_slope_referencia'
  
  directory.i <- paste(directory.pre.i, folder.i, sep = '/')
  setwd(directory.i)
  
  stack.sen.r <- stack( dir() )
  
  # variables topograficas ---
  folder.i <- 'topografico'
  
  directory.i <- paste(directory.pre.i, folder.i, sep = '/')
  setwd(directory.i)
  
  stack.topo <- stack( dir() )
  
  # output ---
  
  var.amb <- stack(stack.clim.r, stack.sen.r, stack.topo)
  return(var.amb)
}

# f2
var.futuro <- function(modelo, rcp, anho){
  
  modelo.i <- modelo
  rcp.i <- rcp
  anho.i <- anho
  
  # variables climaticas ---
  
  if(modelo=='CSIRO'){folder.i <- '2050_2070_CSIRO'} else(folder.i <- '2050_2070')
  directory.pre.i <- 'C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok'
  directory.i <- paste(directory.pre.i, folder.i, sep = '/')
  
  setwd(directory.i)
  
  if(anho.i==2050){anho.j <- '2011_2060'} else(anho.j <- '2011_2080')
  
  k1 <- grep(pattern = modelo.i, dir(), value = T)
  k2 <- grep(pattern = rcp.i, k1, value = T)
  k3 <- grep(pattern = anho.i, k2, value = T)
  
  stack.clim.f <- stack( k3 )
  
  # variables de pendiente futuro ---
  
  folder.i <- 'sen_slope_futuro'
  
  directory.i <- paste(directory.pre.i, folder.i, sep = '/')
  setwd(directory.i)
  
  k1 <- grep(pattern = modelo.i, dir(), value = T)
  k2 <- grep(pattern = rcp.i, k1, value = T)
  k3 <- grep(pattern = anho.j, k2, value = T)
  
  stack.sen.f <- stack(k3)
  
  # variables topograficas ---
  
  folder.i <- 'topografico'
  
  directory.i <- paste(directory.pre.i, folder.i, sep = '/')
  setwd(directory.i)
  
  stack.topo <- stack( dir() )
  
  # output ---
  
  var.amb <- stack(stack.clim.f, stack.sen.f, stack.topo)
  return(var.amb)
}

# f3
trans <- function(stack.i, var.name){
  v.i <- stack.i[[var.name]]
  v.i_trans <- exp(v.i)
  names(v.i_trans) <- names(stack.i[[var.name]])
  return(v.i_trans)
  }

# f4
trans.var.model <- function(modelo.i, marco.i){
  model.pre.j <- stack(modelo.i[['pp_winter']], trans(modelo.i, 'tmax_summer'), modelo.i[['tmin_winter']],
                       modelo.i[['slope_pp_winter']], modelo.i[['slope_tmax_summer']], modelo.i[['slope_tmin_winter']],
                       modelo.i[['altitud']], modelo.i[['orientacion']], modelo.i[['pendiente']])
  model.pre.j <- crop(model.pre.j, marco.i)
  model.j <- mask(model.pre.j, marco.i)
  return(model.j)
}

# fin ---






# lectura coberturas ----

# ---
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
marco_zona_sur_chile_argentina <- readOGR('.', 'polygon_zona_glaciologica_sur_chile_argentina_geo')
marco_zona_austral_chile_argentina <- readOGR('.', 'polygon_zona_glaciologica_austral_chile_argentina_geo')
marco_zona_sur <- readOGR('.', 'polygon_zona_glaciologica_sur_geo')
marco_zona_austral <- readOGR('.', 'polygon_zona_glaciologica_austral_geo')

# ---
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/var_presencia/')
g.sur <- readOGR('.', 'points_presencia_glaciares_zona_glaciologica_sur')
g.austral <- readOGR('.', 'points_presencia_glaciares_zona_glaciologica_austral')

# ---
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/Glaciares_Nacional/')
g.poly <- readOGR('.', 'polygon_glaciares_marco_trabajo_nuevo_geo')

# ---
new_names <- c('pp_winter', 'tmax_summer', 'tmin_winter',
                'slope_pp_winter', 'slope_tmax_summer', 'slope_tmin_winter', 
                'altitud', 'orientacion', 'pendiente')

new_names_jackknife <- c('Pp winter', 'Tmax summer', 'Tmin winter',
               'Slope pp winter', 'Slope tmax summer', 'Slope tmin winter', 
               'Elevation', 'Aspect', 'Slope')

# ---
referencia00 <- var.referencia()
referencia <- referencia00[[c('pp_winter_mean_1980_2010_nueva_area_estudio_geo_ok', 'tmax_summer_mean_1980_2010_nueva_area_estudio_geo_ok', 'tmin_winter_mean_1980_2010_nueva_area_estudio_geo_ok',
                              'sen_slope_pp_1980_2010_geo_ok', 'sen_slope_tmax_1980_2010_geo_ok', 'sen_slope_tmin_1980_2010_geo_ok', 
                              'clip_dem_res_30m_nueva_area_estudio_geo', 'clip_orientacion_res_30m_nueva_area_estudio_geo', 'clip_pendiente_res_30m_nueva_area_estudio_geo')]]
names(referencia) <- new_names
plot(referencia)

# ---
csiro50_45 <- var.futuro('CSIRO', 'rcp45', 2050)
names(csiro50_45) <- new_names
csiro50_85 <- var.futuro('CSIRO', 'rcp85', 2050)
names(csiro50_85) <- new_names
csiro70_45 <- var.futuro('CSIRO', 'rcp45', 2070)
names(csiro70_45) <- new_names
csiro70_85 <- var.futuro('CSIRO', 'rcp85', 2070)
names(csiro70_85) <- new_names

ipsl50_45 <- var.futuro('IPSL', 'rcp45', 2050)
names(ipsl50_45) <- new_names
ipsl50_85 <- var.futuro('IPSL', 'rcp85', 2050)
names(ipsl50_85) <- new_names
ipsl70_45 <- var.futuro('IPSL', 'rcp45', 2070)
names(ipsl70_45) <- new_names
ipsl70_85 <- var.futuro('IPSL', 'rcp85', 2070)
names(ipsl70_85) <- new_names

ccsm450_45 <- var.futuro('CCSM4', 'rcp45', 2050)
names(ccsm450_45) <- new_names
ccsm450_85 <- var.futuro('CCSM4', 'rcp85', 2050)
names(ccsm450_85) <- new_names
ccsm470_45 <- var.futuro('CCSM4', 'rcp45', 2070)
names(ccsm470_45) <- new_names
ccsm470_85 <- var.futuro('CCSM4', 'rcp85', 2070)
names(ccsm470_85) <- new_names

miroc50_45 <- var.futuro('MIROC', 'rcp45', 2050)
names(miroc50_45) <- new_names
miroc50_85 <- var.futuro('MIROC', 'rcp85', 2050)
names(miroc50_85) <- new_names
miroc70_45 <- var.futuro('MIROC', 'rcp45', 2070)
names(miroc70_45) <- new_names
miroc70_85 <- var.futuro('MIROC', 'rcp85', 2070)
names(miroc70_85) <- new_names

#plot(miroc70_85)

# fin ---




# ajuste modelo ---
marco.clip <- marco_zona_austral
g.clip <- g.austral

var.predict.i <- trans.var.model(referencia, marco.clip)
#names(var.predict.i) <- new_names_jackknife # habilitar para cambiar nombre al eje y
plot(var.predict.i)

plot(var.predict.i[[1]])
plot(marco.clip, add=TRUE, border='red')
plot(g.clip, add=TRUE)

p.presencia <- g.clip@data[,c('x', 'y')]
p.background <- randomPoints(var.predict.i, 10000, p = g.clip, excludep = T) # x puntos random del ´background, excluyendo puntos de presencia

m.aju <- maxent(x=var.predict.i, p=p.presencia, a=p.background, factors=c('orientacion'), # por default es cloglog
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=5',
                    'autofeature=TRUE')) # en este caso, por default son: lineal, producto, cuadratico y hinge
plot(m.aju)
response(m.aju, 1, range = 'pa', expand =15)
density(m.aju)
dev.off()

predict.ref <- predict(x = var.predict.i, object = m.aju, args=c('extrapolate=TRUE', 'doclamp=TRUE'))
plot(predict.ref)

# fin ---




# referencia ----
marco.j <- marco_zona_austral_chile_argentina
referencia.chi.arg <- trans.var.model(referencia, marco.j)
plot(referencia.chi.arg)

predict.ref.chi.arg <- predict(x = referencia.chi.arg, object = m.aju, args=c('extrapolate=TRUE', 'doclamp=TRUE'))
plot(predict.ref.chi.arg)

# fin ---



# extrapolacion espacial y temporal ----
model.j1 <- trans.var.model(csiro50_45, marco.j)
model.j2 <- trans.var.model(csiro50_85, marco.j)
model.j3 <- trans.var.model(csiro70_45, marco.j)
model.j4 <- trans.var.model(csiro70_85, marco.j)

plot(model.j1)
plot(model.j4)

f1 <- predict(x = model.j1, object = m.aju, args=c('extrapolate=TRUE', 'doclamp=TRUE'))
f2 <- predict(x = model.j2, object = m.aju, args=c('extrapolate=TRUE', 'doclamp=TRUE'))
f3 <- predict(x = model.j3, object = m.aju, args=c('extrapolate=TRUE', 'doclamp=TRUE'))
f4 <- predict(x = model.j4, object = m.aju, args=c('extrapolate=TRUE', 'doclamp=TRUE'))

par(mfrow=c(2,2), mar=c(2,2,2,2))

plot(f1, main='CSIRO 2050 RCP 4.5')
plot(f2, main='CSIRO 2050 RCP 8.5')
plot(f3, main='CSIRO 2070 RCP 4.5')
plot(f4, main='CSIRO 2070 RCP 8.5')
#dev.off()



setwd('C:/Users/Usuario/Documents/Francisco/predicciones_maxent/')
#writeRaster(predict.ref.chi.arg, filename='prediccion_periodo_referencia_austral.tif', format="GTiff", overwrite=TRUE)

writeRaster(f1, filename='prediccion_csiro_2050_rcp45_zona_austral.tif', format="GTiff", overwrite=TRUE)
writeRaster(f2, filename='prediccion_csiro_2050_rcp85_zona_austral.tif', format="GTiff", overwrite=TRUE)
writeRaster(f3, filename='prediccion_csiro_2070_rcp45_zona_austral.tif', format="GTiff", overwrite=TRUE)
writeRaster(f4, filename='prediccion_csiro_2070_rcp85_zona_austral.tif', format="GTiff", overwrite=TRUE)




# otros ----

# par(mfrow=c(2,2), mar=c(2,2,2,2))
# 
# #umbral <- 0.5
# col.i <- c('#0B3861', '#00FFFF')
# #brks.i <- c( seq(0, 1, by=0.1) )
# 
# plot(f1>0.5, main='CSIRO 2050 RCP 4.5', col = colorRampPalette(col.i)(1000))#( length(brks.i) ), breaks = brks.i)
# 
# plot(f2>0.5, main='CSIRO 2050 RCP 8.5', col = colorRampPalette(col.i)(1000))#( length(brks.i) ), breaks = brks.i)
# 
# plot(f3>0.5, main='CSIRO 2070 RCP 4.5', col = colorRampPalette(col.i)(1000))#( length(brks.i) ), breaks = brks.i)
# 
# plot(f4>0.5, main='CSIRO 2070 RCP 8.5', col = colorRampPalette(col.i)(1000))#( length(brks.i) ), breaks = brks.i)#
# 
# #dev.off()
