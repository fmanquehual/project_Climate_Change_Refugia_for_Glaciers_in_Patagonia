library('dismo')
library('rgdal')
library('rgeos')
library('raster')
library('rJava')
library('ggplot2')
library('PerformanceAnalytics')
library('usdm')
library('cowplot')
library('xtable')

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# funciones ----

# modelo: 'CSIRO', 'IPSL', 'CCSM4', 'MIROC'
# rcp: 'rcp45', 'rcp85'
# anho.i: '2050', '2070'

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
trans <- function(var.name){
  v.i <- var.predict[[var.name]]
  v.i_trans <- exp(v.i)
  names(v.i_trans) <- names(var.predict[[var.name]])
  return(v.i_trans)}

# f3
summary.threshold <- function(AUC_TSS_KAPPA){
  a <- as.matrix(AUC_TSS_KAPPA)
  b <- min(AUC_TSS_KAPPA)
  c <- max(AUC_TSS_KAPPA)
  d <- round(mean(AUC_TSS_KAPPA), 4)
  e <- round(sd(AUC_TSS_KAPPA), 4)
  
  estadisticos <- c(b, c, d, e)
  return(estadisticos)
}

# fin ---






# lectura coberturas ----

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
marco <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')
marco_zona_sur <- readOGR('.', 'polygon_zona_glaciologica_sur_geo')
marco_zona_austral <- readOGR('.', 'polygon_zona_glaciologica_austral_geo')

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/var_presencia/')
g.sur <- readOGR('.', 'points_presencia_glaciares_zona_glaciologica_sur')
g.austral <- readOGR('.', 'points_presencia_glaciares_zona_glaciologica_austral')

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/Glaciares_Nacional/')
g.poly <- readOGR('.', 'polygon_glaciares_marco_trabajo_nuevo_geo')

new_names <- c('pp_winter', 'tmax_summer', 'tmin_winter',
               'slope_pp_winter', 'slope_tmax_summer', 'slope_tmin_winter', 
               'altitud', 'orientacion', 'pendiente')

new_names0 <- c('pp_summer', 'pp_winter', 'tmax_summer', 
                'tmax_winter', 'tmin_summer', 'tmin_winter',
                'altitud', 'orientacion', 'pendiente')

new_names1 <- c('pp_winter', 'tmax_summer', 'tmin_winter',
                'altitud', 'orientacion', 'pendiente')

referencia00 <- var.referencia()
names(referencia00)

referencia0 <- referencia00[[c("pp_summer_mean_1980_2010_nueva_area_estudio_geo_ok", "pp_winter_mean_1980_2010_nueva_area_estudio_geo_ok",  
                               "tmax_summer_mean_1980_2010_nueva_area_estudio_geo_ok", "tmax_winter_mean_1980_2010_nueva_area_estudio_geo_ok",
                               "tmin_summer_mean_1980_2010_nueva_area_estudio_geo_ok", "tmin_winter_mean_1980_2010_nueva_area_estudio_geo_ok",
                               "clip_dem_res_30m_nueva_area_estudio_geo", "clip_orientacion_res_30m_nueva_area_estudio_geo", 
                               "clip_pendiente_res_30m_nueva_area_estudio_geo")]]
names(referencia0) <- new_names0
plot(referencia0)

referencia <- referencia00[[c('pp_winter_mean_1980_2010_nueva_area_estudio_geo_ok', 'tmax_summer_mean_1980_2010_nueva_area_estudio_geo_ok', 'tmin_winter_mean_1980_2010_nueva_area_estudio_geo_ok',
                              'sen_slope_pp_1980_2010_geo_ok', 'sen_slope_tmax_1980_2010_geo_ok', 'sen_slope_tmin_1980_2010_geo_ok', 
                              'clip_dem_res_30m_nueva_area_estudio_geo', 'clip_orientacion_res_30m_nueva_area_estudio_geo', 'clip_pendiente_res_30m_nueva_area_estudio_geo')]]
names(referencia) <- new_names 
# fin ---










# VIF y transformacion de variables ----
var.predict <- referencia
names(var.predict)

for (i in 1:10) {
  vif.i <- vif(var.predict)
  if(i==1){vif0 <- cbind(vif.i['VIF'])} else( vif0 <- cbind(vif0, vif.i['VIF']) )
  print(paste('iteracion', i, 'listo', sep = ' '))
}
vif1 <- data.frame(var=new_names, vif0) ; vif1


for (i in 1:9) {
  vif.mean.i <- mean( as.vector( unlist(vif1[i,c(2:ncol(vif1))]) ) )
  if(i==1){vif.mean0 <- rbind(vif.mean.i) } else( vif.mean0 <- rbind(vif.mean0, vif.mean.i) )
  print(paste('iteracion', i, 'listo', sep = ' '))
}

vif.mean.ori <- data.frame(var=new_names, vif.mean0) ; vif.mean.ori


# var.predict.vif <- stack(var.predict[['pp_winter']], var.predict[['tmax_summer']], var.predict[['tmin_winter']],
#                          var.predict[['altitud']], var.predict[['orientacion']], var.predict[['pendiente']])

var.predict.vif <- stack(var.predict[['pp_winter']], trans('tmax_summer'), var.predict[['tmin_winter']],
                      var.predict[['slope_pp_winter']], var.predict[['slope_tmax_summer']], var.predict[['slope_tmin_winter']],
                      var.predict[['altitud']], var.predict[['orientacion']], var.predict[['pendiente']])

for (i in 1:10) {
  vif.i <- vif(var.predict.vif)
  if(i==1){vif0 <- cbind(vif.i['VIF'])} else( vif0 <- cbind(vif0, vif.i['VIF']) )
  print(paste('iteracion', i, 'listo', sep = ' '))
}
vif1 <- data.frame(var=new_names, vif0) ; vif1


for (i in 1:9) {
  vif.mean.i <- mean( as.vector( unlist(vif1[i,c(2:ncol(vif1))]) ) )
  if(i==1){vif.mean0 <- rbind(vif.mean.i) } else( vif.mean0 <- rbind(vif.mean0, vif.mean.i) )
  print(paste('iteracion', i, 'listo', sep = ' '))
}

vif.mean.2 <- data.frame(var=new_names, vif.mean0) ; vif.mean.2

vif.mean.ori
vif.mean.2
#plot(var.predict.vif)

# fin ---









# MAXENT ----

# calibracion y evaluacion 

var.predict.pre0.i <- var.predict.vif
presencia.glaciares <- g.austral

marco.clip <- marco_zona_austral
var.predict.pre.i <- crop(var.predict.pre0.i, marco.clip)
var.predict.i <- mask(var.predict.pre.i, marco.clip)

plot(var.predict.i[[1]])
plot(presencia.glaciares, add=TRUE)

presvals <- extract(var.predict.i, presencia.glaciares)
backgr <- randomPoints(var.predict.i, 10000, p = presencia.glaciares, excludep = T) # x puntos random del fondo, excluyendo puntos de presencia

k <- 10
group.p <- kfold(presvals, k)
group.b <- kfold(backgr, k)

num.modelos <- c('m1', 'm2', 'm3', 'm4', 'm5', 'm6', 'm7', 'm8')

for (i in 1:length(num.modelos)) {
  #i <- 2
  m.auc <- c()
  out <- paste( num.modelos[i], '.auc.test', '<-m.auc', sep = '' )  
  out2 <- paste( num.modelos[i], '.auc.train', '<-m.auc', sep = '' )  
  eval(parse(text=out))
  eval(parse(text=out2))
  
  m.tss <- c()
  out.tss <- paste( num.modelos[i], '.max.tss.test', '<-m.tss', sep = '' )
  out.tss2 <- paste( num.modelos[i], '.max.tss.train', '<-m.tss', sep = '' )
  eval(parse(text=out.tss))
  eval(parse(text=out.tss2))
  
  m.kappa <- c()
  out.kappa <- paste( num.modelos[i], '.max.kappa.test', '<-m.kappa', sep = '' )
  out.kappa2 <- paste( num.modelos[i], '.max.kappa.train', '<-m.kappa', sep = '' )
  eval(parse(text=out.kappa))
  eval(parse(text=out.kappa2))
}


for (i in 1:k) {
#for (i in 1:2) {
#i <- 1

print( paste('ITERACION', i, 'EN EJECUCION', 'DE', k, sep = ' ') )


pres_train <- presencia.glaciares@data[group.p != i, ]
pres_train <- pres_train[, c('x', 'y')]

pres_test <- presencia.glaciares@data[group.p == i, ]
pres_test <- pres_test[, c('x', 'y')]

backgr_train <- backgr[group.b != i, ]
backgr_test <- backgr[group.b == i, ]

# plot(var.predict.i[[1]])
# points(pres_train, pch= '+', col='red', cex=0.5)
# points(pres_test, pch='+', col='blue', cex=0.5)
# points(backgr_train, pch= '.', col='black', cex=1.5)
# points(backgr_test, pch='.', col='cyan', cex=1.5)

m1 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=1', #'defaultprevalence=0.5',#'outputformat=raw',
                    'autofeature=TRUE'))#,
                    # 'linear=TRUE',
                    # 'quadratic=FALSE',
                    # 'product=FALSE',
                    # 'threshold=FALSE',
                    # 'hinge=FALSE'))
print( 'Modelo 1' )

m2 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=2',
                    'autofeature=TRUE'))
print( 'Modelo 2' )

m3 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=3',
                    'autofeature=TRUE'))
print( 'Modelo 3' )

m4 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=4',
                    'autofeature=TRUE'))
print( 'Modelo 4' )

m5 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=5',
                    'autofeature=TRUE'))
print( 'Modelo 5' )

m6 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=6',
                    'autofeature=TRUE'))
print( 'Modelo 6' )

m7 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=7',
                    'autofeature=TRUE'))
print( 'Modelo 7' )

m8 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'betamultiplier=8',
                    'autofeature=TRUE'))
print( 'Modelo 8' )

for (j in 1:length(num.modelos)) {
  #j <- 1
  # evalueate ---
  m.j <- eval(parse(text=num.modelos[j]))
  e_test <- evaluate(pres_test, backgr_test, m.j, var.predict.i)
  e_test2 <- evaluate(pres_train, backgr_train, m.j, var.predict.i)
  out <- paste( num.modelos[j], '_e_test', '<-e_test', sep = '' )
  out.2 <- paste( num.modelos[j], '_e_test2', '<-e_test2', sep = '' )
  eval(parse(text=out))
  eval(parse(text=out.2))
  
  # AUC ---
  out0 <- paste( num.modelos[j], '.auc.test', sep = '' )
  out02 <- paste( num.modelos[j], '.auc.train', sep = '' )
  
  auc.i <- c( eval(parse(text = out0)), round(e_test@auc, 4) )
  auc.j <- c( eval(parse(text = out02)), round(e_test2@auc, 4) )
  
  out2 <- paste( num.modelos[j], '.auc.test', '<-auc.i', sep = '' )
  out2.2 <- paste( num.modelos[j], '.auc.train', '<-auc.j', sep = '' )
  eval(parse(text=out2))
  eval(parse(text=out2.2))
  
  # TSS, segun Rainer et al., (2019) ---
  out.tss <- paste( num.modelos[j], '.max.tss.test', sep = '' )
  out.tss2 <- paste( num.modelos[j], '.max.tss.train', sep = '' )
  
  max.tss.i <- c( eval(parse(text = out.tss)), round( threshold(e_test, stat = 'spec_sens'), 4) )
  max.tss.j <- c( eval(parse(text = out.tss2)), round( threshold(e_test2, stat = 'spec_sens'), 4) )
  out3 <- paste( num.modelos[j], '.max.tss.test', '<-max.tss.i', sep = '' )
  out3.2 <- paste( num.modelos[j], '.max.tss.train', '<-max.tss.j', sep = '' )
  eval(parse(text=out3))
  eval(parse(text=out3.2))
  
  # KAPPA ---
  out.kappa <- paste( num.modelos[j], '.max.kappa.test', sep = '' )
  out.kappa2 <- paste( num.modelos[j], '.max.kappa.train', sep = '' )
  
  max.kappa.i <- c( eval(parse(text = out.kappa)), round( threshold(e_test, stat = 'kappa'), 4) )
  max.kappa.j <- c( eval(parse(text = out.kappa2)), round( threshold(e_test2, stat = 'kappa'), 4) )
  out4 <- paste( num.modelos[j], '.max.kappa.test', '<-max.kappa.i', sep = '' )
  out4.2 <- paste( num.modelos[j], '.max.kappa.train', '<-max.kappa.j', sep = '' )
  eval(parse(text=out4))
  eval(parse(text=out4.2))
  
  }
}

# m2_e_test@TPR # TRUE POSITIVE RATE (SENSIBILIDAD)
# m2_e_test@TNR # TRUE NEGATIVE RATE (ESPECIFICIDAD)






# prediccion ----

idx <- which(g.poly$ZONA_GLACI=='ZONA AUSTRAL')
g.poly.clip <- g.poly[idx,]
plot(g.poly.clip)

marco.clip.19s <- spTransform(marco.clip, utm19)
g.poly.clip.19s <- spTransform(g.poly.clip, utm19)
g.poly.clip.19s.2 <- gBuffer(g.poly.clip.19s, byid=TRUE, width=0)

rrs.kappa.i <- c()
rrs.tss.i <- c()

oi.kappa.i <- c()
oi.tss.i <- c()

fpr.kappa.i <- c()
fpr.tss.i <- c()

fnr.kappa.i <- c()
fnr.tss.i <- c()

for (k in 1:length(num.modelos)) {
#k <- 1
  print( paste('Ciclo', k, 'de', length(num.modelos), sep=' ') )
  
m.k <- eval(parse(text=num.modelos[k]))

tss.k.name <- paste(num.modelos[k], '.max.tss.test', sep = '') 
tss.k <- round( mean( eval(parse(text=tss.k.name)) ), 3 )

kappa.k.name <- paste(num.modelos[k], '.max.kappa.test', sep = '') 
kappa.k <- round( mean( eval(parse(text=kappa.k.name)) ), 3 )

ej <- predict(x = var.predict.i, object = m.k, args=c('extrapolate=TRUE', 'doclamp=TRUE')) # cloglog output

# RECLAS KAPPA
matriz.reclas = c(0, kappa.k, NA,
                  kappa.k, 1, 1)

rclmat.1 <- matrix(matriz.reclas, ncol=3, byrow=TRUE)
y.reclas <- reclassify(ej, rclmat.1)
poly.glaciar.predict.kappa <- rasterToPolygons(y.reclas, dissolve = TRUE)

# RECLAS TSS
matriz.reclas2 = c(0, tss.k, NA,
                   tss.k, 1, 1)

rclmat.2 <- matrix(matriz.reclas2, ncol=3, byrow=TRUE)
y.reclas2 <- reclassify(ej, rclmat.2)
poly.glaciar.predict.tss <- rasterToPolygons(y.reclas2, dissolve = TRUE)

####
poly.glaciar.predict.kappa.19s <- spTransform(poly.glaciar.predict.kappa, utm19)
poly.glaciar.predict.tss.19s <- spTransform(poly.glaciar.predict.tss, utm19)

m <- gArea(g.poly.clip.19s.2)/1000000 ; m
c.kappa <- gArea(poly.glaciar.predict.kappa.19s)/1000000 ; c.kappa
c.tss <- gArea(poly.glaciar.predict.tss.19s)/1000000 ; c.tss

if( m<=c.kappa ){rrs.kappa.pre <- (c.kappa/m)-1} else( rrs.kappa.pre <- -1*(m/c.kappa-1) )
if( m<=c.tss ){rrs.tss.pre <- (c.tss/m)-1} else( rrs.tss.pre <- -1*(m/c.tss-1) )
#rrs # 0 es lo ideal

overlap.kappa <- intersect(poly.glaciar.predict.kappa.19s, g.poly.clip.19s.2)
overlap.tss <- intersect(poly.glaciar.predict.tss.19s, g.poly.clip.19s.2)

o.kappa <- gArea(overlap.kappa)/1000000; o.kappa
o.tss <- gArea(overlap.tss)/1000000; o.tss

oi.kappa <- o.kappa/m ; oi.kappa # 1 es lo ideal
oi.tss <- o.tss/m ; oi.tss

fpr.kappa <- (c.kappa-o.kappa)/m ; fpr.kappa # 0 es lo ideal
fpr.tss <- (c.tss-o.tss)/m ; fpr.tss

fnr.kappa <- (m-o.kappa)/m ; fnr.kappa # 0 es lo ideal
fnr.tss <- (m-o.tss)/m ; fnr.tss

# output 

rrs.kappa.i <- c(rrs.kappa.i, round(rrs.kappa.pre, 3))
rrs.tss.i <- c(rrs.tss.i, round(rrs.tss.pre, 3))

oi.kappa.i <- c(oi.kappa.i, round(oi.kappa, 3))
oi.tss.i <- c(oi.tss.i, round(oi.tss, 3))

fpr.kappa.i <- c(fpr.kappa.i, round(fpr.kappa, 3))
fpr.tss.i <- c(fpr.tss.i, round(fpr.tss, 3))

fnr.kappa.i <- c(fnr.kappa.i, round(fnr.kappa, 3))
fnr.tss.i <- c(fnr.tss.i, round(fnr.tss, 3))
 }



# plots

m1.auc.dif <- mean(m1.auc.train) - mean(m1.auc.test)
m2.auc.dif <- mean(m2.auc.train) - mean(m2.auc.test)
m3.auc.dif <- mean(m3.auc.train) - mean(m3.auc.test)
m4.auc.dif <- mean(m4.auc.train) - mean(m4.auc.test)
m5.auc.dif <- mean(m5.auc.train) - mean(m5.auc.test)
m6.auc.dif <- mean(m6.auc.train) - mean(m6.auc.test)
m7.auc.dif <- mean(m7.auc.train) - mean(m7.auc.test)
m8.auc.dif <- mean(m8.auc.train) - mean(m8.auc.test)

auc.dif <- c(m1.auc.dif, m2.auc.dif, m3.auc.dif, m4.auc.dif, m5.auc.dif, m6.auc.dif, m7.auc.dif, m8.auc.dif)

auc.eval <- c( mean(m1.auc.test), mean(m2.auc.test), mean(m3.auc.test), mean(m4.auc.test),
               mean(m5.auc.test), mean(m6.auc.test), mean(m7.auc.test), mean(m8.auc.test))
coef.regu <- 1:8

kappa.col <- '#6632D6' # MORADO
tss.col <- '#A2D632' # VERDE

p1 <- ggplot() + 
  geom_line(aes(x = coef.regu, y = auc.eval), col = 'red') + 
  geom_point(aes(x = coef.regu, y = auc.eval), col = 'black') +
  labs(x = 'Regularization multipler', y = 'AUC mean') +
  scale_x_discrete(limits = coef.regu) + 
  theme_bw()
  

p2 <- ggplot() + 
  geom_line(aes(x = coef.regu, y = auc.dif), colour="#208BB0")+  # AZUL
  geom_point(aes(x = coef.regu, y = auc.dif)) +
  labs(x = 'Regularization multipler', y = 'Calibration - Evaluation') +
  scale_x_discrete(limits = coef.regu) +
  theme_bw()

p3 <- ggplot() + 
  geom_line(aes(x = coef.regu, y = rrs.kappa.i), colour=kappa.col) +  
  geom_point(aes(x = coef.regu, y = rrs.kappa.i)) +
  geom_line(aes(x = coef.regu, y = rrs.tss.i), colour=tss.col) +  
  geom_point(aes(x = coef.regu, y = rrs.tss.i)) +
  labs(x = 'Regularization multipler', y = 'RRS') +
  scale_x_discrete(limits = coef.regu) +
  theme_bw()

p4 <- ggplot() + 
  geom_line(aes(x = coef.regu, y = oi.kappa.i), colour=kappa.col) +  
  geom_point(aes(x = coef.regu, y = oi.kappa.i)) +
  geom_line(aes(x = coef.regu, y = oi.tss.i), colour=tss.col) +  
  geom_point(aes(x = coef.regu, y = oi.tss.i)) +
  labs(x = 'Regularization multipler', y = 'OI') +
  scale_x_discrete(limits = coef.regu) +
  theme_bw()

p5 <- ggplot() + 
  geom_line(aes(x = coef.regu, y = fpr.kappa.i), colour=kappa.col) +  
  geom_point(aes(x = coef.regu, y = fpr.kappa.i)) +
  geom_line(aes(x = coef.regu, y = fpr.tss.i), colour=tss.col) +  
  geom_point(aes(x = coef.regu, y = fpr.tss.i)) +
  labs(x = 'Regularization multipler', y = 'FPR') +
  scale_x_discrete(limits = coef.regu) +
  theme_bw()

p6 <- ggplot() + 
  geom_line(aes(x = coef.regu, y = fnr.kappa.i), colour=kappa.col) +  
  geom_point(aes(x = coef.regu, y = fnr.kappa.i)) +
  geom_line(aes(x = coef.regu, y = fnr.tss.i), colour=tss.col) +  
  geom_point(aes(x = coef.regu, y = fnr.tss.i)) +
  labs(x = 'Regularization multipler', y = 'FNR') +
  scale_x_discrete(limits = coef.regu) +
  theme_bw()

setwd('C:/Users/Usuario/OneDrive/plots_paper/')
cairo_ps("grafico_AUC_difAUC_austral.eps", width = 6, height = 3)
p0 <- plot_grid(p1, p2,
               labels="AUTO", ncol = 2, nrow = 1)
p0
dev.off()

# ---

p <- plot_grid(p1, p2,
               p3, p4,
               p5, p6,
               labels="AUTO", ncol = 2, nrow = 3)

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# cairo_ps("grafico_AUC_difAUC_indices_austral.eps", width = 6, height = 8)
p
# dev.off()


# # fin ---









# estadisticos para AUC y umbrales ----
m2

auc.j <- summary.threshold(m5.auc.test)
tss.j <- summary.threshold(m5.max.tss.test)
kappa.j <- summary.threshold(m5.max.kappa.test)
est.j <- c('Min', 'Max', 'Mean', 'SD')
m.j <- '5'

df2 <- data.frame(Model=m.j, Statisticians=est.j, AUC=auc.j, TSS=tss.j, KAPPA=kappa.j)
xtable(df5, digits = 3)

# fin ---



# apuntes ---

# a <- e_test@confusion[,1]
# b <- e_test@confusion[,2]
# c <- e_test@confusion[,3]
# d <- e_test@confusion[,4]
# 
# N <- e_test@na + e_test@np
# 
# prA = (a+d)/N
# prY = (a+b)/N * (a+c)/N
# prN = (c+d)/N * (b+d)/N
# prE = prY + prN
# ej.kappa <- (prA - prE) / (1-prE)
# kappa.l <- e_test@t[which.max(ej.kappa)] # calculo max kappa
# threshold(e_test, stat = 'kappa')
# 
# ej.spec_sens <- e_test@t[which.max(e_test@TPR + e_test@TNR)] # TSS, segun Rainer et al., (2019)
# threshold(e_test, stat = 'spec_sens')
