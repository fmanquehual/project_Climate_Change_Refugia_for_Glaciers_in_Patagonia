library('dismo')
library('rgdal')
library('rgeos')
library('raster')
library('rJava')
library('ggplot2')
library('PerformanceAnalytics')
library('usdm')
library("gridExtra")

# funciones ----

sameName <- function(var.futuro){

  names(var.futuro)[1] <- names(var.predict)[1]
  names(var.futuro)[2] <- names(var.predict)[2]
  names(var.futuro)[3] <- names(var.predict)[3]
  names(var.futuro)[4] <- names(var.predict)[4]
  names(var.futuro)[5] <- names(var.predict)[5]
  names(var.futuro)[6] <- names(var.predict)[6]
  names(var.futuro)[7] <- names(var.predict)[7]
  names(var.futuro)[8] <- names(var.predict)[8]
  names(var.futuro)[9] <- names(var.predict)[9]
  
  return(var.futuro)
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
names(var.predict) <- c("tmin_verano", "tmin_invierno", "tmax_verano", 
                 "tmax_invierno", "pp_verano", "pp_invierno", 
                 'orientacion', 'pendiente', 'altitud')
plot(var.predict)
# fin ---







# VP futuro ----

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_futuro/')

ipsl50_60 <- stack('stack_IPSL_2050_60.tif')
ipsl50_60 <- sameName(ipsl50_60) 

miroc50_60 <- stack('stack_MIROC_2050_60.tif')
miroc50_60 <- sameName(miroc50_60)

ccsm450_60 <- stack('stack_CCSM4_2050_60.tif')
ccsm450_60 <- sameName(ccsm450_60)

csiro50_60 <- stack('stack_CSIRO_2050_60.tif')
csiro50_60 <- sameName(csiro50_60)

ipsl50_85 <- stack('stack_IPSL_2050_85.tif')
ipsl50_85 <- sameName(ipsl50_85)

miroc50_85 <- stack('stack_MIROC_2050_85.tif')
miroc50_85 <- sameName(miroc50_85)

ccsm450_85 <- stack('stack_CCSM4_2050_85.tif')
ccsm450_85 <- sameName(ccsm450_85)

csiro50_85 <- stack('stack_CSIRO_2050_85.tif')
csiro50_85 <- sameName(csiro50_85)

ipsl70_60 <- stack('stack_IPSL_2070_60.tif')
ipsl70_60 <- sameName(ipsl70_60)

miroc70_60 <- stack('stack_MIROC_2070_60.tif')
miroc70_60 <- sameName(miroc70_60)

ccsm470_60 <- stack('stack_CCSM4_2070_60.tif')
ccsm470_60 <- sameName(ccsm470_60)

csiro70_60 <- stack('stack_CSIRO_2070_60.tif')
csiro70_60 <- sameName(csiro70_60)

ipsl70_85 <- stack('stack_IPSL_2070_85.tif')
ipsl70_85 <- sameName(ipsl70_85)

miroc70_85 <- stack('stack_MIROC_2070_85.tif')
miroc70_85 <- sameName(miroc70_85)

ccsm470_85 <- stack('stack_CCSM4_2070_85.tif')
ccsm470_85 <- sameName(ccsm470_85)

csiro70_85 <- stack('stack_CSIRO_2070_85.tif')
csiro70_85 <- sameName(csiro70_85)

# fin ---





# VIF ----

vif(var.predict4)

#vifstep(var.predict, th=10)

# var.predict2 <- stack(var.predict[[c('tmax_verano', 'pp_invierno', 'orientacion', 'pendiente', 'altitud')]])
# plot(var.predict2)

# fin ---




# tranformacion de variable ----

trans <- function(var.name){
m.k <- var.predict[[var.name]]
m.k_tranformado <- exp(m.k)
names(m.k_tranformado) <- names(var.predict[[var.name]])
return(m.k_tranformado)}

# names(var.predict)


# var.predict2 <- stack(var.predict[['tmin_verano']], var.predict[['pp_verano']], 
#                       var.predict[['orientacion']], var.predict[['pendiente']], var.predict[['altitud']])
# 
# var.predict3 <- stack(var.predict[['tmax_verano']], var.predict[['pp_invierno']], 
#                       var.predict[['orientacion']], var.predict[['pendiente']], var.predict[['altitud']])

var.predict4 <- stack(trans('tmin_invierno'), trans('tmax_verano'), var.predict[['pp_invierno']],
                      var.predict[['orientacion']], var.predict[['pendiente']], var.predict[['altitud']])

# var.predict2 <- stack(var.predict[['tmin_invierno']], var.predict[['tmax_verano']], var.predict[['pp_invierno']],
#                       var.predict[['orientacion']], var.predict[['pendiente']], var.predict[['altitud']])

# var.predict2 <- stack(var.predict[['tmin_verano']], 
#                       var.predict[['pp_invierno']], 
#                       var.predict[['orientacion']], var.predict[['pendiente']], var.predict[['altitud']])

plot(var.predict4)





















# fin ---






# MAXENT ----
setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

g.presencia.shp <- readOGR(".", "points_presencia_glaciares")
poly.presencia.shp <- readOGR(".", "polygon_glaciares_cuenca_baker_para_presencia_19s")
head(g.presencia.shp@data)
dim(g.presencia.shp)






# calibracion y evaluacion ---
m1.auc <- c()
m2.auc <- c() 
m3.auc <- c() 
m4.auc <- c() 

# m1.max.tss <- c()
# m2.max.tss <- c()
# m3.max.tss <- c()
# m4.max.tss <- c()

var.predict.i <- var.predict4

presvals <- extract(var.predict.i, g.presencia.shp)
backgr <- randomPoints(var.predict.i, 10000, p = g.presencia.shp, excludep = T) # 10000 puntos random del fondo, excluyendo puntos de presencia


k <- 10
group.p <- kfold(presvals, k)
group.b <- kfold(backgr, k)

#for (i in 1:k) {
i <- 10

print( paste('ITERACION', i, 'EN EJECUCION', 'DE', k, sep = ' ') )
  

pres_train <- g.presencia.shp@data[group.p != i, ]
pres_train <- pres_train[, c('x', 'y')]

pres_test <- g.presencia.shp@data[group.p == i, ]
pres_test <- pres_test[, c('x', 'y')]

backgr_train <- backgr[group.b != i, ]
backgr_test <- backgr[group.b == i, ]

# dev.off()
# plot(var.predict[[1]])
# plot(lim, add=TRUE, lwd=2)
# points(pres_train, pch= '+', col='red', cex=0.5)
# points(pres_test, pch='+', col='blue', cex=0.5)
# points(backgr_train, pch= '.', col='black', cex=4)
# points(backgr_test, pch='.', col='white', cex=4)


#plot(var.predict)
m1 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion', 'pendiente', 'altitud'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'defaultprevalence=0.5', 'betamultiplier=0.1',#'outputformat=raw',
                    'autofeature=FALSE',
                    'linear=TRUE',
                    'quadratic=TRUE',
                    'product=TRUE',
                    'threshold=TRUE',
                    'hinge=TRUE'))
#m1
# plot(m1)
# response(m1)

m2 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion', 'pendiente', 'altitud'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'defaultprevalence=0.5', 'betamultiplier=1',#'outputformat=raw',
                    'autofeature=FALSE',
                    'linear=TRUE',
                    'quadratic=TRUE',
                    'product=TRUE',
                    'threshold=TRUE',
                    'hinge=TRUE'))

m3 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion', 'pendiente', 'altitud'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'defaultprevalence=0.5', 'betamultiplier=5',#'outputformat=raw',
                    'autofeature=FALSE',
                    'linear=TRUE',
                    'quadratic=TRUE',
                    'product=TRUE',
                    'threshold=TRUE',
                    'hinge=TRUE'))

m4 <- maxent(x=var.predict.i, p=pres_train, a=backgr_train, factors=c('orientacion', 'pendiente', 'altitud'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'defaultprevalence=0.5', 'betamultiplier=10',#'outputformat=raw',
                    'autofeature=FALSE',
                    'linear=TRUE',
                    'quadratic=TRUE',
                    'product=TRUE',
                    'threshold=TRUE',
                    'hinge=TRUE'))


m1_e_test <- evaluate(pres_test, backgr_test, m1, var.predict.i)
m2_e_test <- evaluate(pres_test, backgr_test, m2, var.predict.i)
m3_e_test <- evaluate(pres_test, backgr_test, m3, var.predict.i)
m4_e_test <- evaluate(pres_test, backgr_test, m4, var.predict.i)
#e_test

if(i==1){m1.auc <- c( round(m1_e_test@auc, 4) )}else(m1.auc <- c(m1.auc, round(m1_e_test@auc, 4)))
if(i==1){m2.auc <- c( round(m2_e_test@auc, 4) )}else(m2.auc <- c(m2.auc, round(m2_e_test@auc, 4)))
if(i==1){m3.auc <- c( round(m3_e_test@auc, 4) )}else(m3.auc <- c(m3.auc, round(m3_e_test@auc, 4)))
if(i==1){m4.auc <- c( round(m4_e_test@auc, 4) )}else(m4.auc <- c(m4.auc, round(m4_e_test@auc, 4)))

# if(i==1){m1.max.tss <- c( m1_e_test@t[which.max( m1_e_test@TPR+m1_e_test@TNR-1 )] )}else(m1.max.tss <- c(m1.max.tss, m1_e_test@t[which.max( m1_e_test@TPR+m1_e_test@TNR-1 )]) )
# if(i==1){m2.max.tss <- c( m2_e_test@t[which.max( m2_e_test@TPR+m2_e_test@TNR-1 )] )}else(m2.max.tss <- c(m2.max.tss, m2_e_test@t[which.max( m2_e_test@TPR+m2_e_test@TNR-1 )]) )
# if(i==1){m3.max.tss <- c( m3_e_test@t[which.max( m3_e_test@TPR+m3_e_test@TNR-1 )] )}else(m3.max.tss <- c(m3.max.tss, m3_e_test@t[which.max( m3_e_test@TPR+m3_e_test@TNR-1 )]) )
# if(i==1){m4.max.tss <- c( m4_e_test@t[which.max( m4_e_test@TPR+m4_e_test@TNR-1 )] )}else(m4.max.tss <- c(m4.max.tss, m4_e_test@t[which.max( m4_e_test@TPR+m4_e_test@TNR-1 )]) )

#}

# m2_e_test@TPR # TRUE POSITIVE RATE (SENSIBILIDAD)
# m2_e_test@TNR # TRUE NEGATIVE RATE (ESPECIFICIDAD)


# AUC ----
as.matrix(m1.auc)
min(m1.auc)
max(m1.auc)
round(mean(m1.auc), 4)
round(sd(m1.auc), 4)

as.matrix(m2.auc)
min(m2.auc)
max(m2.auc)
round(mean(m2.auc), 4)
round(sd(m2.auc), 4)

as.matrix(m3.auc)
min(m3.auc)
max(m3.auc)
round(mean(m3.auc), 4)
round(sd(m3.auc), 4)

as.matrix(m4.auc)
min(m4.auc)
max(m4.auc)
round(mean(m4.auc), 4)
round(sd(m4.auc), 4)

# fin ---




# TSS ----

# as.matrix(m1.max.tss)
# min(m1.max.tss)
# max(m1.max.tss)
# round(mean(m1.max.tss), 4)
# round(sd(m1.max.tss), 4)
# 
# as.matrix(m2.max.tss)
# min(m2.max.tss)
# max(m2.max.tss)
# round(mean(m2.max.tss), 4)
# round(sd(m2.max.tss), 4)
# 
# as.matrix(m3.max.tss)
# min(m3.max.tss)
# max(m3.max.tss)
# round(mean(m3.max.tss), 4)
# round(sd(m3.max.tss), 4)
# 
# as.matrix(m4.max.tss)
# min(m4.max.tss)
# max(m4.max.tss)
# round(mean(m4.max.tss), 4)
# round(sd(m4.max.tss), 4)



m1
m2
m3
m4

# setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
# png("curvas_de_respuesta_6var.png", width = 650, height = 500, units = "px")

par(mfrow=c(2,2))

response(m1, 'pp_invierno', main='Modelo 1')
response(m2, 'pp_invierno', main='Modelo 2')
response(m3, 'pp_invierno', main='Modelo 3')
response(m4, 'pp_invierno', main='Modelo 4')

dev.off()

# fin ---












# modelo final ----
k.i <- 1:k

pres_pre_final <- g.presencia.shp@data[group.p%in%k.i, ]
pres_final <- pres_pre_final[, c('x', 'y')]

backgr_final <- backgr[group.b%in%k.i, ]


m3 <- maxent(x=var.predict.i, p=pres_final, a=backgr_final, factors=c('orientacion', 'pendiente', 'altitud'), 
             args=c('jackknife=TRUE', 'responsecurves=TRUE', 'defaultprevalence=0.5', 'betamultiplier=5',#'outputformat=raw',
                    'autofeature=FALSE',
                    'linear=TRUE',
                    'quadratic=TRUE',
                    'product=TRUE',
                    'threshold=TRUE',
                    'hinge=TRUE'))

# setwd('C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/')
# png('contribucion_variables.png')
# plot(m3, main='Contribución de las variables', xlab='(%)')
# dev.off()

ej <- predict(x = var.predict.i, object = m3, args=c('extrapolate=TRUE', 'doclamp=TRUE')) # logistic output
plot(ej>0.5)#, main='m4')










# # umbral ----
# 
# # Hijmans y Graham 2006
# 
# m3.umbral.1 <- 0.35 # tss asignado de forma arbitraria 
# 
# m3.tss.mean <- 0.3896 # promedio
# 
# m3.tss.ult <- m3.max.tss[length(m3.max.tss)] # ultimo tss calculado
# 
# m3.umbral.2 <- 0.45 # tss asignado de forma arbitraria 
# 
# m3.umbral.3 <- 0.5 # tss asignado de forma arbitraria 
# 
# m3.umbral.4 <- 0.55 # tss asignado de forma arbitraria 
# 
# m3.umbral.5 <- 0.6 # tss asignado de forma arbitraria 
# 
# m3.umbral.6 <- 0.65 # tss asignado de forma arbitraria 
# 
# m3.umbral.7 <- 0.7 # tss asignado de forma arbitraria 
# 
# umbrales <- c(m3.umbral.1, m3.tss.mean, m3.tss.ult, m3.umbral.2, m3.umbral.3, m3.umbral.4, m3.umbral.5, m3.umbral.6, m3.umbral.7)
# #tr_test <- threshold(m1_e_test, 'spec_sens')
# #plot(ej > m3.tss.ok, main='Presencia/Ausencia')
# 
# m <- c()
# c <- c()
# o <- c()
# rrs <- c()
# oi <- c()
# fpr <- c()
# fnr <- c()
# 
# 
# #for (i in 1:length(umbrales)) {
# #i <- 2
# 
# print( paste('ITERACION', i, 'EN EJECUCION', 'DE', length(umbrales), sep = ' ') )
# 
# y <- ej
# 
# # (begin, end, value ...)
# v1 <- c(0, umbrales[i], 1)
# 
# m1 = c(v1[1], v1[2], NA,
#        v1[2], v1[3], 1)
# 
# rclmat.1 <- matrix(m1, ncol=3, byrow=TRUE)
# y.reclas <- reclassify(y, rclmat.1)
# # plot(y.reclas, col=rev(heat.colors(2)))
# 
# poly.glaciar.predict <- rasterToPolygons(y.reclas, dissolve = TRUE)
# # plot(poly.glaciar.predict)
# 
# # par(mfrow=c(1,2))
# # plot(poly.presencia.shp)
# # plot(poly.glaciar.predict)
# 
# 
# m <- gArea(poly.presencia.shp)/1000000 ; m
# c <- gArea(poly.glaciar.predict)/1000000 ; c
# 
# 
# if( m<=c ){rrs.pre <- (c/m)-1}else(rrs.pre <- -1*(m/c-1) )
# #rrs # 0 es lo ideal
# 
# overlap <- gIntersection(poly.glaciar.predict, poly.presencia.shp)
# 
# # par(mfrow=c(1,3))
# # plot(poly.glaciar.predict)
# # plot(poly.presencia.shp)
# # plot(overlap)
# 
# #dev.off()
# 
# o <- gArea(overlap)/1000000; o
# 
# oi <- o/m ; oi # 1 es lo ideal
# 
# fpr <- (c-o)/m ; fpr # 0 es lo ideal
# 
# fnr <- (m-o)/m ; fnr # 0 es lo ideal
# 
# 
# if(i==1){m.i <- c( round(m, 4) )}else(m.i <- c(m.i, round(m, 4)))
# if(i==1){c.i <- c( round(c, 4) )}else(c.i <- c(c.i, round(c, 4)))
# if(i==1){o.i <- c( round(o, 4) )}else(o.i <- c(o.i, round(o, 4)))
# if(i==1){rrs.i <- c( round(rrs.pre, 4) )}else(rrs.i <- c(rrs.i, round(rrs.pre, 4)))
# if(i==1){oi.i <- c( round(oi, 4) )}else(oi.i <- c(oi.i, round(oi, 4)))
# if(i==1){fpr.i <- c( round(fpr, 4) )}else(fpr.i <- c(fpr.i, round(fpr, 4)))
# if(i==1){fnr.i <- c( round(fnr, 4) )}else(fnr.i <- c(fnr.i, round(fnr, 4)))
# 
# # }
# 
# db.umbrales <- data.frame(m=m.i, c=c.i, o=o.i, rrs=rrs.i, oi=oi.i, fpr=fpr.i, fnr=fnr.i)
# db.umbrales
# # fin ---

umbral.final <- 0.5 ; umbral.final
















#setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")

#png("prediccion_2011_glaciares_fuera_cuenca.png", width = 1000, height = 750, units = "px")

par(mfrow=c(2,2))
plot(ej, main="Predicción MAXENT 2011")
plot(lim, add=TRUE)

plot(ej > umbral.final, main='Presencia/Ausencia threshold > 50%')
plot(lim, add=TRUE)

plot(lim, axes=TRUE, main="Glaciares Inventariados 2011 (DGA)")
plot(g.rasterizado, add=TRUE, col="green", border="green")
plot(lim, add=TRUE)

#dev.off()

# setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
# writeRaster(ej, filename="ej_maxent_output", format="GTiff", overwrite=TRUE)
# fin ---



# prediccion futuro ----
# dev.off()
# 
trans2 <- function(model.i, var.name){
  m.k <- model.i[[var.name]]
  m.k_tranformado <- exp(m.k)
  names(m.k_tranformado) <- names(model.i[[var.name]])
  return(m.k_tranformado)}

# anho 2050 RCP 6.0
# ipsl50_60
# miroc50_60
# ccsm450_60
# csiro50_60

# anho 2050 RCP 8.5
# ipsl50_85
# miroc50_85
# ccsm450_85
# csiro50_85

# anho 2070 RCP 6.0
# ipsl70_60
# miroc70_60
# ccsm470_60
# csiro70_60

# anho 2070 RCP 8.5
# ipsl70_85
# miroc70_85
# ccsm470_85
# csiro70_85


model.pre.j1 <- ccsm450_60
model.j1 <- stack(trans2(model.pre.j1, 'tmin_invierno'), trans2(model.pre.j1, 'tmax_verano'), model.pre.j1[['pp_invierno']],
                  model.pre.j1[['orientacion']], model.pre.j1[['pendiente']], model.pre.j1[['altitud']])

model.pre.j2 <- ccsm450_85
model.j2 <- stack(trans2(model.pre.j2, 'tmin_invierno'), trans2(model.pre.j2, 'tmax_verano'), model.pre.j2[['pp_invierno']],
                  model.pre.j2[['orientacion']], model.pre.j2[['pendiente']], model.pre.j2[['altitud']])

model.pre.j3 <- ccsm470_60
model.j3 <- stack(trans2(model.pre.j3, 'tmin_invierno'), trans2(model.pre.j3, 'tmax_verano'), model.pre.j3[['pp_invierno']],
                  model.pre.j3[['orientacion']], model.pre.j3[['pendiente']], model.pre.j3[['altitud']])

model.pre.j4 <- ccsm470_85
model.j4 <- stack(trans2(model.pre.j4, 'tmin_invierno'), trans2(model.pre.j4, 'tmax_verano'), model.pre.j4[['pp_invierno']],
                  model.pre.j4[['orientacion']], model.pre.j4[['pendiente']], model.pre.j4[['altitud']])


f1 <- predict(x = model.j1, object = m3, args=c('extrapolate=TRUE', 'doclamp=TRUE'))
f2 <- predict(x = model.j2, object = m3, args=c('extrapolate=TRUE', 'doclamp=TRUE'))
f3 <- predict(x = model.j3, object = m3, args=c('extrapolate=TRUE', 'doclamp=TRUE'))
f4 <- predict(x = model.j4, object = m3, args=c('extrapolate=TRUE', 'doclamp=TRUE'))

# plot presente v/S futuro

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
png("prediccion_CCSM4_6var.png", width = 550, height = 550, units = "px")

par(mfrow=c(2,2), mar=c(2,2,2,2))

plot(f1, main='2050 RCP 6.0')#, axes=FALSE, legend=FALSE)
plot(lim, add=TRUE)

plot(f2, main='2050 RCP 8.5')#, axes=FALSE)
plot(lim, add=TRUE)

plot(f3, main='2070 RCP 6.0')#, axes=FALSE, legend=FALSE)
plot(lim, add=TRUE)

plot(f4, main='2070 RCP 8.5')#, axes=FALSE)
plot(lim, add=TRUE)

dev.off()




par(mfrow=c(2,2))

plot(f1 > umbral.final, main='2050 RCP 6.0')
plot(lim, add=TRUE)

plot(f2 > umbral.final, main='2050 RCP 8.5')
plot(lim, add=TRUE)

plot(f3 > umbral.final, main='2070 RCP 6.0')
plot(lim, add=TRUE)

plot(f4 > umbral.final, main='2070 RCP 8.5')
plot(lim, add=TRUE)





setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/resultados_prediccion/')
f <- stack(ej, f1, f2, f3, f4)
plot(f)
# writeRaster(f, filename='prediccion_csiro_6var.tif', format="GTiff", overwrite=TRUE)

