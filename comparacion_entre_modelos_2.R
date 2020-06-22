library('raster')
library('rgdal')
library('stringr')
library('cowplot')
library('ggplot2')
library('ggspatial')
library('prettymapr')

rm(list=ls())
dev.off()

# funciones ----

# f1
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

# output ---

var.amb.pre <- stack(stack.clim.f)
if(zona.glaciologica=='sur'){var.amb <- crop(var.amb.pre, marco_zona_sur)} else(var.amb <- crop(var.amb.pre, marco_zona_austral))
if(zona.glaciologica=='sur'){var.amb.out <- mask(var.amb, marco_zona_sur)} else(var.amb.out <- mask(var.amb, marco_zona_austral))
return(var.amb.out)
}

# f2
var.referencia <- function(zona.glaciologica){

# variables de pendiente referencia ---

folder.i <- 'historico'

directory.pre.i <- 'C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok'
directory.i <- paste(directory.pre.i, folder.i, sep = '/')

setwd(directory.i)

stack.clim.pre <- stack( dir()[2], dir()[3], dir()[6] )
# output ---

var.amb.pre <- stack(stack.clim.pre)
if(zona.glaciologica=='sur'){var.amb <- crop(var.amb.pre, marco_zona_sur)} else(var.amb <- crop(var.amb.pre, marco_zona_austral))
if(zona.glaciologica=='sur'){var.amb.out <- mask(var.amb, marco_zona_sur)} else(var.amb.out <- mask(var.amb, marco_zona_austral))

return(var.amb.out)
}

# f3
var.referencia.2005 <- function(zona.glaciologica){
  
  # variables de pendiente referencia ---
  setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/historico_1980_2005/')
  ref.pre <- raster('tmax_summer_mean_1980_2005_nueva_area_estudio_geo.tif')
  
  # output ---
  if(zona.glaciologica=='sur'){var.amb <- crop(ref.pre, marco_zona_sur)} else(var.amb <- crop(ref.pre, marco_zona_austral))
  var.amb2 <- resample(var.amb, referencia[[2]], method='bilinear')
  var.amb.out <- mask(var.amb2, referencia[[2]])
  return(var.amb.out)
}

# f4
eval.stack <- function(stack.i, zona.glaciologica, var.i){
  if(zona.glaciologica=='sur'){stack.j <- crop(stack.i, marco_zona_sur)} else(stack.j <- crop(stack.i, marco_zona_austral))
  if(var.i=='pp'){range.i <- c(min.pp, max.pp)} else(range.i <- c(min.temp, max.temp))
  names(stack.j) <- c('REFERENCE', 'IPSL', 'MIROC', 'CCSM4', 'CSIRO')
  
  mean.i <- cellStats(stack.j, stat = 'mean', na.rm=TRUE)  
  sd.i <- cellStats(stack.j, stat = 'sd', na.rm=TRUE)  
  min.i <- cellStats(stack.j, stat = 'min', na.rm=TRUE)
  max.i <- cellStats(stack.j, stat = 'max', na.rm=TRUE)
  
  # label.i <- paste('SD = ', round(sd.i, 2), sep = '') ; label.i
  
  boxplot(stack.j, xlab = 'GCMs', las=1, ylim=range.i)
  points(mean.i, col='red', pch=16)
  # text(x=1, y=(max(max.i)+0.5), labels=label.i[1], cex=0.8)
  # text(x=2, y=(max(max.i)+0.5), labels=label.i[2], cex=0.8)
  # text(x=3, y=(max(max.i)+0.5), labels=label.i[3], cex=0.8)
  # text(x=4, y=(max(max.i)+0.5), labels=label.i[4], cex=0.8)
  # text(x=5, y=(max(max.i)+0.5), labels=label.i[5], cex=0.8)
}

# f5
eval.stack.without.axis.x <- function(stack.i, zona.glaciologica, var.i){
  if(zona.glaciologica=='sur'){stack.j <- crop(stack.i, marco_zona_sur)} else(stack.j <- crop(stack.i, marco_zona_austral))
  if(var.i=='pp'){range.i <- c(min.pp, max.pp)} else(range.i <- c(min.temp, max.temp))
  names(stack.j) <- c('REFERENCE', 'IPSL', 'MIROC', 'CCSM4', 'CSIRO')
  
  mean.i <- cellStats(stack.j, stat = 'mean', na.rm=TRUE)  
  sd.i <- cellStats(stack.j, stat = 'sd', na.rm=TRUE)  
  min.i <- cellStats(stack.j, stat = 'min', na.rm=TRUE)
  max.i <- cellStats(stack.j, stat = 'max', na.rm=TRUE)
  
  # label.i <- paste('SD = ', round(sd.i, 2), sep = '') ; label.i
  
  boxplot(stack.j, xlab = '', xaxt='n', las=1, ylim=range.i)
  points(mean.i, col='red', pch=16)
  # text(x=1, y=(max(max.i)+0.5), labels=label.i[1], cex=0.8)
  # text(x=2, y=(max(max.i)+0.5), labels=label.i[2], cex=0.8)
  # text(x=3, y=(max(max.i)+0.5), labels=label.i[3], cex=0.8)
  # text(x=4, y=(max(max.i)+0.5), labels=label.i[4], cex=0.8)
  # text(x=5, y=(max(max.i)+0.5), labels=label.i[5], cex=0.8)
}

# f6
eval.stack.without.axis.y <- function(stack.i, zona.glaciologica, var.i){
  if(zona.glaciologica=='sur'){stack.j <- crop(stack.i, marco_zona_sur)} else(stack.j <- crop(stack.i, marco_zona_austral))
  if(var.i=='pp'){range.i <- c(min.pp, max.pp)} else(range.i <- c(min.temp, max.temp))
  names(stack.j) <- c('REFERENCE', 'IPSL', 'MIROC', 'CCSM4', 'CSIRO')
  
  mean.i <- cellStats(stack.j, stat = 'mean', na.rm=TRUE)  
  sd.i <- cellStats(stack.j, stat = 'sd', na.rm=TRUE)  
  min.i <- cellStats(stack.j, stat = 'min', na.rm=TRUE)
  max.i <- cellStats(stack.j, stat = 'max', na.rm=TRUE)
  
 # label.i <- paste('SD = ', round(sd.i, 2), sep = '') ; label.i
  
  boxplot(stack.j, ylab = '', xlab = '', xaxt='n', yaxt='n', ylim=range.i)
  points(mean.i, col='red', pch=16)
  # text(x=1, y=(max(max.i)+0.5), labels=label.i[1], cex=0.8)
  # text(x=2, y=(max(max.i)+0.5), labels=label.i[2], cex=0.8)
  # text(x=3, y=(max(max.i)+0.5), labels=label.i[3], cex=0.8)
  # text(x=4, y=(max(max.i)+0.5), labels=label.i[4], cex=0.8)
  # text(x=5, y=(max(max.i)+0.5), labels=label.i[5], cex=0.8)
}

# f7
eval.stack.without.axis.y2 <- function(stack.i, zona.glaciologica, var.i){
  if(zona.glaciologica=='sur'){stack.j <- crop(stack.i, marco_zona_sur)} else(stack.j <- crop(stack.i, marco_zona_austral))
  if(var.i=='pp'){range.i <- c(min.pp, max.pp)} else(range.i <- c(min.temp, max.temp))
  names(stack.j) <- c('REFERENCE', 'IPSL', 'MIROC', 'CCSM4', 'CSIRO')
  
  mean.i <- cellStats(stack.j, stat = 'mean', na.rm=TRUE)  
  sd.i <- cellStats(stack.j, stat = 'sd', na.rm=TRUE)  
  min.i <- cellStats(stack.j, stat = 'min', na.rm=TRUE)
  max.i <- cellStats(stack.j, stat = 'max', na.rm=TRUE)
  
  # label.i <- paste('SD = ', round(sd.i, 2), sep = '') ; label.i
  
  boxplot(stack.j, ylab = '', xlab = '', yaxt='n', ylim=range.i)
  points(mean.i, col='red', pch=16)
  # text(x=1, y=(max(max.i)+0.5), labels=label.i[1], cex=0.8)
  # text(x=2, y=(max(max.i)+0.5), labels=label.i[2], cex=0.8)
  # text(x=3, y=(max(max.i)+0.5), labels=label.i[3], cex=0.8)
  # text(x=4, y=(max(max.i)+0.5), labels=label.i[4], cex=0.8)
  # text(x=5, y=(max(max.i)+0.5), labels=label.i[5], cex=0.8)
}

# f8
axes.map <- function(l.i, raster.i){
  if(l.i=='lon'){l.j <- 1:2} else(l.j <- 3:4)
  if(l.i=='lon'){n.i <- 1} else(n.i <- 1)
  if(l.i=='lon'){sufijo <- '°W'} else(sufijo <- '°S')  
  l <- extent(raster.i)[l.j]
  l.min <- min(l)
  l.max <- max(l)
  l.min.j <- round(l.min, 0)-1
  l.max.j <- round(l.max, 0)+1
  seq.l.pre <- seq(l.min.j, l.max.j, by=n.i)
  seq.l <- paste( str_sub(seq.l.pre, 2, 3), sufijo, sep = '' )
  out.i <- data.frame(value=seq.l.pre, sufijo=seq.l)
  
  return(out.i)
}

# f9
db.boxplot <- function(stack.i, anho.i){
  db.stack.i.ref <- as.data.frame(stack.i[[1]], na.rm=TRUE)
  db.stack.i.ref$GCM <- 'REFERENCE'
  names(db.stack.i.ref) <- c('VALOR', 'GCM')
  
  db.stack.i.ipsl <- as.data.frame(stack.i[[2]], na.rm=TRUE)
  db.stack.i.ipsl$GCM <- 'IPSL'
  names(db.stack.i.ipsl) <- c('VALOR', 'GCM')
  
  db.stack.i.miroc <- as.data.frame(stack.i[[3]], na.rm=TRUE)
  db.stack.i.miroc$GCM <- 'MIROC'
  names(db.stack.i.miroc) <- c('VALOR', 'GCM')
  
  db.stack.i.ccsm4 <- as.data.frame(stack.i[[4]], na.rm=TRUE)
  db.stack.i.ccsm4$GCM <- 'CCSM4'
  names(db.stack.i.ccsm4) <- c('VALOR', 'GCM')
  
  db.stack.i.csiro <- as.data.frame(stack.i[[5]], na.rm=TRUE)
  db.stack.i.csiro$GCM <- 'CSIRO'
  names(db.stack.i.csiro) <- c('VALOR', 'GCM')
  
  db <- rbind(db.stack.i.ref, db.stack.i.ipsl, db.stack.i.miroc, db.stack.i.ccsm4, db.stack.i.csiro)
  db$anho <- anho.i
  return(db)
}

# f10
GCMs.referencia <- function(name.model, zona.glaciologica){
  #name.model <- 'CCSM4'
  setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/historico_1980_2005/')
  r.names <- grep(pattern = name.model, dir(), value = T)

  for (i in 1:length(r.names)) {
    #i <- 2
    if(i==1){stack.i <- stack(r.names[i])} else(stack.i <- stack(stack.i, r.names[i]))
  }
  
  stack.pre.out <- mean(stack.i)
  stack.pre2.out <- stack.pre.out-273.15
  names(stack.pre2.out) <- name.model
  
  if(zona.glaciologica=='sur'){var.amb <- crop(stack.pre2.out, marco_zona_sur)} else(var.amb <- crop(stack.pre2.out, marco_zona_austral))
  var.amb2 <- resample(var.amb, referencia[[2]], method='bilinear')
  var.amb.out <- mask(var.amb2, referencia[[2]])
  
  return(var.amb.out)
  }

# fin ---





# lectura coberturas (2010)----

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
marco_zona_sur <- readOGR('.', 'polygon_zona_glaciologica_sur_geo')
marco_zona_austral <- readOGR('.', 'polygon_zona_glaciologica_austral_geo')

zona.glaciologica <- 'sur'

# 2050 
rcp.50 <- 'rcp85'
anho.50 <- 2050

ipsl.50 <- var.futuro('IPSL', rcp.50, anho.50)
miroc.50 <- var.futuro('MIROC', rcp.50, anho.50)
csiro.50 <- var.futuro('CSIRO', rcp.50, anho.50)
ccsm4.50 <- var.futuro('CCSM4', rcp.50, anho.50)
#plot(ccsm4.50)

# 2070
rcp.70 <- 'rcp85'
anho.70 <- 2070

ipsl.70 <- var.futuro('IPSL', rcp.70, anho.70)
miroc.70 <- var.futuro('MIROC', rcp.70, anho.70)
csiro.70 <- var.futuro('CSIRO', rcp.70, anho.70)
ccsm4.70 <- var.futuro('CCSM4', rcp.70, anho.70)
#plot(ccsm4.70)

referencia <- var.referencia(zona.glaciologica)
# plot(referencia)
# plot(referencia.2005)

# fin ---










# lectura coberturas (2005)----

ipsl.2005.sur <- GCMs.referencia('IPSL', 'sur')
miroc.2005.sur <- GCMs.referencia('MIROC', 'sur')
csiro.2005.sur <- GCMs.referencia('CSIRO', 'sur')
ccsm4.2005.sur <- GCMs.referencia('CCSM4', 'sur')

ipsl.2005.austral <- GCMs.referencia('IPSL', 'austral')
miroc.2005.austral <- GCMs.referencia('MIROC', 'austral')
csiro.2005.austral <- GCMs.referencia('CSIRO', 'austral')
ccsm4.2005.austral <- GCMs.referencia('CCSM4', 'austral')

referencia.2005.sur <- var.referencia.2005('sur')
referencia.2005.austral <- var.referencia.2005('austral')

# fin ---










# variables climaticas ---

# mayor a menor sensibilidad a CO2 (2010)
tmax.50 <- stack(referencia[[2]], ipsl.50[[2]], miroc.50[[2]], ccsm4.50[[2]], csiro.50[[2]])
round(cellStats(tmax.50, stat = 'mean', na.rm=TRUE), 2)
round(cellStats(tmax.50, stat = 'sd', na.rm=TRUE), 2)

tmax.70 <- stack(referencia[[2]], ipsl.70[[2]], miroc.70[[2]], ccsm4.70[[2]], csiro.70[[2]])
round(cellStats(tmax.70, stat = 'mean', na.rm=TRUE), 2)
round(cellStats(tmax.70, stat = 'sd', na.rm=TRUE), 2)



# mayor a menor sensibilidad a CO2 (2005)
tmax.2005.sur <- stack(referencia.2005.sur, ipsl.2005.sur, miroc.2005.sur, ccsm4.2005.sur, csiro.2005.sur)
round(cellStats(tmax.2005.sur, stat = 'mean', na.rm=TRUE), 2)
round(cellStats(tmax.2005.sur, stat = 'sd', na.rm=TRUE), 2)

tmax.2005.austral <- stack(referencia.2005.austral, ipsl.2005.austral, miroc.2005.austral, ccsm4.2005.austral, csiro.2005.austral)
round(cellStats(tmax.2005.austral, stat = 'mean', na.rm=TRUE), 2)
round(cellStats(tmax.2005.austral, stat = 'sd', na.rm=TRUE), 2)



# mapa referencia 2010
ref <- as.data.frame(referencia[[2]], xy = TRUE) 
head(ref)
names(ref) <- c('x', 'y', 'temp')

map <- ggplot() +
  geom_raster(data = ref, aes(x = x, y = y, fill = temp)) +
  theme_classic() + 
  theme(legend.key.size = unit(1, "cm"), legend.position = 'bottom', 
        axis.text = element_text(face="bold", size=14),
        legend.text = element_text(size = 14)) +
  labs(tag = '\n', y = '', x = '') +
  coord_sf(crs=4326, expand = TRUE) +
  scale_fill_continuous('C°', low = 'yellow', high = 'red',
                        space = "Lab", na.value = "transparent", guide = "colourbar",
                        aesthetics = "fill") +
  annotation_scale(location = "bl", text_col = 'black', style = 'ticks', line_col = 'black',
                   text_cex = 1, height =  unit(0.35,"cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"))
map




# mapa referencia 2005
ref.2005 <- as.data.frame(referencia.2005, xy = TRUE) 
head(ref.2005)
names(ref.2005) <- c('x', 'y', 'temp')

map.2005 <- ggplot() +
  geom_raster(data = ref.2005, aes(x = x, y = y, fill = temp)) +
  theme_classic() + 
  theme(legend.key.size = unit(1, "cm"), legend.position = 'bottom', 
        axis.text = element_text(face="bold", size=14),
        legend.text = element_text(size = 14)) +
  labs(tag = '\n', y = '', x = '') +
  coord_sf(crs=4326, expand = TRUE) +
  scale_fill_continuous('C°', low = 'yellow', high = 'red',
                        space = "Lab", na.value = "transparent", guide = "colourbar",
                        aesthetics = "fill") +
  annotation_scale(location = "bl", text_col = 'black', style = 'ticks', line_col = 'black',
                   text_cex = 1, height =  unit(0.35,"cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"))
map.2005


# boxplot ---

db.50 <- db.boxplot(tmax.50, anho.50)
#head(db.50)

db.70 <- db.boxplot(tmax.70, anho.70)
#head(db.70)

dbf <- rbind(db.50, db.70)
#head(dbf)

p <- ggplot(dbf, aes(reorder(GCM, VALOR), VALOR)) + scale_y_continuous(position = "right")
out <- p + geom_boxplot() + 
  labs(tag = '\n', y = 'Tx summer (C°)', x = '') +
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  theme_bw() +
  theme(axis.text = element_text(size=14, color = 'black'), 
        axis.title.y = element_text(size=14, color = 'black')) +
  facet_wrap(~anho, nrow = 2) + 
  theme(strip.text.x = element_text(size=12, color="black"))

# output plot 

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "comparacion_GCMs_periodo_referencia.eps", height = 25/2.54, width = 30/2.54)  # Una figura en cm
# par(mar=c(4,4,0,0)+0.1)
# out
# dev.off()
# fin ---









# boxplot 2005 ---

db.2005.sur <- db.boxplot(tmax.2005.sur, 'South')
db.2005.austral <- db.boxplot(tmax.2005.austral, 'Austral')

dbf.2005 <- rbind(db.2005.sur, db.2005.austral)
dbf.2005$anho <- factor(dbf.2005$anho, levels = c('South', 'Austral'))

p <- ggplot(dbf.2005, aes(reorder(GCM, VALOR), VALOR))
out <- p + geom_boxplot() + 
  labs(y = 'Tx summer (C°)', x = '') +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  theme_bw() +
  theme(axis.text = element_text(size=10, color = 'black'), 
        axis.title.y = element_text(size=14, color = 'black')) +
  facet_wrap(~anho, ncol = 2) + 
  theme(strip.text.x = element_text(size=14, color="black"))
out

# output plot 

setwd('C:/Users/Usuario/OneDrive/plots_paper/')
setEPS()
postscript(file = "comparacion_GCMs_periodo_referencia.eps", height = 6.5, width = 8)  # Una figura en cm
par(mar=c(4,4,0,0)+0.1)
out
dev.off()
# fin ---












# otros ----
# # mayor a menor sensibilidad a CO2
# pp.50 <- stack(referencia[[1]], ipsl.50[[1]], miroc.50[[1]], ccsm4.50[[1]], csiro.50[[1]])
# tmax.50 <- stack(referencia[[2]], ipsl.50[[2]], miroc.50[[2]], ccsm4.50[[2]], csiro.50[[2]])
# tmin.50 <- stack(referencia[[3]], ipsl.50[[3]], miroc.50[[3]], ccsm4.50[[3]], csiro.50[[3]])
# 
# pp.70 <- stack(referencia[[1]], ipsl.70[[1]], miroc.70[[1]], ccsm4.70[[1]], csiro.70[[1]])
# tmax.70 <- stack(referencia[[2]], ipsl.70[[2]], miroc.70[[2]], ccsm4.70[[2]], csiro.70[[2]])
# tmin.70 <- stack(referencia[[3]], ipsl.70[[3]], miroc.70[[3]], ccsm4.70[[3]], csiro.70[[3]])

# max.temp <- max(maxValue(tmax.50), maxValue(tmax.70), maxValue(tmin.50), maxValue(tmin.70))
# min.temp <- min(minValue(tmax.50), minValue(tmax.70), minValue(tmin.50), minValue(tmin.70))
# 
# max.pp <- max(maxValue(pp.50), maxValue(pp.70))
# min.pp <- min(minValue(pp.50), minValue(pp.70))

# min.i <- round((min.temp), 0)-1
# max.i <- round((max.temp), 0)+1
# 
# breakpoints <- seq(min.i, max.i, by=4)
# l.breakpoints <- length(breakpoints)
# 
# col.temp <- colorRampPalette(colors = c('#F8FC00', '#FC0900'))

# # setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# # setEPS()
# # postscript(file = "comparacion_referencia_GCMs_rcp85_austral.eps")#, height = 20/2.54, width = 25/2.54)  # Una figura en cm
# 
# par(mfrow=c(3,2), oma=c(2,5,1,0), mai=c(0,0,0,0))
# 
# eval.stack.without.axis.x(tmax.70, zona.glaciologica, 'temp')
# mtext('A', side = 3, line = -1.5, adj=0.02, font = 2)
# mtext('Maximum temperature summer (C°)', side = 2, line = 3, cex = 0.7)
# 
# eval.stack.without.axis.y(tmax.70, zona.glaciologica, 'temp')
# mtext('B', side = 3, line = -1.5, adj=0.02, font = 2)
# 
# eval.stack.without.axis.x(tmin.50, zona.glaciologica, 'temp')
# mtext('C', side = 3, line = -1.5, adj=0.02, font = 2)
# mtext('Minimum temperature winter (C°)', side = 2, line = 3, cex = 0.7)
# 
# eval.stack.without.axis.y(tmin.70, zona.glaciologica, 'temp')
# mtext('D', side = 3, line = -1.5, adj=0.02, font = 2)
# 
# eval.stack(pp.50, zona.glaciologica, 'pp')
# mtext('E', side = 3, line = -1.5, adj=0.02, font = 2)
# mtext('Precipitation winter (mm)', side = 2, line = 3, cex = 0.7)
# 
# eval.stack.without.axis.y2(pp.70, zona.glaciologica, 'pp')
# mtext('F', side = 3, line = -1.5, adj=0.02, font = 2)
# 
# dev.off()

# # plots
# min.i <- round(min(minValue(tmin)), 0)-1
# max.i <- round(max(maxValue(tmax)), 0)+1
# 
# breakpoints <- seq(min.i, max.i, by=4) 
# l.breakpoints <- length(breakpoints)
# 
# col.temp <- colorRampPalette(colors = c('#F8FC00', '#FC0900'))

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "comparacion_referencia_GCMs_sur.eps", height = 20/2.54, width = 25/2.54)  # Una figura en cm
# #par(mar=c(4,4,0,0)+0.1)
# 
# #dev.off()
# layout(matrix(c(1,2,
#                 3,4,
#                 5,6), 3, 2, byrow=TRUE))
# 
# par(oma=c(2,0,0,0), mai=c(0,0.5,0,0))
# 
# lat.i <- axes.map('lat', referencia[[2]])
# lon.i <- axes.map('lon', referencia[[2]])
# label.y <- 'Temperatura maxima (C)'
# plot(referencia[[2]], breaks=breakpoints, col = col.temp(l.breakpoints), axes=FALSE)
# axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 1, las = 1) 
# addscalebar(label.col = 'black', style = "ticks") 
# addnortharrow(pos = "topleft", cols = c("black", "white"), border = 'black', text.col = 'black', scale = 0.7) 
# mtext('A)', side = 3, line = -1.5, adj=0.98, font = 2)
# 
# eval.stack.without.axis.x(tmax, zona.glaciologica.i)
# mtext('B)', side = 3, line = -1.5, adj=0.98, font = 2)
# 
# 
# lat.i <- axes.map('lat', referencia[[3]])
# lon.i <- axes.map('lon', referencia[[3]])
# label.y <- 'Temperatura minima (C)'
# plot(referencia[[3]], breaks=breakpoints,col = col.temp(l.breakpoints), axes=FALSE)
# axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 1, las = 1) 
# addscalebar(label.col = 'black', style = "ticks") 
# addnortharrow(pos = "topleft", cols = c("black", "white"), border = 'black', text.col = 'black', scale = 0.7) 
# mtext('C)', side = 3, line = -1.5, adj=0.98, font = 2)
# 
# eval.stack.without.axis.x(tmin, zona.glaciologica.i)
# mtext('D)', side = 3, line = -1.5, adj=0.98, font = 2)
# 
# 
# lat.i <- axes.map('lat', referencia[[1]])
# lon.i <- axes.map('lon', referencia[[1]])
# label.y <- 'Precipitacion (mm)'
# plot(referencia[[1]], col=colours.pp(10), axes=FALSE)
# axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 1, las = 1) 
# axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 1, las = 1) 
# addscalebar(label.col = 'black', style = "ticks") 
# addnortharrow(pos = "topleft", cols = c("black", "white"), border = 'black', text.col = 'black', scale = 0.7) 
# mtext('E)', side = 3, line = -1.5, adj=0.98, font = 2)
# 
# eval.stack(pp, zona.glaciologica.i)
# mtext('F)', side = 3, line = -1.5, adj=0.98, font = 2)
# 
# dev.off()


# dev.off()
# layout(matrix(c(1,2,
#                 1,3,
#                 1,4), 3, 2, byrow=TRUE))
# par(oma=c(2,0,0,0), mai=c(0,0.7,0,0))
# eval.stack(pp, zona.glaciologica.i)
# mtext('A)', side = 3, line = -1.5, adj=0.98, font = 2)
# eval.stack.without.axis.x(tmax, zona.glaciologica.i)
# mtext('B)', side = 3, line = -1.5, adj=0.98, font = 2)
# eval.stack.without.axis.x(tmin, zona.glaciologica.i)
# mtext('C)', side = 3, line = -1.5, adj=0.98, font = 2)
# eval.stack(pp, zona.glaciologica.i)
# mtext('D)', side = 3, line = -1.5, adj=0.98, font = 2)
