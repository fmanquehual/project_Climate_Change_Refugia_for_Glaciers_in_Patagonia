library(ggplot2)
library(rgdal)
library(raster)
library(rasterVis)
library(cowplot)
library(ggspatial)
library(graticule)
library(corrplot)
library(ggplotify)
library(grid)

rm(list=ls())
dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84


# funciones ----

# f1
raster.to.db <- function(raster.i){
  r1 <- as.vector(as.matrix(raster.i))
  r2 <- na.omit(r1)
  db.r <- data.frame(values=r2)
  return(db.r)
}

# f2
db_prob_elev <- function(predict.i, raster1, raster2, anho.rcp.i, zone.i){
  
  r.i <- mask(raster1, predict.i)
  db.r.i <- raster.to.db(r.i)
  
  r.j <- crop(raster2, r.i)
  r.k <- mask(r.j, r.i)
  
  db.r.k <- raster.to.db(r.k)
  out <- data.frame(p = db.r.i$values, elevacion = db.r.k$values, cat = anho.rcp.i, zona = zone.i)
  
  return(out)
}

# fin ---






# lectura coberturas ----

setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/Glaciares_Nacional/")
ref <- readOGR('.', 'polygon_glaciares_marco_trabajo_nuevo_simplificado_geo')
#plot(ref)


# predicciones 

setwd('C:/Users/Usuario/Documents/Francisco/predicciones_maxent/')

r01 <- raster('prediccion_periodo_referencia_sur.tif')
r02 <- raster('prediccion_periodo_referencia_austral.tif')
r0 <- merge(r01, r02)

r1 <- raster('prediccion_csiro_2050_rcp45_zona_sur.tif')
r2 <- raster('prediccion_csiro_2050_rcp45_zona_austral.tif')
r12 <- merge(r1, r2)

r3 <- raster('prediccion_csiro_2050_rcp85_zona_sur.tif')
r4 <- raster('prediccion_csiro_2050_rcp85_zona_austral.tif')
r34 <- merge(r3, r4)

r5 <- raster('prediccion_csiro_2070_rcp45_zona_sur.tif')
r6 <- raster('prediccion_csiro_2070_rcp45_zona_austral.tif')
r56 <- merge(r5, r6)

r7 <- raster('prediccion_csiro_2070_rcp85_zona_sur.tif')
r8 <- raster('prediccion_csiro_2070_rcp85_zona_austral.tif')
r78 <- merge(r7, r8)

all <- stack(r0, r12, r34, r56, r78)
#plot(all)

# fin ---





# Mapas con graficos de densidad ----

myPal <- colorRampPalette(c('#E7E4E4', '#CAC8C8', '#315669', '#181C4D'))(1000)
myTheme <- rasterTheme(region = myPal)

my.at <- seq(0, 1, by=0.01) # breaks para la leyenda
prob.max <- 0.8 # ymax para el eje 'y' de los graficos de densidad

lons <- pretty(c(-76, -70)) # para la grilla de coordenadas
lats <- pretty(c(-49, -41), n=10) ; lats
grat <- graticule(lons, lats, proj = wgs84,
                  xlim = range(lons), ylim = range(lats))

FUN.i <- function(x){
  y <- quantile(x, probs=c(0.75))
  out <- as.vector(y)
  return(out)
}
  


p1 <- levelplot(all, layers=1, margin = list(FUN = FUN.i, axis = gpar(col = 'black', fontsize = 10.5), 
                                             scales = list(x=c(0, prob.max),
                                                           y=c(0, prob.max))), 
                par.settings=myTheme, colorkey=FALSE, xlab = '', ylab = '',
                at=my.at) + layer(sp.lines(grat, lty=2, col = '#777878'))

p2 <- levelplot(all, layers=2, margin = list(FUN = FUN.i, axis = gpar(col = 'black', fontsize = 10.5), 
                                             scales = list(x=c(0, prob.max),
                                                           y=c(0, prob.max))), 
                par.settings=myTheme, colorkey = FALSE, xlab = '', ylab = '',
                at=my.at) + layer(sp.lines(grat, lty=2, col = '#777878'))

p3 <- levelplot(all, layers=3, margin = list(FUN = FUN.i, axis = gpar(col = 'black', fontsize = 10.5), 
                                             scales = list(x=c(0, prob.max),
                                                           y=c(0, prob.max))), 
                par.settings=myTheme, colorkey = FALSE, xlab = '', ylab = '',
                at=my.at) + layer(sp.lines(grat, lty=2, col = '#777878'))

p4 <- levelplot(all, layers=4, margin = list(FUN = FUN.i, axis = gpar(col = 'black', fontsize = 10.5), 
                                             scales = list(x=c(0, prob.max),
                                                           y=c(0, prob.max))), 
                par.settings=myTheme, colorkey = FALSE, xlab = '', ylab = '',
                at=my.at) + layer(sp.lines(grat, lty=2, col = '#777878'))

p5 <- levelplot(all, layers=5, margin = list(FUN = FUN.i, axis = gpar(col = 'black', fontsize = 10.5), 
                                             scales = list(x=c(0, prob.max),
                                                           y=c(0, prob.max))), 
                par.settings=myTheme, colorkey = FALSE, xlab = '', ylab = '',
                at=my.at) + layer(sp.lines(grat, lty=2, col = '#777878'))

scale.i <- 1.2 # variacion en el tamaño del mapa
p.group <- plot_grid(p1, p2, p3,
                     p1, p4, p5,
                     labels=c('A', 'B', 'C',
                              'A', 'D', 'E'), ncol = 3, nrow = 2, 
                     scale = c(scale.i, scale.i, scale.i, scale.i, scale.i, scale.i))
# p.out <- plot_grid(p1, p.group,
#                    labels=c('A', ''), ncol = 2, nrow = 1)

leyenda0 <- as.grob(~c(
  plot(0,xlim = c(0,1), ylim = c(0,1), type ='n', axes=FALSE,#xaxt='n', yaxt='n',
       ylab='', xlab=''),
  colorlegend(myPal, seq(0, 1, 0.1), vertical = FALSE, align = 'c',
              xlim = c(0, 0.8), ylim = c(0,1)),
  text(0.4,-0.3, 'Probability of Climate Change Refugia', srt = 0, font = 2)
))

leyenda <- ggdraw(leyenda0)


setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# jpeg('predic_MAXENT_CSIRO_con_graficos_de_densidad.jpg', width = 1290, height = 1400, units = "px", pointsize = 12,
#      quality = 100, type = 'cairo', res = 110)
tiff('predic_MAXENT_CSIRO_con_graficos_de_densidad_300dpi.tiff',width=12,height=12,units="in",res=300)

plot_grid(p.group, leyenda,
          ncol = 1, nrow = 2,
          scale = c(1, 0.8),
          rel_heights = c(9,1),
          rel_widths = c(1,1))
dev.off()

# fin ---





# elevacion ----

# Recorte del dem hecho en Qgis!
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/topografico/')
dem <- raster('clip_dem_res_30m_nueva_area_estudio_geo.tif')
#plot(dem)

# fin ---




# Regresion periodo referencia ----

# Sur
db.ref.sur <- db_prob_elev(ref, r01, dem, 'Reference', 'South')

# lm
lm.ref.sur <- lm(elevacion~p, data = db.ref.sur)
summary(lm.ref.sur)

b0.ref.sur <- coef(lm.ref.sur)[1] ; b0.ref.sur
b1.ref.sur <- coef(lm.ref.sur)[2] ; b1.ref.sur
valores.ref.sur <- seq(0, max(db.ref.sur$p), by=0.001)
valores.ajustados.ref.sur <- (b1.ref.sur*valores.ref.sur)+b0.ref.sur

db.ref.sur$intercepto <- round(b0.ref.sur, 1)
db.ref.sur$b1 <- round(b1.ref.sur, 1)
head(db.ref.sur)  

plot(db.ref.sur$p, db.ref.sur$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.ref.sur, valores.ajustados.ref.sur, col='red')


# Austral
db.ref.austral <- db_prob_elev(ref, r02, dem, 'Reference', 'Austral')

# lm
lm.ref.austral <- lm(elevacion~p, data = db.ref.austral)
summary(lm.ref.austral)

b0.ref.austral <- coef(lm.ref.austral)[1] ; b0.ref.austral
b1.ref.austral <- coef(lm.ref.austral)[2] ; b1.ref.austral
valores.ref.austral <- seq(0, max(db.ref.austral$p), by=0.001)
valores.ajustados.ref.austral <- (b1.ref.austral*valores.ref.austral)+b0.ref.austral

db.ref.austral$intercepto <- round(b0.ref.austral, 1)
db.ref.austral$b1 <- round(b1.ref.austral, 1)
head(db.ref.austral)  

plot(db.ref.austral$p, db.ref.austral$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.ref.austral, valores.ajustados.ref.austral, col='red')

# fin ---




# Regresion periodo referencia 2 ----

# Sur
db.ref.sur.2 <- db_prob_elev(ref, r01, dem, 'Reference ', 'South')

# lm
lm.ref.sur <- lm(elevacion~p, data = db.ref.sur.2)
summary(lm.ref.sur)

b0.ref.sur <- coef(lm.ref.sur)[1] ; b0.ref.sur
b1.ref.sur <- coef(lm.ref.sur)[2] ; b1.ref.sur
valores.ref.sur <- seq(0, max(db.ref.sur.2$p), by=0.001)
valores.ajustados.ref.sur <- (b1.ref.sur*valores.ref.sur)+b0.ref.sur

db.ref.sur.2$intercepto <- round(b0.ref.sur, 1)
db.ref.sur.2$b1 <- round(b1.ref.sur, 1)
head(db.ref.sur.2)  

plot(db.ref.sur.2$p, db.ref.sur.2$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.ref.sur, valores.ajustados.ref.sur, col='red')


# Austral
db.ref.austral.2 <- db_prob_elev(ref, r02, dem, 'Reference ', 'Austral')

# lm
lm.ref.austral <- lm(elevacion~p, data = db.ref.austral.2)
summary(lm.ref.austral)

b0.ref.austral <- coef(lm.ref.austral)[1] ; b0.ref.austral
b1.ref.austral <- coef(lm.ref.austral)[2] ; b1.ref.austral
valores.ref.austral <- seq(0, max(db.ref.austral.2$p), by=0.001)
valores.ajustados.ref.austral <- (b1.ref.austral*valores.ref.austral)+b0.ref.austral

db.ref.austral.2$intercepto <- round(b0.ref.austral, 1)
db.ref.austral.2$b1 <- round(b1.ref.austral, 1)
head(db.ref.austral.2)  

plot(db.ref.austral.2$p, db.ref.austral.2$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.ref.austral, valores.ajustados.ref.austral, col='red')

# fin ---




# Regresion 2050 RCP45 ---

# Sur
rcp45.2050.sur <- db_prob_elev(ref, r1, dem, '2050 RCP 4.5', 'South')

# lm
lm.rcp45.2050.sur <- lm(elevacion~p, data = rcp45.2050.sur)
summary(lm.rcp45.2050.sur)

b0.rcp45.2050.sur <- coef(lm.rcp45.2050.sur)[1] ; b0.rcp45.2050.sur
b1.rcp45.2050.sur <- coef(lm.rcp45.2050.sur)[2] ; b1.rcp45.2050.sur
valores.rcp45.2050.sur <- seq(0, max(rcp45.2050.sur$p), by=0.001)
valores.ajustados.rcp45.2050.sur <- (b1.rcp45.2050.sur*valores.rcp45.2050.sur)+b0.rcp45.2050.sur

rcp45.2050.sur$intercepto <- round(b0.rcp45.2050.sur, 1)
rcp45.2050.sur$b1 <- round(b1.rcp45.2050.sur, 1)
head(rcp45.2050.sur)  

plot(rcp45.2050.sur$p, rcp45.2050.sur$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.rcp45.2050.sur, valores.ajustados.rcp45.2050.sur, col='red')


# Austral
rcp45.2050.austral <- db_prob_elev(ref, r2, dem, '2050 RCP 4.5', 'Austral')

# lm
lm.rcp45.2050.austral <- lm(elevacion~p, data = rcp45.2050.austral)
summary(lm.rcp45.2050.austral)

b0.rcp45.2050.austral <- coef(lm.rcp45.2050.austral)[1] ; b0.rcp45.2050.austral
b1.rcp45.2050.austral <- coef(lm.rcp45.2050.austral)[2] ; b1.rcp45.2050.austral
valores.rcp45.2050.austral <- seq(0, max(rcp45.2050.austral$p), by=0.001)
valores.ajustados.rcp45.2050.austral <- (b1.rcp45.2050.austral*valores.rcp45.2050.austral)+b0.rcp45.2050.austral

rcp45.2050.austral$intercepto <- round(b0.rcp45.2050.austral, 1)
rcp45.2050.austral$b1 <- round(b1.rcp45.2050.austral, 1)
head(rcp45.2050.austral)  

plot(rcp45.2050.austral$p, rcp45.2050.austral$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.rcp45.2050.austral, valores.ajustados.rcp45.2050.austral, col='red')

# fin ---




# Regresion 2050 RCP85 ---

# Sur
rcp85.2050.sur <- db_prob_elev(ref, r3, dem, '2050 RCP 8.5', 'South')

# lm
lm.rcp85.2050.sur <- lm(elevacion~p, data = rcp85.2050.sur)
summary(lm.rcp85.2050.sur)

b0.rcp85.2050.sur <- coef(lm.rcp85.2050.sur)[1] ; b0.rcp85.2050.sur
b1.rcp85.2050.sur <- coef(lm.rcp85.2050.sur)[2] ; b1.rcp85.2050.sur
valores.rcp85.2050.sur <- seq(0, max(rcp85.2050.sur$p), by=0.001)
valores.ajustados.rcp85.2050.sur <- (b1.rcp85.2050.sur*valores.rcp85.2050.sur)+b0.rcp85.2050.sur

rcp85.2050.sur$intercepto <- round(b0.rcp85.2050.sur, 1)
rcp85.2050.sur$b1 <- round(b1.rcp85.2050.sur, 1)
head(rcp85.2050.sur)  

plot(rcp85.2050.sur$p, rcp85.2050.sur$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.rcp85.2050.sur, valores.ajustados.rcp85.2050.sur, col='red')


# Austral
rcp85.2050.austral <- db_prob_elev(ref, r4, dem, '2050 RCP 8.5', 'Austral')

# lm
lm.rcp85.2050.austral <- lm(elevacion~p, data = rcp85.2050.austral)
summary(lm.rcp85.2050.austral)

b0.rcp85.2050.austral <- coef(lm.rcp85.2050.austral)[1] ; b0.rcp85.2050.austral
b1.rcp85.2050.austral <- coef(lm.rcp85.2050.austral)[2] ; b1.rcp85.2050.austral
valores.rcp85.2050.austral <- seq(0, max(rcp85.2050.austral$p), by=0.001)
valores.ajustados.rcp85.2050.austral <- (b1.rcp85.2050.austral*valores.rcp85.2050.austral)+b0.rcp85.2050.austral

rcp85.2050.austral$intercepto <- round(b0.rcp85.2050.austral, 1)
rcp85.2050.austral$b1 <- round(b1.rcp85.2050.austral, 1)
head(rcp85.2050.austral)  

plot(rcp85.2050.austral$p, rcp85.2050.austral$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.rcp85.2050.austral, valores.ajustados.rcp85.2050.austral, col='red')

# fin ---




# Regresion 2070 RCP45 ---

# Sur
rcp45.2070.sur <- db_prob_elev(ref, r5, dem, '2070 RCP 4.5', 'South')

# lm
lm.rcp45.2070.sur <- lm(elevacion~p, data = rcp45.2070.sur)
summary(lm.rcp45.2070.sur)

b0.rcp45.2070.sur <- coef(lm.rcp45.2070.sur)[1] ; b0.rcp45.2070.sur
b1.rcp45.2070.sur <- coef(lm.rcp45.2070.sur)[2] ; b1.rcp45.2070.sur
valores.rcp45.2070.sur <- seq(0, max(rcp45.2070.sur$p), by=0.001)
valores.ajustados.rcp45.2070.sur <- (b1.rcp45.2070.sur*valores.rcp45.2070.sur)+b0.rcp45.2070.sur

rcp45.2070.sur$intercepto <- round(b0.rcp45.2070.sur, 1)
rcp45.2070.sur$b1 <- round(b1.rcp45.2070.sur, 1)
head(rcp45.2070.sur)  

plot(rcp45.2070.sur$p, rcp45.2070.sur$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.rcp45.2070.sur, valores.ajustados.rcp45.2070.sur, col='red')


# Austral
rcp45.2070.austral <- db_prob_elev(ref, r6, dem, '2070 RCP 4.5', 'Austral')

# lm
lm.rcp45.2070.austral <- lm(elevacion~p, data = rcp45.2070.austral)
summary(lm.rcp45.2070.austral)

b0.rcp45.2070.austral <- coef(lm.rcp45.2070.austral)[1] ; b0.rcp45.2070.austral
b1.rcp45.2070.austral <- coef(lm.rcp45.2070.austral)[2] ; b1.rcp45.2070.austral
valores.rcp45.2070.austral <- seq(0, max(rcp45.2070.austral$p), by=0.001)
valores.ajustados.rcp45.2070.austral <- (b1.rcp45.2070.austral*valores.rcp45.2070.austral)+b0.rcp45.2070.austral

rcp45.2070.austral$intercepto <- round(b0.rcp45.2070.austral, 1)
rcp45.2070.austral$b1 <- round(b1.rcp45.2070.austral, 1)
head(rcp45.2070.austral)  

plot(rcp45.2070.austral$p, rcp45.2070.austral$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.rcp45.2070.austral, valores.ajustados.rcp45.2070.austral, col='red')

# fin ---




# Regresion 2070 RCP85 ---

# Sur
rcp85.2070.sur <- db_prob_elev(ref, r7, dem, '2070 RCP 8.5', 'South')

# lm
lm.rcp85.2070.sur <- lm(elevacion~p, data = rcp85.2070.sur)
summary(lm.rcp85.2070.sur)

b0.rcp85.2070.sur <- coef(lm.rcp85.2070.sur)[1] ; b0.rcp85.2070.sur
b1.rcp85.2070.sur <- coef(lm.rcp85.2070.sur)[2] ; b1.rcp85.2070.sur
valores.rcp85.2070.sur <- seq(0, max(rcp85.2070.sur$p), by=0.001)
valores.ajustados.rcp85.2070.sur <- (b1.rcp85.2070.sur*valores.rcp85.2070.sur)+b0.rcp85.2070.sur

rcp85.2070.sur$intercepto <- round(b0.rcp85.2070.sur, 1)
rcp85.2070.sur$b1 <- round(b1.rcp85.2070.sur, 1)
head(rcp85.2070.sur)  

plot(rcp85.2070.sur$p, rcp85.2070.sur$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.rcp85.2070.sur, valores.ajustados.rcp85.2070.sur, col='red')


# Austral
rcp85.2070.austral <- db_prob_elev(ref, r8, dem, '2070 RCP 8.5', 'Austral')

# lm
lm.rcp85.2070.austral <- lm(elevacion~p, data = rcp85.2070.austral)
summary(lm.rcp85.2070.austral)

b0.rcp85.2070.austral <- coef(lm.rcp85.2070.austral)[1] ; b0.rcp85.2070.austral
b1.rcp85.2070.austral <- coef(lm.rcp85.2070.austral)[2] ; b1.rcp85.2070.austral
valores.rcp85.2070.austral <- seq(0, max(rcp85.2070.austral$p), by=0.001)
valores.ajustados.rcp85.2070.austral <- (b1.rcp85.2070.austral*valores.rcp85.2070.austral)+b0.rcp85.2070.austral

rcp85.2070.austral$intercepto <- round(b0.rcp85.2070.austral, 1)
rcp85.2070.austral$b1 <- round(b1.rcp85.2070.austral, 1)
head(rcp85.2070.austral)  

plot(rcp85.2070.austral$p, rcp85.2070.austral$elevacion, xlab='Probability', ylab='Altitude')
lines(valores.rcp85.2070.austral, valores.ajustados.rcp85.2070.austral, col='red')

# fin ---




# union dbs ----
db.sur <- rbind(db.ref.sur, rcp45.2050.sur, rcp45.2070.sur, 
                db.ref.sur.2, rcp85.2050.sur, rcp85.2070.sur)

db.sur$cat <- factor(db.sur$cat, levels = unique(db.sur$cat))
levels(db.sur$cat)

db.austral <- rbind(db.ref.austral, rcp45.2050.austral, rcp45.2070.austral, 
                    db.ref.austral.2, rcp85.2050.austral, rcp85.2070.austral)

db.austral$cat <- factor(db.austral$cat, levels = unique(db.austral$cat))
levels(db.austral$cat)

# fin ---




# Graficos densidad 2D Z.G.Sur ----

intercepto.sur <- as.vector(tapply(db.sur$intercepto, db.sur$cat, unique))
etiqueta.intercepto <- paste('Intercept of trend line = ', intercepto.sur, sep = '')

b1.sur <- as.vector(tapply(db.sur$b1, db.sur$cat, unique))
etiqueta.b1 <- paste('Slope of trend line = ', b1.sur, sep = '')

nombres.sur <- names(tapply(db.sur$intercepto, db.sur$cat, unique))


etiqueta.intercepto.sur <- data.frame(
  label = etiqueta.intercepto,
  cat   = nombres.sur,
  x     = rep(0.8, length(intercepto.sur)),
  y     = rep(500, length(intercepto.sur))
)

etiqueta.b1.sur <- data.frame(
  label = etiqueta.b1,
  cat   = nombres.sur,
  x     = rep(0.8, length(intercepto.sur)),
  y     = rep(500, length(intercepto.sur))
)

setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setwd('C:/Users/Usuario/Desktop/')
# jpeg('densidad_de_probabilidad_2D_elevacion_sur.jpg', width = 2200, height = 1400, units = "px", pointsize = 12,
#      quality = 100, type = 'cairo', res = 210)
# tiff('densidad_de_probabilidad_2D_elevacion_sur_300dpi.tiff',width=11,height=7,units="in",res=300)

ggplot(db.sur, aes(x=p, y=elevacion) ) +
  geom_hex(bins = 70) +
  lims(x=c(0,1)) +
  labs(x = 'Probability', y = 'Altitude') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  facet_wrap(~factor(cat), ncol = 3) +
  scale_fill_gradientn('Absolute\nfrequency', colours = c('#103C5D', '#2683C8', '#03FBFF'), 
                       na.value = NA) +
  scale_y_continuous(limits = c(0, 2500)) +
  geom_text(
    data    = etiqueta.intercepto.sur,
    mapping = aes(x = 0.13, y = -Inf, label = label[1:6]),
    check_overlap = TRUE,
    hjust   = -0.065,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  geom_text(
    data    = etiqueta.b1.sur,
    mapping = aes(x = 0.225, y = 100, label = label[1:6]),
    check_overlap = TRUE,
    hjust   = -0.05,
    vjust   = -1,
    inherit.aes=FALSE
  ) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"))

dev.off()

# fin ---



# Graficos densidad 2D Z.G.Austral ----

intercepto.austral <- as.vector(tapply(db.austral$intercepto, db.austral$cat, unique))
etiqueta.intercepto <- paste('Intercept of trend line = ', intercepto.austral, sep = '')

b1.austral <- as.vector(tapply(db.austral$b1, db.austral$cat, unique))
etiqueta.b1 <- paste('Slope of trend line = ', b1.austral, sep = '')

nombres.austral <- names(tapply(db.austral$intercepto, db.austral$cat, unique))
nombres.austral <- factor(nombres.austral, levels = nombres.austral)

etiqueta.intercepto.austral <- data.frame(
  label = etiqueta.intercepto,
  cat   = nombres.austral,
  x     = rep(0.8, length(intercepto.austral)),
  y     = rep(500, length(intercepto.austral))
)

etiqueta.b1.austral <- data.frame(
  label = etiqueta.b1,
  cat   = nombres.austral,
  x     = rep(0.8, length(intercepto.austral)),
  y     = rep(500, length(intercepto.austral))
)

setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# jpeg('densidad_de_probabilidad_2D_elevacion_austral.jpg', width = 2550, height = 1550, units = "px", pointsize = 12,
#      quality = 100, type = 'cairo', res = 220)
# tiff('densidad_de_probabilidad_2D_elevacion_austral_300dpi.tiff',width=12,height=7,units="in",res=300)

ggplot(db.austral, aes(x=p, y=elevacion) ) +
  geom_hex(bins = 70) +
  lims(x=c(0,1)) +
  labs(x = 'Probability', y = 'Altitude') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  facet_wrap(~factor(cat), ncol = 3) +
  scale_fill_gradientn('Absolute\nfrequency', colours = c('#103C5D', '#2683C8', '#03FBFF'), na.value = NA) +
  scale_y_continuous(limits = c(-100, 4000)) +
  geom_text(
    data    = etiqueta.intercepto.austral,
    mapping = aes(x = 0.296, y = -Inf, label = label[1:6]),
    check_overlap = TRUE,
    hjust   = 0,
    vjust   = -1, 
    inherit.aes=FALSE
  ) +
  geom_text(
    data    = etiqueta.b1.austral,
    mapping = aes(x = 0.37, y = -30, label = label[1:6]),
    check_overlap = TRUE,
    hjust   = 0,
    vjust   = -1, 
    inherit.aes=FALSE
  ) +
  theme_bw() +
  theme(text = element_text(size=14), panel.spacing = unit(1, "lines"))

dev.off()

# fin ---





# otros ----
# Grafico densidad 1D ---
# ggplot(db.sur) + 
#   geom_density(aes(x = elevacion, fill = cat), position = position_dodge(1), alpha = 0.3) + 
#   facet_grid(cat~.,) +
#   labs(x = 'Altitude', y = 'Density') +
#   ylim(0, 0.002) +
#   theme_bw() +
#   theme(legend.position="none")
# # graficos de violin ---
# ancho.boxplot <- 0.1
# 
# # 0 a 500
# db500 <- subset(db, elevacion<=500)
# head(db500)
# 
# p1 <- ggplot(db500, aes(x = cat, y = p)) +
#   geom_violin(trim=FALSE) +
#   geom_boxplot(width=ancho.boxplot) +
#   stat_summary(fun.y=mean, geom="point", size=1, color="red") +
#   theme_bw() +
#   theme(legend.position="none") +
#   facet_wrap(~zona)
# 
# # 500 a 1000
# db1000 <- subset(db, elevacion>500 & elevacion<=1000)
# head(db1000)
# 
# p2 <- ggplot(db1000, aes(x = cat, y = p)) +
#   geom_violin(trim=FALSE) +
#   geom_boxplot(width=ancho.boxplot) +
#   stat_summary(fun.y=mean, geom="point", size=1, color="red") +
#   theme_bw() +
#   theme(legend.position="none") +
#   facet_wrap(~zona)
# 
# # 1000 a 1500
# db1500 <- subset(db, elevacion>1000 & elevacion<=1500)
# head(db1500)
# 
# p3 <- ggplot(db1500, aes(x = cat, y = p)) +
#   geom_violin(trim=FALSE) +
#   geom_boxplot(width=ancho.boxplot) +
#   stat_summary(fun.y=mean, geom="point", size=1, color="red") +
#   theme_bw() +
#   theme(legend.position="none") +
#   facet_wrap(~zona)
# 
# # 1500 a 2000
# db2000 <- subset(db, elevacion>1500 & elevacion<=2000)
# head(db2000)
# 
# p4 <- ggplot(db2000, aes(x = cat, y = p)) +
#   geom_violin(trim=FALSE) +
#   geom_boxplot(width=ancho.boxplot) +
#   stat_summary(fun.y=mean, geom="point", size=1, color="red") +
#   theme_bw() +
#   theme(legend.position="none") +
#   facet_wrap(~zona)
# 
# # 2000 a 2500
# db2500 <- subset(db, elevacion>2000 & elevacion<=2500)
# head(db2500)
# 
# p5 <- ggplot(db2500, aes(x = cat, y = p)) +
#   geom_violin(trim=FALSE) +
#   geom_boxplot(width=ancho.boxplot) +
#   stat_summary(fun.y=mean, geom="point", size=1, color="red") +
#   theme_bw() +
#   theme(legend.position="none") +
#   facet_wrap(~zona)
# 
# # 2500 a 3000
# db3000 <- subset(db, elevacion>2500 & elevacion<=3000)
# head(db3000)
# 
# p6 <- ggplot(db3000, aes(x = cat, y = p)) +
#   geom_violin(trim=FALSE) +
#   geom_boxplot(width=ancho.boxplot) +
#   stat_summary(fun.y=mean, geom="point", size=1, color="red") +
#   theme_bw() +
#   theme(legend.position="none") +
#   facet_wrap(~zona)
# 
# # 3000 a 3682
# max(db$elevacion)
# 
# db3500 <- subset(db, elevacion>3000)
# head(db3500)
# 
# p7 <- ggplot(db3500, aes(x = cat, y = p)) +
#   geom_violin(trim=FALSE) +
#   geom_boxplot(width=ancho.boxplot) +
#   stat_summary(fun.y=mean, geom="point", size=1, color="red") +
#   theme_bw() +
#   theme(legend.position="none") +
#   facet_wrap(~zona)
# 
# # plot final 
# library(cowplot)
# 
# setwd('C:/Users/Usuario/Desktop/')
# p.group <- plot_grid(p1, p2, p3, p4,
#                      p5, p6, p7,
#                      labels=c('AUTO'), ncol = 4, nrow = 2)
# 
# pdf('ej.pdf', width = 35, height = 10)
# p.group
# dev.off()
# # fin ---