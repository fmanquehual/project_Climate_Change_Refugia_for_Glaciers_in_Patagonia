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
jpeg('predic_MAXENT_CSIRO_con_graficos_de_densidad.jpg', width = 1290, height = 1400, units = "px", pointsize = 12,
     quality = 100, type = 'cairo', res = 110)

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


# preparacion

# referencia ---
ref.sur2 <- db_prob_elev(ref, r01, dem, 'Reference', 'sur')
ref.austral2 <- db_prob_elev(ref, r02, dem, 'Reference', 'austral')
db.ref <- rbind(ref.sur2, ref.austral2)
head(db.ref)

# lm
lm.ref <- lm(elevacion~p, data = db.ref)
summary(lm.ref)

b0.ref <- coef(lm.ref)[1] ; b0.ref
b1.ref <- coef(lm.ref)[2] ; b1.ref
range.p <- seq(0, max(db.ref$p), by=0.001)
val.ref <- (b1.ref*range.p)+b0.ref

plot(db.ref$p, db.ref$elevacion, xlab='Probability', ylab='Elevation')
lines(range.p, val.ref, col='red')

# 2050 RCP45 ---
rcp45.2050.sur <- db_prob_elev(ref, r1, dem, '2050 RCP45', 'sur')
rcp45.2050.austral <- db_prob_elev(ref, r2, dem, '2050 RCP45', 'austral')
db.rcp45.2050 <- rbind(rcp45.2050.sur, rcp45.2050.austral)
head(db.rcp45.2050)

# lm
lm.rcp45.2050 <- lm(elevacion~p, data = db.rcp45.2050)
summary(lm.rcp45.2050)

b0.rcp45.2050 <- coef(lm.rcp45.2050)[1] ; b0.rcp45.2050
b1.rcp45.2050 <- coef(lm.rcp45.2050)[2] ; b1.rcp45.2050
range.p.rcp45.2050 <- seq(0, max(db.rcp45.2050$p), by=0.001)
val.rcp45.2050 <- (b1.rcp45.2050*range.p.rcp45.2050)+b0.rcp45.2050

plot(db.rcp45.2050$p, db.rcp45.2050$elevacion, xlab='Probability', ylab='Elevation')
lines(range.p.rcp45.2050, val.rcp45.2050, col='red')

# 2050 RCP85 ---
rcp85.2050.sur <- db_prob_elev(ref, r3, dem, '2050 RCP85', 'sur')
rcp85.2050.austral <- db_prob_elev(ref, r4, dem, '2050 RCP85', 'austral')
db.rcp85.2050 <- rbind(rcp85.2050.sur, rcp85.2050.austral)
head(db.rcp85.2050)

# lm
lm.rcp85.2050 <- lm(elevacion~p, data = db.rcp85.2050)
summary(lm.rcp85.2050)

b0.rcp85.2050 <- coef(lm.rcp85.2050)[1] ; b0.rcp85.2050
b1.rcp85.2050 <- coef(lm.rcp85.2050)[2] ; b1.rcp85.2050
range.p.rcp85.2050 <- seq(0, max(db.rcp85.2050$p), by=0.001)
val.rcp85.2050 <- (b1.rcp85.2050*range.p.rcp85.2050)+b0.rcp85.2050

plot(db.rcp85.2050$p, db.rcp85.2050$elevacion, xlab='Probability', ylab='Elevation')
lines(range.p.rcp85.2050, val.rcp85.2050, col='red')

# 2070 RCP45 ---
rcp45.2070.sur <- db_prob_elev(ref, r5, dem, '2070 RCP45', 'sur')
rcp45.2070.austral <- db_prob_elev(ref, r6, dem, '2070 RCP45', 'austral')
db.rcp45.2070 <- rbind(rcp45.2070.sur, rcp45.2070.austral)
head(db.rcp45.2070)

# lm
lm.rcp45.2070 <- lm(elevacion~p, data = db.rcp45.2070)
summary(lm.rcp45.2070)

b0.rcp45.2070 <- coef(lm.rcp45.2070)[1] ; b0.rcp45.2070
b1.rcp45.2070 <- coef(lm.rcp45.2070)[2] ; b1.rcp45.2070
range.p.rcp45.2070 <- seq(0, max(db.rcp45.2070$p), by=0.001)
val.rcp45.2070 <- (b1.rcp45.2070*range.p.rcp45.2070)+b0.rcp45.2070

plot(db.rcp45.2070$p, db.rcp45.2070$elevacion, xlab='Probability', ylab='Elevation')
lines(range.p.rcp45.2070, val.rcp45.2070, col='red')

# 2050 RCP85 ---
rcp85.2070.sur <- db_prob_elev(ref, r7, dem, '2070 RCP85', 'sur')
rcp85.2070.austral <- db_prob_elev(ref, r8, dem, '2070 RCP85', 'austral')
db.rcp85.2070 <- rbind(rcp85.2070.sur, rcp85.2070.austral)
head(db.rcp85.2070)

# lm
lm.rcp85.2070 <- lm(elevacion~p, data = db.rcp85.2070)
summary(lm.rcp85.2070)

b0.rcp85.2070 <- coef(lm.rcp85.2070)[1] ; b0.rcp85.2070
b1.rcp85.2070 <- coef(lm.rcp85.2070)[2] ; b1.rcp85.2070
range.p.rcp85.2070 <- seq(0, max(db.rcp85.2070$p), by=0.001)
val.rcp85.2070 <- (b1.rcp85.2070*range.p.rcp85.2070)+b0.rcp85.2070

plot(db.rcp85.2070$p, db.rcp85.2070$elevacion, xlab='Probability', ylab='Elevation')
lines(range.p.rcp85.2070, val.rcp85.2070, col='red')

# union dbs ----
db <- rbind(db.ref, db.rcp45.2050, db.rcp45.2070, db.rcp85.2050, db.rcp85.2070)
head(db)

# agrupacion de elevacion 

db$elevacion2 <- 0

idx <- which(db$elevacion<=10)
db$elevacion2[idx] <- 10

max(db$elevacion)
for (i in seq(10, 3680, by=10)) {
idx <- which(db$elevacion>i & db$elevacion<=i+10)
db$elevacion2[idx] <- i+10
}

# idx <- which(db$elevacion>3500 & db$elevacion<=max(db$elevacion))
# db$elevacion2[idx] <- 3600

head(db)


# agrupacion de probabilidad

db$p2 <- 0

idx <- which(db$p<=0.01)
db$p2[idx] <- 0.01

for (i in seq(0.01, 0.99, by=0.01)) {
  idx <- which(db$p>i & db$p<=i+0.01)
  db$p2[idx] <- i+0.01
}

head(db)

# fin ---

# Grafico densidad 1D ---
ggplot(db) + 
  geom_density(aes(x = elevacion, fill = cat), position = position_dodge(1), alpha = 0.3) + 
  facet_grid(cat~.,) +
  labs(x = 'Elevation', y = 'Density') +
  ylim(0, 0.002) +
  theme_bw() +
  theme(legend.position="none")


# Graficos densidad 2D Z.G.Sur ----
db.s <- subset(db, zona == 'sur')
dim(db.s)

setwd('C:/Users/Usuario/OneDrive/plots_paper/')
setEPS()
postscript(file = "densidad_de_probabilidad_2D_elevacion_sur.eps", height = 6, width = 9)  # Una figura en cm
par(mar=c(4,4,0,0)+0.1)

ggplot(db.s, aes(x=p2, y=elevacion2) ) +
  geom_hex(bins = 70) +
  lims(x=c(0,1)) +
  labs(x = 'Probability', y = 'Elevation') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  facet_wrap(vars(cat), ncol = 3) +
  scale_fill_gradientn('Absolute\nfrequency', colours = c('#103C5D', '#2683C8', '#03FBFF'), na.value = NA) +
  #scale_fill_gradientn('Absolute\nfrequency', colours = c('#4E2577', '#E5C285'), na.value = NA) +
  #scale_fill_viridis_c('Absolute\nfrequency') +
  #scale_fill_viridis_c('Absolute\nfrequency', direction = -1) +
  #scale_fill_viridis_c('Absolute\nfrequency', option = 'magma', direction = -1) +
  theme_bw()
dev.off()
# fin ---

# Graficos densidad 2D Z.G.Austral ----
db.a <- subset(db, zona == 'austral')
dim(db.a)

setwd('C:/Users/Usuario/OneDrive/plots_paper/')
setEPS()
postscript(file = "densidad_de_probabilidad_2D_elevacion_austral.eps", height = 6, width = 9)  # Una figura en cm
par(mar=c(4,4,0,0)+0.1)

ggplot(db.a, aes(x=p, y=elevacion) ) +
  geom_hex(bins = 70) +
  lims(x=c(0,1)) +
  labs(x = 'Probability', y = 'Elevation') +
  geom_smooth(method = lm, # Recta de regresión
              se = FALSE, col = 'red') + # Oculta intervalo de confianza
  facet_wrap(vars(cat), ncol = 3) +
  scale_fill_gradientn('Absolute\nfrequency', colours = c('#103C5D', '#2683C8', '#03FBFF'), na.value = NA) +
  theme_bw()
dev.off()
# fin ---





# otros ----
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