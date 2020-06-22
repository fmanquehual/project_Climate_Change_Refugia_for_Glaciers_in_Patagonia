library('ggplot2')
library('rgdal')
library('rgeos')
library('raster')
require('foreign')

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
db.hist <- function(raster.i){
  
  db <- as.data.frame(raster.i)
  db2 <- na.omit(db)
  head(db2)
  
  db2$group <- 0
  db2$group[db2[,1] <= 500] <- 1
  db2$group[db2[,1] > 500 & db2[,1] <= 1000] <- 2
  db2$group[db2[,1] > 1000 & db2[,1] <= 1500] <- 3
  db2$group[db2[,1] > 1500 & db2[,1] <= 2000] <- 4
  db2$group[db2[,1] > 2000 & db2[,1] <= 2500] <- 5
  db2$group[db2[,1] > 2500 & db2[,1] <= 3000] <- 6
  db2$group[db2[,1] > 3000 & db2[,1] <= 3500] <- 7
  db2$group[db2[,1] > 3500 & db2[,1] <= 4000] <- 8
  
  db2$group.name <- "NA"
  db2$group.name[db2$group==1] <- "0-500"
  db2$group.name[db2$group==2] <- "500-1000"
  db2$group.name[db2$group==3] <- "1000-1500"
  db2$group.name[db2$group==4] <- "1500-2000"
  db2$group.name[db2$group==5] <- "2000-2500"
  db2$group.name[db2$group==6] <- "2500-3000"
  db2$group.name[db2$group==7] <- "3000-3500"
  db2$group.name[db2$group==8] <- "3500-4000"
  
  row.names(db2) <- 1:nrow(db2)
  
  return(db2)
}

# f3
preparacion.db <- function(db.i, zona.i){
  db.i$group.name <- as.character(db.i$group.name)
  
  value.i <- unlist(table(db.i$group.name))
  value.i
  
  db.j <- data.frame(freq=value.i)
  row.names(db.j) <- 1:nrow(db.j)
  names(db.j) <- c("rangos", "freq")
  
  area.pixel.km2 <- (24.95*24.95)/1000000 # resolucion original 30m
  db.j$area <- db.j$freq*area.pixel.km2
  sum(db.j$area) # superficie de glaciares 2.164 km2
  
  db.j$rangos <- factor( db.j$rangos, levels = c('0-500', '500-1000', '1000-1500', '1500-2000', '2000-2500', '2500-3000', '3000-3500', '3500-4000') )
  
  if(zona.i=='sur'){db.j$zona <- 'sur'} else(db.j$zona <- 'austral')
  return(db.j)
  }

# fin ---






# lectura coberturas ----

setwd("C:/Users/Usuario/Documents/Francisco/predicciones_maxent/refugiosCC/")

ref <- readOGR('.', 'polygon_referencia')
plot(ref)
ref.19s <- spTransform(ref, utm19)
ref.19s <- gBuffer(ref.19s, width = 0)
ref.wgs84 <- spTransform(ref.19s, wgs84)

predict.2050.rcp45 <- readOGR('.', 'polygon_refugioCC_csiro_2050_rcp45')
predict.2050.rcp45.wgs84 <- spTransform(predict.2050.rcp45, wgs84)
predict.2050.rcp45.19S <- spTransform(predict.2050.rcp45, utm19)
predict.2050.rcp45.19S <- gBuffer(predict.2050.rcp45.19S, width = 0)
predict.2050.rcp45.wgs84 <- spTransform(predict.2050.rcp45.19S, wgs84)

predict.2050.rcp85 <- readOGR('.', 'polygon_refugioCC_csiro_2050_rcp85')
predict.2050.rcp85.wgs84 <- spTransform(predict.2050.rcp85, wgs84)
predict.2050.rcp85.19S <- spTransform(predict.2050.rcp85, utm19)

predict.2070.rcp45 <- readOGR('.', 'polygon_refugioCC_csiro_2070_rcp45')
predict.2070.rcp45.wgs84 <- spTransform(predict.2070.rcp45, wgs84)
predict.2070.rcp45.19S <- spTransform(predict.2070.rcp45, utm19)

predict.2070.rcp85 <- readOGR('.', 'polygon_refugioCC_csiro_2070_rcp85')
predict.2070.rcp85.wgs84 <- spTransform(predict.2070.rcp85, wgs84)
predict.2070.rcp85.19S <- spTransform(predict.2070.rcp85, utm19)

head(ref.19s)
db <- data.frame(Periodo=c('Reference', 's_rcp45_2050', 't_rcp85_2050', 'u_rcp45_2070', 'v_rcp85_2070'),
                 area_km2=c( round( gArea(ref.19s)/1000000, 2),
                             round( gArea(predict.2050.rcp45.19S)/1000000, 2),
                             round( gArea(predict.2050.rcp85.19S)/1000000, 2),
                             round( gArea(predict.2070.rcp45.19S)/1000000, 2),
                             round( gArea(predict.2070.rcp85.19S)/1000000, 2)
                 ))

db$id <- 1:nrow(db)
db$RCP <- c('-', '4.5', '8.5', '4.5', '8.5')
db

col.f <- c("#003366", "#6A9AC6", "#C4D7E8")#, "#FFFF00", "#FF6600")

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "grafico_area_refugiosCC.eps", height = 6, width = 6)  # Una figura en cm
# par(mfrow=c(1,1),mai=c(0.5,0.5,0.3,0))

#ggplot(db,aes(x=Periodo, y=area_km2, label=area_km2, fill=RCP))+ # con etiquetas
ggplot(db,aes(x=Periodo, y=area_km2, fill=RCP))+
     geom_bar(stat="identity",position="dodge", width = 0.8)+
#     geom_text(position = position_dodge(width = 0.5), angle = 0, vjust = -.5, size = 3) + # etiquetas
     scale_fill_manual(values = c('#063346', '#566E78', '#AAB7BC', '#566E78', '#AAB7BC')) +
     labs(x='Period', y='Area (km2)') +
     scale_x_discrete(labels = c('Reference','2050','2050','2070','2070')) +
     scale_y_continuous(breaks = c(seq(0, 20000, by=2500)) ) +
  theme_bw()

# dev.off()





















# altitud ----

# Recorte del dem hecho en Qgis!
setwd('C:/Users/Usuario/Documents/Francisco/predicciones_maxent/refugiosCC/dem_refugiosCC/zona_glaciologica_sur/')

# zona sur 
dem.ref.sur <- raster('dem_ref_sur_geo.tif')
db.dem.ref.sur <- raster.to.db(dem.ref.sur)
db.hist.dem.ref.sur <- db.hist(db.dem.ref.sur)
head(db.hist.dem.ref.sur)

dem.2050.rcp45.sur <- raster('dem_refugioCC_csiro_2050_rcp45_sur_geo.tif')
db.dem.2050.rcp45.sur <- raster.to.db(dem.2050.rcp45.sur)
db.hist.dem.2050.rcp45.sur <- db.hist(db.dem.2050.rcp45.sur)
head(db.hist.dem.2050.rcp45.sur)

dem.2050.rcp85.sur <- raster('dem_refugioCC_csiro_2050_rcp85_sur_geo.tif')
db.dem.2050.rcp85.sur <- raster.to.db(dem.2050.rcp85.sur)
db.hist.dem.2050.rcp85.sur <- db.hist(db.dem.2050.rcp85.sur)
head(db.hist.dem.2050.rcp85.sur)

dem.2070.rcp45.sur <- raster('dem_refugioCC_csiro_2070_rcp45_sur_geo.tif')
db.dem.2070.rcp45.sur <- raster.to.db(dem.2070.rcp45.sur)
db.hist.dem.2070.rcp45.sur <- db.hist(db.dem.2070.rcp45.sur)
head(db.hist.dem.2070.rcp45.sur)

dem.2070.rcp85.sur <- raster('dem_refugioCC_csiro_2070_rcp85_sur_geo.tif')
db.dem.2070.rcp85.sur <- raster.to.db(dem.2070.rcp85.sur)
db.hist.dem.2070.rcp85.sur <- db.hist(db.dem.2070.rcp85.sur)
head(db.hist.dem.2070.rcp85.sur)


setwd('C:/Users/Usuario/Documents/Francisco/predicciones_maxent/refugiosCC/dem_refugiosCC/zona_glaciologica_austral/')

# zona austral
dem.ref.austral <- raster('dem_ref_austral_geo.tif')
db.dem.ref.austral <- raster.to.db(dem.ref.austral)
db.hist.dem.ref.austral <- db.hist(db.dem.ref.austral)
head(db.hist.dem.ref.austral)

dem.2050.rcp45.austral <- raster('dem_refugioCC_csiro_2050_rcp45_austral_geo.tif')
db.dem.2050.rcp45.austral <- raster.to.db(dem.2050.rcp45.austral)
db.hist.dem.2050.rcp45.austral <- db.hist(db.dem.2050.rcp45.austral)
head(db.hist.dem.2050.rcp45.austral)

dem.2050.rcp85.austral <- raster('dem_refugioCC_csiro_2050_rcp85_austral_geo.tif')
db.dem.2050.rcp85.austral <- raster.to.db(dem.2050.rcp85.austral)
db.hist.dem.2050.rcp85.austral <- db.hist(db.dem.2050.rcp85.austral)
head(db.hist.dem.2050.rcp85.austral)

dem.2070.rcp45.austral <- raster('dem_refugioCC_csiro_2070_rcp45_austral_geo.tif')
db.dem.2070.rcp45.austral <- raster.to.db(dem.2070.rcp45.austral)
db.hist.dem.2070.rcp45.austral <- db.hist(db.dem.2070.rcp45.austral)
head(db.hist.dem.2070.rcp45.austral)

dem.2070.rcp85.austral <- raster('dem_refugioCC_csiro_2070_rcp85_austral_geo.tif')
db.dem.2070.rcp85.austral <- raster.to.db(dem.2070.rcp85.austral)
db.hist.dem.2070.rcp85.austral <- db.hist(db.dem.2070.rcp85.austral)
head(db.hist.dem.2070.rcp85.austral)

# fin ---





# plot glaciares ----

dbf0.sur <- preparacion.db(db.hist.dem.ref.sur, 'sur')
dbf0.austral <- preparacion.db(db.hist.dem.ref.austral, 'austral')

dbf0 <- rbind(dbf0.sur, dbf0.austral)
head(dbf0)

bp0 <- ggplot(dbf0, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(A)", x="Rangos de Altitud", y="Superficie (km2)") +
  facet_wrap(~zona)
bp0

# fin ---








# plot rcp60_2050 ----
dbf1.sur <- preparacion.db(db.hist.dem.2050.rcp45.sur, 'sur')
dbf1.austral <- preparacion.db(db.hist.dem.2050.rcp45.austral, 'austral')

dbf1 <- rbind(dbf1.sur, dbf1.austral)

bp1 <- ggplot(dbf1, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(B)", x="Rangos de Altitud", y="Superficie (km2)") +
  facet_wrap(~zona)
bp1

# fin ---





# plot rcp85_2050 ----
dbf2.sur <- preparacion.db(db.hist.dem.2050.rcp85.sur, 'sur')
dbf2.austral <- preparacion.db(db.hist.dem.2050.rcp85.austral, 'austral')

dbf2 <- rbind(dbf2.sur, dbf2.austral)

bp2 <- ggplot(dbf2, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(C)", x="Rangos de Altitud", y="Superficie (km2)") +
  facet_wrap(~zona)
bp2

# fin ---







# plot rcp60_2070 ----
dbf3.sur <- preparacion.db(db.hist.dem.2070.rcp45.sur, 'sur')
dbf3.austral <- preparacion.db(db.hist.dem.2070.rcp45.austral, 'austral')

dbf3 <- rbind(dbf3.sur, dbf3.austral)

bp3 <- ggplot(dbf3, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(D)", x="Rangos de Altitud", y="Superficie (km2)") +
  facet_wrap(~zona)
bp3

# fin ---







# plot rcp85_2070 ----
dbf4.sur <- preparacion.db(db.hist.dem.2070.rcp85.sur, 'sur')
dbf4.austral <- preparacion.db(db.hist.dem.2070.rcp85.austral, 'austral')

dbf4 <- rbind(dbf4.sur, dbf4.austral)

bp4 <- ggplot(dbf4, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(E)", x="Rangos de Altitud", y="Superficie (km2)") +
  facet_wrap(~zona)
bp4

# fin ---



dbf0$tipo <- '0_Referencia'
dbf1$tipo <- '2050_RCP45'
dbf2$tipo <- '2050_RCP85'
dbf3$tipo <- '2070_RCP45'
dbf4$tipo <- '2070_RCP85'

dbf.plot1 <- rbind(dbf0, dbf1, dbf2, dbf3, dbf4)
head(dbf.plot1)
levels(dbf.plot1$rangos)

db.complemento <- data.frame(rangos=c('0-500', '500-1000', '0-500', '500-1000', '0-500', '500-1000', '0-500', '500-1000'),
                             freq=c(0,0,0,0, 0,0,0,0),
                             area=c(0,0,0,0, 0,0,0,0),
                             zona=c('austral', 'austral', 'austral', 'austral', 'austral', 'austral', 'austral', 'austral'),
                             tipo=c('2050_RCP45', '2050_RCP85', '2070_RCP45', '2070_RCP85', '2050_RCP85', '2050_RCP45', '2070_RCP85', '2070_RCP45'))
dbf.plot2 <- rbind(dbf.plot1, db.complemento)

dbf.plot2$zona.ingles <- NA
dbf.plot2$zona.ingles[dbf.plot2$zona%in%'sur'] <- 'South'
dbf.plot2$zona.ingles[dbf.plot2$zona%in%'austral'] <- 'Austral'

dbf.plot2$rangos <- factor( dbf.plot2$rangos, levels = c('0-500', '500-1000', '1000-1500', '1500-2000', '2000-2500', '2500-3000', '3000-3500', '3500-4000') )
dbf.plot2$zona.ingles <- factor(dbf.plot2$zona.ingles, levels = c('South', 'Austral'))
levels(dbf.plot2$rangos)
levels(dbf.plot2$zona.ingles)

dbf.plot3 <- subset(dbf.plot2, rangos!='NA')
dbf.plot3

rcp60_2050_name <- paste('2050', 'RCP', '4.5')
rcp85_2050_name <- paste('2050', 'RCP', '8.5')
rcp60_2070_name <- paste('2070', 'RCP', '4.5')
rcp85_2070_name <- paste('2070', 'RCP', '8.5')

tapply(dbf.plot3$area, dbf.plot3$tipo, sum)

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "grafico_area_altitud.eps", height = 6, width = 6)  # Una figura en cm
# par(mfrow=c(1,1),mai=c(0.5,0.5,0.3,0))

ggplot(dbf.plot3, aes(x=rangos, y=area, fill=tipo)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values = c('#063346', '#566E78', '#AAB7BC', '#566E78', '#AAB7BC'), name= 'Period and\nscenario', 
                      labels = c("Reference", rcp60_2050_name, rcp85_2050_name, rcp60_2070_name, rcp85_2070_name)) +
  labs(x="Elevation range", y="Area (km2)") +
  scale_y_continuous(breaks = c( seq(0,6000, by=1000) )) +
  facet_wrap(~zona.ingles) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, size = 8,hjust = 1, vjust = 1))
# dev.off()

# fin ---







# calculos ----

d1 <- subset(dbf.plot3, zona=='sur')
d1
