library('raster')
library('ggplot2')
library('gridExtra')
library('RColorBrewer')

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

# functions ----
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
# fin ---

# dem <- raster("dem_baker_grande_clip_19s.tif")
dem.g <- raster("dem_glaciares_baker_19s.tif")
# dem.g.sin.CHN <- raster("dem_glaciares_baker_sin_CHN_19s.tif")


setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/resultados_prediccion/')

rcp60_2050 <- readOGR('.', 'polygon_refugioCC_csiro_2050_rcp60')
dem_rcp60_2050 <- mask(dem.g, rcp60_2050)
plot(dem_rcp60_2050)
  
rcp85_2050 <- readOGR('.', 'polygon_refugioCC_csiro_2050_rcp85')
dem_rcp85_2050 <- mask(dem.g, rcp85_2050)
plot(dem_rcp85_2050)

rcp60_2070 <- readOGR('.', 'polygon_refugioCC_csiro_2070_rcp60')
dem_rcp60_2070 <- mask(dem.g, rcp60_2070)
plot(dem_rcp60_2070)

rcp85_2070 <- readOGR('.', 'polygon_refugioCC_csiro_2070_rcp85')
dem_rcp85_2070 <- mask(dem.g, rcp85_2070)
plot(dem_rcp85_2070)


# plot(dem)
# plot(dem.g)



#db.dem <- db.hist(dem)
#head(db.dem)

db.dem.g <- db.hist(dem.g)
#head(db.dem.g)

#db.dem.g.sin.CHN <- db.hist(dem.g.sin.CHN)
#head(db.dem.g.sin.CHN)



db.rcp60_2050 <- db.hist(dem_rcp60_2050)
db.rcp60_2070 <- db.hist(dem_rcp60_2070)
db.rcp85_2050 <- db.hist(dem_rcp85_2050)
db.rcp85_2070 <- db.hist(dem_rcp85_2070)






# plot glaciares ----
db.dem.g$group.name <- as.character(db.dem.g$group.name)

value.i <- unlist(table(db.dem.g$group.name))
value.i

db <- data.frame(freq=value.i)
row.names(db) <- 1:nrow(db)
names(db) <- c("rangos", "freq")
db 

area.pixel.km2 <- 900/1000000 # resolucion original 30m
db$area <- db$freq*area.pixel.km2
sum(db$area) # superficie de glaciares 2.164 km2
db

db$rangos <- factor( db$rangos, levels = levels( db$rangos )[ c(1, 8, 2, 3, 4, 5, 6, 7) ] )
db0 <- db

bp0 <- ggplot(db, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(A)", x="Rangos de Altitud", y="Superficie (km2)") +
  scale_y_continuous(limits=c(0, 1100), breaks = c(0, 200, 400, 600, 800, 1000))
bp0

# fin ---








# plot rcp60_2050 ----
db.rcp60_2050$group.name <- as.character(db.rcp60_2050$group.name)

value.i <- unlist(table(db.rcp60_2050$group.name))
value.i

db <- data.frame(freq=value.i)
row.names(db) <- 1:nrow(db)
names(db) <- c("rangos", "freq")
db 

area.pixel.km2 <- 900/1000000 # resolucion original 30m
db$area <- db$freq*area.pixel.km2
sum(db$area) # superficie de glaciares 2.164 km2
db

db2 <- data.frame(rangos=c('0-500', '3500-4000'), freq=c(0, 0), area=c(0, 0))
db2

dbf <- rbind(db, db2)

# db$rangos <- factor( db$rangos, levels = levels( db$rangos )[ c(6, 1, 2, 3, 4, 5) ] )
# db

dbf$rangos <- factor( dbf$rangos, levels = levels( dbf$rangos )[ c(7, 6, 1, 2, 3, 4, 5, 8) ] )
dbf1 <- dbf

bp1 <- ggplot(dbf1, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(B)", x="Rangos de Altitud", y="Superficie (km2)") +
  scale_y_continuous(limits=c(0, 1100), breaks = c(0, 200, 400, 600, 800, 1000))
bp1

# fin ---





# plot rcp85_2050 ----
db.rcp85_2050$group.name <- as.character(db.rcp85_2050$group.name)

value.i <- unlist(table(db.rcp85_2050$group.name))
value.i

db <- data.frame(freq=value.i)
row.names(db) <- 1:nrow(db)
names(db) <- c("rangos", "freq")
db 

area.pixel.km2 <- 900/1000000 # resolucion original 30m
db$area <- db$freq*area.pixel.km2
sum(db$area) # superficie de glaciares 2.164 km2
db

db2 <- data.frame(rangos=c('0-500', '3500-4000'), freq=c(0, 0), area=c(0, 0))
db2

dbf <- rbind(db, db2)

# db$rangos <- factor( db$rangos, levels = levels( db$rangos )[ c(6, 1, 2, 3, 4, 5) ] )
# db

dbf$rangos <- factor( dbf$rangos, levels = levels( dbf$rangos )[ c(7, 6, 1, 2, 3, 4, 5, 8) ] )
dbf2 <- dbf

bp2 <- ggplot(dbf2, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(C)", x="Rangos de Altitud", y="Superficie (km2)")+
  scale_y_continuous(limits=c(0, 1100), breaks = c(0, 200, 400, 600, 800, 1000))
bp2

# fin ---







# plot rcp60_2070 ----
db.rcp60_2070$group.name <- as.character(db.rcp60_2070$group.name)

value.i <- unlist(table(db.rcp60_2070$group.name))
value.i

db <- data.frame(freq=value.i)
row.names(db) <- 1:nrow(db)
names(db) <- c("rangos", "freq")
db 

area.pixel.km2 <- 900/1000000 # resolucion original 30m
db$area <- db$freq*area.pixel.km2
sum(db$area) # superficie de glaciares 2.164 km2
db

db2 <- data.frame(rangos=c('0-500', '3500-4000'), freq=c(0, 0), area=c(0, 0))
db2

dbf <- rbind(db, db2)

# db$rangos <- factor( db$rangos, levels = levels( db$rangos )[ c(6, 1, 2, 3, 4, 5) ] )
# db

dbf$rangos <- factor( dbf$rangos, levels = levels( dbf$rangos )[ c(7, 6, 1, 2, 3, 4, 5, 8) ] )
dbf3 <- dbf

bp3 <- ggplot(dbf3, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(D)", x="Rangos de Altitud", y="Superficie (km2)")+
  scale_y_continuous(limits=c(0, 1100), breaks = c(0, 200, 400, 600, 800, 1000))
bp3

# fin ---







# plot rcp85_2070 ----
db.rcp85_2070$group.name <- as.character(db.rcp85_2070$group.name)

value.i <- unlist(table(db.rcp85_2070$group.name))
value.i

db <- data.frame(freq=value.i)
row.names(db) <- 1:nrow(db)
names(db) <- c("rangos", "freq")
db 

area.pixel.km2 <- 900/1000000 # resolucion original 30m
db$area <- db$freq*area.pixel.km2
sum(db$area) # superficie de glaciares 2.164 km2
db

db2 <- data.frame(rangos=c('0-500', '3500-4000'), freq=c(0, 0), area=c(0, 0))
db2

dbf <- rbind(db, db2)

# db$rangos <- factor( db$rangos, levels = levels( db$rangos )[ c(6, 1, 2, 3, 4, 5) ] )
# db

dbf$rangos <- factor( dbf$rangos, levels = levels( dbf$rangos )[ c(7, 6, 1, 2, 3, 4, 5, 8) ] )
dbf4 <- dbf

bp4 <- ggplot(dbf4, aes(x=rangos, y=area))+
  geom_bar(width = 0.7, stat = "identity") +
  labs(title = "(E)", x="Rangos de Altitud", y="Superficie (km2)") +
  scale_y_continuous(limits=c(0, 1100), breaks = c(0, 200, 400, 600, 800, 1000))
bp4

# fin ---





# setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
# 
# png("piechart_superficie_clima_grupo_principal_koppen_presente_futuro.png", width = 680, height = 450, units = "px")

# grid.arrange(bp0, bp1, bp2, bp3, bp4,                                   # bar plot spaning two columns
#              ncol = 2, nrow = 3)
#dev.off()

db0$tipo <- '0_Referencia'
dbf1$tipo <- '2050_RCP60'
dbf2$tipo <- '2050_RCP85'
dbf3$tipo <- '2070_RCP60'
dbf4$tipo <- '2070_RCP85'

dbf.ok <- rbind(db0, dbf1, dbf2, dbf3, dbf4)
dbf.ok


rcp60_2050_name <- paste('2050', 'RCP', '6,0')
rcp85_2050_name <- paste('2050', 'RCP', '8,5')
rcp60_2070_name <- paste('2070', 'RCP', '6,0')
rcp85_2070_name <- paste('2070', 'RCP', '8,5')



setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
png("barchart_superficie_glaciares_refugiosCC_2.png", width = 680, height = 400, units = "px")

ggplot(dbf.ok, aes(x=rangos, y=area, fill=tipo)) + 
  geom_bar(stat="identity", position="dodge") +#, color='black') + 
  #scale_fill_brewer()
  scale_fill_viridis_d(name= 'Periodo', labels = c("Referencia", rcp60_2050_name, rcp85_2050_name, 
                                                   rcp60_2070_name, rcp85_2070_name)) +
  labs(x="Rangos de Altitud", y="Superficie (km2)")
dev.off()
# 
# 
# 
# 
# #plot cuenca ----
# db.dem$group.name <- as.factor(db.dem$group.name)
# levels(db.dem$group.name)
# value.i <- unlist(table(db.dem$group.name))
# 
# db <- data.frame(freq=value.i)
# row.names(db) <- 1:nrow(db)
# names(db) <- c("rangos", "freq")
# str(db)
# 
# area.pixel.km2 <- 900/1000000 # resolucion original 30m
# db$area <- db$freq*area.pixel.km2
# sum(db$area) # superficie teorica 28050.48 km2
# db
# 
# db$rangos <- factor( db$rangos, levels = levels( db$rangos )[ c(1, 8, 2, 3, 4, 5, 6, 7) ] )
# db
# 
# bp0 <- ggplot(db, aes(x=rangos, y=area))+
#   geom_bar(width = 0.7, stat = "identity") +
#   labs(title = "(A) Distribución de superficie según altitud a nivel de cuenca", x="Rangos de Altitud", y="Superficie (km2)")
# bp0
# 
# # fin ---
# 
# 
# 
# 
# 
# 
# # plot glaciares ----
# db.dem.g.sin.CHN$group.name <- as.character(db.dem.g.sin.CHN$group.name)
# 
# value.i <- unlist(table(db.dem.g.sin.CHN$group.name))
# value.i
# 
# db <- data.frame(freq=value.i)
# row.names(db) <- 1:nrow(db)
# names(db) <- c("rangos", "freq")
# db 
# 
# area.pixel.km2 <- 900/1000000 # resolucion original 30m
# db$area <- db$freq*area.pixel.km2
# sum(db$area)
# db
# 
# db$rangos <- factor( db$rangos, levels = levels( db$rangos )[ c(1, 8, 2, 3, 4, 5, 6, 7) ] )
# db
# 
# bp2 <- ggplot(db, aes(x=rangos, y=area))+
#   geom_bar(width = 0.7, stat = "identity") +
#   labs(title = "(C) Distribución de superficie según altitud de glaciares (sin CHN)", x="Rangos de Altitud", y="Superficie (km2)")
# bp2
# 
# 
# 
# 
# 
# 
# 
# 
# # plot out ----
# #library(gridExtra)
# setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
# 
# #png("hist_superficie_altiud_cuenca_y_glaciares.png")
# grid.arrange(bp0, 
#              bp1,
#              bp2, 
#              ncol = 1, nrow = 3)
# dev.off()
