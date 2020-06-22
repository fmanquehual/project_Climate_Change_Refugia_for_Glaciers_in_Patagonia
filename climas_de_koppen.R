library('raster')
library('rgdal')
library('rgeos')
library('stringr')
library('ggplot2')
library('RColorBrewer')

# lectura coberturas ----

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# ---
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/climas_koppen/rasters/')
r1 <- raster('Beck_KG_V1_present_0p0083.tif')
r2 <- raster('Beck_KG_V1_future_0p0083.tif')
plot(r1)
plot(r2)

# ---
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
marco <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')

# ---
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/var_presencia/')
#g <- readOGR('.', 'points_presencia_glaciares_anho_2011')
g <- readOGR('.', 'points_presencia_glaciares_completo')

# ---
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/Glaciares_Nacional/')
g.poly <- readOGR(".", "polygon_glaciares_marco_trabajo_nuevo_geo")
head(g.poly@data)

# ---
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/climas_koppen/')
db <- read.csv('codificacion_koppen_para_join.csv')
db

# ---
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
r.clim <- raster('climas_koppen_nueva_area_estudio_geo.tif')
r.clim2 <- crop(r2, marco)

# fin ---






# reclasificacion ----

m1 = c(0, 0, NA,
       0, 3, 5,
       3, 7, 4,
       7, 16, 3,
       16, 28, 2,
       28, 30, 1)

rclmat.1 = matrix(m1, ncol=3, byrow=TRUE)
r.clim.reclas <- reclassify(r.clim, rclmat.1, include.lowest=TRUE)
r.clim.reclas2 <- reclassify(r.clim2, rclmat.1, include.lowest=TRUE)
unique(values(r.clim.reclas))
unique(values(r.clim.reclas2))
plot(r.clim.reclas, col=bpy.colors(5))
plot(r.clim.reclas2, col=bpy.colors(5))

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
# writeRaster(r.clim.reclas, filename='climas_koppen_nueva_area_estudio_reclas_geo.tif', format="GTiff", overwrite=TRUE)
# writeRaster(r.clim.reclas2, filename='climas_koppen_futuro_nueva_area_estudio_reclas_geo.tif', format="GTiff", overwrite=TRUE)
# fin ---




# preparacion raster clima ----

r1.poly.ori <- rasterToPolygons(r.clim, fun=function(x){x>0}, dissolve = TRUE)
r2.poly.fut <- rasterToPolygons(r.clim2, fun=function(x){x>0},  dissolve = TRUE)

plot(r1.poly.ori)
head(r1.poly.ori@data)

plot(r2.poly.fut)
head(r2.poly.fut@data)

r1.poly <- r1.poly.ori
names(r1.poly@data) <- 'id_clima'
r1.poly@data$cod <- NA
r1.poly@data$class <- NA

r2.poly <- r2.poly.fut
names(r2.poly@data) <- 'id_clima'
r2.poly@data$cod <- NA
r2.poly@data$class <- NA

head(r1.poly@data)
table(r1.poly@data$id_clima)

head(r2.poly@data)
table(r2.poly@data$id_clima)

id.i <- as.numeric(names(table(r1.poly@data$id_clima)))
id.j <- as.numeric(names(table(r2.poly@data$id_clima)))
db$cod <- as.character(db$cod)
db$class <- as.character(db$class)


for (i in id.i) {
  r1.poly@data$cod[r1.poly@data$id_clima==i] <- unique(db$cod[db$id==i])  
  r1.poly@data$class[r1.poly@data$id_clima==i] <- unique(db$class[db$id==i])  
}

for (j in id.j) {
  r2.poly@data$cod[r2.poly@data$id_clima==j] <- unique(db$cod[db$id==j])  
  r2.poly@data$class[r2.poly@data$id_clima==j] <- unique(db$class[db$id==j])  
}

r1.poly@data$main_cod <- str_sub(r1.poly$cod, 1, 1)
head(r1.poly@data)

r2.poly@data$main_cod <- str_sub(r2.poly$cod, 1, 1)
head(r2.poly@data)

par(mfrow=c(1,2))
plot(r1.poly, col=terrain.colors(length(id.i)), border=terrain.colors(length(id.i)))
plot(r2.poly, col=terrain.colors(length(id.j)), border=terrain.colors(length(id.j)))

setwd("C:/Users/Usuario/Documents/Francisco/coberturas/")
#writeOGR(r1.poly, ".", "polygon_climas_koppen_nueva_area_estudio_geo", driver="ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(r2.poly, ".", "polygon_climas_koppen_futuro_nueva_area_estudio_geo", driver="ESRI Shapefile", overwrite_layer = TRUE)
# ----

r1.poly@data$main_cod
idx <- which(r1.poly@data$main_cod=='E')
r1.poly2 <- r1.poly[idx,]
plot(r1.poly2)

r2.poly@data$main_cod
idx <- which(r2.poly@data$main_cod=='E')
r2.poly2 <- r2.poly[idx,]
plot(r2.poly2)

setwd("C:/Users/Usuario/Documents/Francisco/coberturas/")
#writeOGR(r1.poly2, ".", "polygon_clima_koppen_E_nueva_area_estudio_geo", driver="ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(r2.poly2, ".", "polygon_clima_koppen_futuro_E_nueva_area_estudio_geo", driver="ESRI Shapefile", overwrite_layer = TRUE)

# fin ---






# reproyeccion y calculo area ----

# En QGIS

# ---





# plot ----
setwd("C:/Users/Usuario/Documents/Francisco/coberturas/")

c1 <- readOGR('.', 'polygon_climas_koppen_nueva_area_estudio_19s')
c2 <- readOGR('.', 'polygon_climas_koppen_futuro_nueva_area_estudio_19s')

# grafico
k <- c1

#plot(k, col=terrain.colors(length(k@data$id_clima)), border=terrain.colors(length(k@data$id_clima)))
head(k@data)

k@data$main_cod <- str_sub(k$cod, 1, 1)

class(k@data$main_cod)
k@data$main_cod <- as.character(k@data$main_cod)
unique(k@data$main_cod)

var1 <- k@data$main_cod

t1 <- tapply(k@data$area_km2, var1, sum) ; t1
sum(t1)
# t2 <- tapply(g.poly@data$AREA_Km2, g.poly@data$CLASIFICA, sum) ; t2
# sum(t2)

k@data$porc <- round((k@data$area_km2*100)/sum(k@data$area_km2), 3)
value <- tapply(k@data$porc, var1, sum)

class(k@data$main_cod)
k@data$main_cod <- as.factor(k@data$main_cod)
var1 <- k@data$main_cod
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "") # original
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp +  coord_polar("y", start=0)+ 
  scale_fill_manual('', values = c('#e9912c', '#f1e751', '#9dbf36', '#0d5f63'))+
  xlab("") + ylab("") +
  theme(legend.position="top") + 
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_discrete(expand = c(0, 0))

pie

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS() 
# postscript(file = "superficie_climas_koppen_glaciares_nueva_area_de_estudio.eps", height = 3.7, width = 3.7)  # Una figura de 5x10 cm
# pie
# dev.off()

# fin ---
