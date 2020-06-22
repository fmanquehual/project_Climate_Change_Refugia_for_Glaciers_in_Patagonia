library('rgdal')
library('rgeos')
library('raster')
library('ggplot2')
library('RColorBrewer')

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

#setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
#g <- readOGR(".", "polygon_glaciares_cuenca_baker_19s") # baker
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/Glaciares_Nacional/')
g <- readOGR(".", "polygon_glaciares_marco_trabajo_nuevo_geo")

# setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Seminario de investigacion/coberturas/")
# c <- readOGR(".", "polygon_cuenca_baker_chile_ok_19s") # baker
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
c <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')

# setwd("C:/Users/Francisco/Downloads/")
# r <- raster("CHELSA_prec_2012_01_clip_19s.tif")  # baker
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/2050_2070/')
r.obj <- raster('CCSM4_pp_winter_mean_rcp45_2050.tif')

plot(r.obj)
plot(c, border='red', lwd=2, add=TRUE)

# fin ---








# ----

head(g@data)
levels(g@data$REGION)

levels(g@data$NOMB_CUEN)
#g@data$NOMB_CUEN <- gsub(" ", "_", g@data$NOMB_CUEN)
#unique(g.baker@data$CLASIFICA)

g@data$CLASIFICA <- as.character(g@data$CLASIFICA)
g@data$CLASIFICA <- sub("MONTAï¿½A", "MONTANA", g@data$CLASIFICA)
unique(g@data$CLASIFICA)

g@data$CLAS_2_CUB[g@data$CLASIFICA=="GLACIARES EFLUENTES"]

g@data$CLASIFICA[is.na(g@data$CLASIFICA)] <- "No Aplica"
table(g@data$CLASIFICA)

head(g)
table(g@data$INVENT_FEC)

plot(c, axes=T)
plot(g, add=TRUE, border="red")

num.glaiares.i <- unlist(table(g@data$INVENT_FEC)) ; num.glaiares.i

num.total.glaciares <- sum(num.glaiares.i) ; num.total.glaciares
num.glaciares.anho.2011 <- num.glaiares.i[3] ; num.glaciares.anho.2011
num.glaciares.anho.2010 <- num.glaiares.i[2] ; num.glaciares.anho.2010
num.glaciares.anho.2009 <- num.glaiares.i[1] ; num.glaciares.anho.2009

round((num.glaciares.anho.2011*100)/num.total.glaciares, 3) # porcentaje total de glaciares que fueron inventariados el 2011
round((num.glaciares.anho.2010*100)/num.total.glaciares, 3) # porcentaje total de glaciares que fueron inventariados el 2010
round((num.glaciares.anho.2009*100)/num.total.glaciares, 3) # porcentaje total de glaciares que fueron inventariados el 2009

areas.i <- round(unlist(tapply(g@data$AREA_Km2, g@data$INVENT_FEC, sum)), 3) ; areas.i

area.km2.total.glaciares <- sum(areas.i) ; area.km2.total.glaciares
area.km2.2011.glaciares <- areas.i[3] ; area.km2.2011.glaciares
area.km2.2010.glaciares <- areas.i[2] ; area.km2.2010.glaciares
area.km2.2009.glaciares <- areas.i[1] ; area.km2.2009.glaciares


round((area.km2.2011.glaciares*100)/area.km2.total.glaciares, 3) # porcentaje del area ocupada por glaciares inventariados el 2011
round((area.km2.2010.glaciares*100)/area.km2.total.glaciares, 3) # porcentaje del area ocupada por glaciares inventariados el 2010
round((area.km2.2009.glaciares*100)/area.km2.total.glaciares, 3) # porcentaje del area ocupada por glaciares inventariados el 2009



# plot ----
idx <- which(g@data$INVENT_FEC==2011)
g2 <- g[idx,]
lim1 <- g2

class(lim1@data$CLASIFICA)
unique(lim1@data$CLASIFICA)

var1 <- lim1@data$CLASIFICA

head(lim1@data)

lim1@data$area_ha <- round(lim1@data$AREA_Km2*100, 3)
lim1@data$porc <- round((lim1@data$AREA_Km2*100)/sum(lim1@data$AREA_Km2), 3)

t1 <- tapply(lim1@data$AREA_Km2, var1, sum) ; t1
t2 <- tapply(lim1@data$area_ha, var1, sum) ; t2

sum(t1)
sum(t2)

value <- tapply(lim1@data$porc, var1, sum)

class(lim1@data$CLASIFICA)
lim1@data$CLASIFICA <- as.factor(lim1@data$CLASIFICA)
var1 <- lim1@data$CLASIFICA
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$siglas <- c("GM", "GV", "GE", "G") # hecho para la ppt
db$leyenda <- paste("(", db$siglas, ")"," " , db$porc, "%", sep = "") # hecho para la ppt
#db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "") # original
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)+ 
  scale_fill_manual('', values=rev(brewer.pal(n = nrow(db), name = "Blues"))) +#scale_fill_brewer(palette="Blues") +
  #ggtitle ("Distribución de superficie por\nclase morfológica") +
  xlab("") + ylab("") +
  theme(legend.position="right") + 
  guides(fill = guide_legend(nrow = 4)) +
  scale_x_discrete(expand = c(0, 0))

pie

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS() 
# postscript(file = "dist_superficie_glaciares_2011_2.eps")#, height = 3, width = 5)
# pie
# dev.off()

# fin ---








# poligono area de estudio chi-arg ----
rasterize.c <- rasterize(c, r.obj, fun="last", background=NA)
rasterize.obj <- mask(rasterize.c, r.obj)
plot(rasterize.obj)

poly.obj <- rasterToPolygons(rasterize.obj, dissolve = TRUE)
plot(poly.obj)

setwd("C:/Users/Usuario/Documents/Francisco/coberturas/")
#writeOGR(poly.obj, ".", "polygon_chile_area_de_estudio_geo", driver="ESRI Shapefile", overwrite_layer = TRUE)

# fin ---










# puntos de presencia ----
idx <- which(g$ZONA_GLACI=='ZONA SUR')
g2 <- g[idx,]
plot(g2)

g2@data$presencia <- 1
head(g2@data)

# Rasterizando columna de presencia 
g.rasterizado <- rasterize(g2, r.obj, field="presencia", fun="last", background=NA)

plot(g.rasterizado, col='red')
plot(poly.obj, add=TRUE)

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
# writeRaster(g.rasterizado2, filename='glaciares_marco_trabajo_nuevo_geo.tif', format="GTiff", overwrite=TRUE)


# Raster a puntos de presencia ----
g.presencia <- rasterToPoints(g.rasterizado)
class(g.presencia)
head(g.presencia)

g.presencia.db <- as.data.frame(g.presencia)
g.presencia.db$id <- 1:nrow(g.presencia.db)
head(g.presencia.db)

g.presencia.pre.shp <- SpatialPoints(c(g.presencia.db[1],g.presencia.db[2]), proj4string = CRS(wgs84))
g.presencia.shp.db <- data.frame(id= c(1:nrow(g.presencia.db)), presencia = 1,x=coordinates(g.presencia.pre.shp)[,1], y=coordinates(g.presencia.pre.shp)[,2])
g.presencia.shp <- SpatialPointsDataFrame(g.presencia.pre.shp, data = g.presencia.shp.db, match.ID = TRUE)

g.presencia.shp

plot(g.presencia.shp, col='red', axes=TRUE, pch=16, cex=0.1)
plot(poly.obj, add=TRUE)

# setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/var_presencia/")
# writeOGR(g.presencia.shp, ".", "points_presencia_glaciares_zona_glaciologica_sur", driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(g.presencia.shp, ".", "points_presencia_glaciares_zona_glaciologica_austral", driver="ESRI Shapefile", overwrite_layer = TRUE)

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/var_presencia/')
#writeOGR(g.presencia.shp, ".", "points_presencia_glaciares_completo", driver="ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(g.presencia.shp, ".", "points_presencia_glaciares_anho_2011", driver="ESRI Shapefile", overwrite_layer = TRUE)
