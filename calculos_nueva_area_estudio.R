library('raster')
library('rgdal')
library('rgeos')
library('ggplot2')

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/Glaciares_Nacional/')

g <- readOGR('.', 'polygon_glaciares_marco_trabajo_nuevo_19s')
head(g@data)
num.glaciares <- sum(as.vector(unlist(table(g@data$INVENT_FEC)))) ; num.glaciares
(2545*100)/num.glaciares # glaciares del 2009
(3278*100)/num.glaciares # glaciares del 2010
(4941*100)/num.glaciares # glaciares del 2011

class(g@data$CLASIFICA)
g@data$CLASIFICA <- as.character(g@data$CLASIFICA)
unique(g@data$CLASIFICA)

g@data$CLASIFICA <- gsub('ï¿½', 'N', g@data$CLASIFICA)

var1 <- g@data$CLASIFICA

t1 <- tapply(g@data$area_km2_2, var1, sum) ; t1
sum(t1)
# t2 <- tapply(g.poly@data$AREA_Km2, g.poly@data$CLASIFICA, sum) ; t2
# sum(t2)

g@data$porc <- round((g@data$area_km2_2*100)/sum(g@data$area_km2_2), 3)
value <- tapply(g@data$porc, var1, sum)

class(var1)
var1 <- as.factor(var1)
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "") # original
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp +  coord_polar("y", start=0)+ 
  scale_fill_manual('', values = c('#122F4D', '#385D85', '#8DA3B9', '#E3E8ED'))+
  xlab("") + ylab("") +
  theme(legend.position="top") + 
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_discrete(expand = c(0, 0))

pie

# area glaciares Arg (km2) ----
arg.nor.pat.area <- 121.77 + 1.2 + 110.23 + 8.12 + 9.2 ; arg.nor.pat.area
arg.sur.pat.area <- 5.58 + 41.75 + 25.02 ; arg.sur.pat.area
sum(arg.nor.pat.area, arg.sur.pat.area)

# numero de glaciares
arg.nor.pat.num <- 505 + 53 + 780 + 151 + 241 ; arg.nor.pat.num
arg.sur.pat.num <- 43 + 211 + 181 ; arg.sur.pat.num

# apuntes
# Región de los Andes del norte de la Patagonia (35° - 45°S)
# Región de los Andes del sur de la Patagonia (45° - 54°S)
# Fuente: atlas de glaciares de la argentina







# area de glaciares segun clima ----
setwd("C:/Users/Usuario/Documents/Francisco/coberturas/")

climas <- readOGR('.', 'clip_polygon_climas_koppen_nueva_area_estudio_con_area_19s')
plot(climas)

class(climas@data$main_cod)
climas@data$main_cod <- as.character(climas@data$main_cod)
unique(climas@data$main_cod)

var1 <- climas@data$main_cod

t1 <- tapply(climas@data$area_km2, var1, sum) ; t1
sum(t1)

climas@data$porc <- round((climas@data$area_km2*100)/sum(climas@data$area_km2), 3)
value <- tapply(climas@data$porc, var1, sum)

class(var1)
var1 <- as.factor(var1)
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "") # original
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp +  coord_polar("y", start=0)+ 
  scale_fill_manual('', values = c('#122F4D', '#385D85', '#8DA3B9', '#E3E8ED'))+
  xlab("") + ylab("") +
  theme(legend.position="top") + 
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_discrete(expand = c(0, 0))

pie
