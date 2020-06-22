library('raster')
library('rgdal')
library('rgeos')

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " #coordenadas geograficas WGS84
wgs84.19s <- '+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0' 

setwd('C:/Users/Francisco/Downloads/')

g.ae <- readOGR('.', 'polygon_glaciares_marco_trabajo_nuevo_19s')
table(g.ae@data$CLASIFICA)

g.ae@data$CLASIFICA <- gsub('GLACIAR DE MONTAï¿½A', 'GLACIAR DE MONTANA', g.ae@data$CLASIFICA)
table(g.ae@data$CLASIFICA)

idx <- which(g.ae@data$CLASIFICA=='GLACIAR DE MONTANA')
g.ae.montanhas <- g.ae[idx,]
table(g.ae.montanhas@data$CLASIFICA)

area.gm.ae <- gArea(g.ae.montanhas)/1000000



setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Seminario de investigacion/coberturas/Glaciares_Nacional_2015/')
g <- readOGR('.', 'Glaciares_Nacional_2015')

levels(g@data$CLASIFICA)

g@data$CLASIFICA <- gsub('GLACIAR DE MONTAÃ'A', 'GLACIAR DE MONTANA', g@data$CLASIFICA)

idx <- which(g@data$CLA1SIFICA=='GLACIAR DE MONTANA')
g.montanhas <- g[idx,]
table(g.montanhas@data$CLASIFICA)
area.gm <- sum(g.montanhas@data$AREA_Km2)

(area.gm.ae*100)/area.gm

# ---

tapply(g@data$AREA_Km2, g@data$ZONA_GLACI, sum)
tapply(g.montanhas@data$AREA_Km2, g.montanhas@data$ZONA_GLACI, sum)
