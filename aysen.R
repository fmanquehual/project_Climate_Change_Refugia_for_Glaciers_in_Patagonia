#library(maptools) ## checks «rgeos» availability (if so, it will be the option used)
library("RColorBrewer")
library(ggplot2)
library(rgdal)
library(rgeos)
library(raster)
require(foreign)

wgs84 <- "+proj=longlat +ellps=WGS84" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Seminario de investigacion/coberturas/Glaciares_Nacional_2015/")

g <- readOGR(".", "Glaciares_Nacional_2015")

head(g@data)
levels(g@data$REGION)

idx <- which(g@data$REGION=="AISEN DEL GENERAL CARLOS IBA�'EZ DEL CAMPO")
g.aysen <- g[idx,]
table(g.aysen@data$REGION)

head(g.aysen@data)
levels(g.aysen@data$NOMB_CUEN)
g.aysen@data$NOMB_CUEN <- gsub(" ", "_", g.aysen@data$NOMB_CUEN)

idx <- which(g.aysen@data$NOMB_CUEN=="RIO_BAKER")
g.baker <- g.aysen[idx,]
table(g.baker@data$NOMB_CUEN)

unique(g.baker@data$CLASIFICA)
g.baker@data$CLASIFICA <- as.character(g.baker@data$CLASIFICA)
g.baker@data$CLASIFICA <- sub("MONTA�'A", "MONTANA", g.baker@data$CLASIFICA)
unique(g.baker@data$CLASIFICA)

g@data$CLAS_2_CUB[g@data$CLASIFICA=="GLACIARES EFLUENTES"]

g.baker@data$CLASIFICA[is.na(g.baker@data$CLASIFICA)] <- "No Aplica"
table(g.baker@data$CLASIFICA)

head(g.baker)
table(g.baker@data$INVENT_FEC)
plot(g.baker)

# fin ---

# calculo ----
setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Seminario de investigacion/coberturas/")
c <- readOGR(".", "polygon_cuenca_baker_chile_ok_19s")
c.geo <- spTransform(c, CRSobj = wgs84)
  
plot(c, axes=T)
plot(g.baker, add=TRUE, border="red")

num.total.glaciares <- sum(unlist(table(g.baker@data$INVENT_FEC))) ; num.total.glaciares
num.glaciares.anho.2011 <- unlist(table(g.baker@data$INVENT_FEC))[5] ; num.glaciares.anho.2011
num.glaciares.anho.2010 <- unlist(table(g.baker@data$INVENT_FEC))[4] ; num.glaciares.anho.2010

(num.glaciares.anho.2011*100)/num.total.glaciares # porcentaje total de glaciares que fueron inventariados el 2011
(num.glaciares.anho.2010*100)/num.total.glaciares # porcentaje total de glaciares que fueron inventariados el 2010

area.km2.total.glaciares <- sum(g.baker@data$AREA_Km2) ; area.km2.total.glaciares
area.km2.2011.glaciares <- sum(g.baker@data$AREA_Km2[g.baker@data$INVENT_FEC==2011]) ; area.km2.2011.glaciares
area.km2.2010.glaciares <- sum(g.baker@data$AREA_Km2[g.baker@data$INVENT_FEC==2010]) ; area.km2.2010.glaciares 

(area.km2.2010.glaciares*100)/area.km2.total.glaciares # porcentaje del area ocupada por glaciares inventariados el 2010
(area.km2.2011.glaciares*100)/area.km2.total.glaciares # porcentaje del area ocupada por glaciares inventariados el 2011


setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
c.real <- readOGR(".", "polygon_cuenca_baker_chile_grass_19s")

# plot ----
lim1 <- g.baker

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
  scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Blues"))) +#scale_fill_brewer(palette="Blues") +
 # ggtitle ("Distribuci�n de superficie por\nclase morfol�gica") +
  xlab("") + ylab("") +
  theme(legend.position="top") + guides(fill = guide_legend(nrow = 2))

pie

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos")
png("piechart_superficie_clas_morfologica_2.png", width = 680, height = 420, units = "px")

pie

dev.off()
# -

n.clas.i <- as.vector(unlist(table(lim1@data$CLASIFICA)))
nm.clas.i <- names(table(lim1@data$CLASIFICA))

db2 <- data.frame(nm.clas=nm.clas.i, n.clas=n.clas.i)
db2$siglas <- c("GM", "GV", "GE", "G")
db2$leyenda <- paste("(", db2$siglas, ")", " ", db2$nm.clas,sep = "")
db2

bar <-  ggplot(data=db2, aes(x=siglas, y=n.clas, fill=leyenda, label=n.clas)) + # original
  geom_bar(stat="identity", position="dodge") +# + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  geom_text(position = position_dodge(width = 0.5), angle = 0, vjust = -.5, size = 3) +#, fontface="bold") +
  scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Blues"))) +
  xlab("Clase Morfol�gica") + ylab("N� Glaciares") +
  #ggtitle ("N�mero de glaciares seg�n\nclase morfol�gica") # original
  theme(legend.position="none") #+ guides(fill = guide_legend(nrow = 1))# hecho para el ppt 
bar

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos")
png("barchart_numero_glaciares_clas_morfologica_2.png", width = 680, height = 420, units = "px")

bar

dev.off()

#---


#library(gridExtra)

 setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos")
 png("bar_pie_chart_glaciares_clas_morfologica.png", width = 680, height = 420, units = "px")
 
grid.arrange(bar, pie,
             ncol = 2, nrow = 1)
 dev.off()








# Pais y region ----
# setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Seminario de investigacion/coberturas/DivisionPoliticaAdministrativa2019")
# ch <- readOGR(".", "DivisionPoliticaAdministrativa2019")

setwd("C:/Users/Pancho/Documents/Seminario de investigacion/coberturas")
ch2 <- readOGR(".", "polygon_regiones_19s")
ch2.geo <- spTransform(ch2, CRSobj = wgs84)
ch2.geo@data

idx <- which(ch2@data$NOMBRE=="aisenDelGralCarlosIbanhezDelCampo")
aysen <- ch2[idx,]
aysen.geo <- spTransform(aysen, CRSobj = wgs84)

# aysen19s <- spTransform(aysen, CRSobj = utm19)
# head(aysen19s@data)
# 
# plot(aysen19s)
# marco.aysen <- draw
#   
# plot(c, add=T, border="red")
# text(aysen19s, labels="COMUNA", col="red", cex=1.2)
# ---


# Mapa ubicacion ----
setwd("C:/Users/Pancho/Documents/Practica_profesional/coberturas")

nac <- readOGR(".", "mundo_naciones")
head(nac@data)

nac@data$ESPAñoL <- as.character(nac@data$ESPAñoL)
sort(nac@data$ESPAñoL)


paises <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Ecuador", 
            "Guyana", "Paraguay", "Perú", "Trinidad y Tobago", "Surinam",
            "Uruguay", "Venezuela", "Guayana Francesa")#, "Costa Rica", "Cuba", 
            #"El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", 
            #"Puerto Rico", "República Dominicana")

sud.id <- which(nac@data$ESPAñoL%in%paises)
sud <- nac[sud.id,]
table(sud@data$ESPAñoL)

plot(sud)
marco.sud <- drawExtent()

sud.clip <- crop(sud, marco.sud)

# marcos ----

plot(sud)
marco.ch <- drawExtent()
  
plot(ch2.geo)
plot(aysen.geo, add=TRUE, border="red")
marco.reg <- drawExtent()

plot(aysen.geo)
plot(c.geo, add=TRUE, border="red")
marco.c <- drawExtent()
# ---


# plot ubicacion ----

png("mapa_ubicacion_cuenca.png")
setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos")

par(oma=c(2,2,2,2), mar=c(0,0,0,0))

layout(matrix(c(1,2,
                1,3), 2, 2, byrow = TRUE))



plot(sud.clip)
mtext("A)", font = 1, line = -1.1, adj = 0.98)
plot(marco.ch, add=TRUE, col="red")
axis(1)
axis(2)
box()

plot(ch2.geo)
mtext("B)", font = 1, line = -1.1, adj = 0.98)
plot(marco.reg, add=TRUE, col="red")
axis(3)
axis(4)
box()

plot(aysen.geo)
plot(marco.c, add=TRUE, col="red")
mtext("C)", font = 1, line = -1.1, adj = 0.98)
axis(1)
axis(4)
box()

plot(c.geo, add=TRUE, border="red", lty=2)

dev.off()

#xaxt="n"
# ---


# catastro BN ----
setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas")
cbn <- read.dbf("polygon_cbn_cuenca_baker_19s.dbf")
#plot(cbn)

# correcion nombres ----
db.cbn <- cbn
head(db.cbn)

levels(db.cbn$USO_TIERRA)
sum(db.cbn$area_km2)

class(db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- as.character(db.cbn$USO_TIERRA)
unique(db.cbn$USO_TIERRA)

#db.cbn$USO_TIERRA <- sub("??adis Herbï¿½ceos y Arbustivos", "Nadis", db.cbn$USO_TIERRA)
#db.cbn$USO_TIERRA <- gsub("??Nadis Herbaceos y Arbustivos", "Nadis Herbaceos y Arbustivos", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA[db.cbn$USO_TIERRA%in%"??Nadis Herbaceos y Arbustivos"] <- "Nadis Herbaceos y Arbustivos"

#db.cbn$USO_TIERRA <- gsub("?reas", "Areas", db.cbn$USO_TIERRA)
#db.cbn$USO_TIERRA <- gsub("?AAreas", "Areas", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA[db.cbn$USO_TIERRA%in%"?Areas Sobre Limite Vegetacion"] <- "Areas Sobre Limite Vegetacion"

db.cbn$USO_TIERRA <- gsub("Herbï¿½ceos", "Herbaceos", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Lï¿½mite", "Limite", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Vegetaciï¿½n", "Vegetacion", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Exï¿½ticas", "Exoticas", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Plantaciï¿½n", "Plantacion", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Rï¿½os", "Rios", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Vegetaciï¿½n", "Vegetacion", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Patagï¿½nica", "Patagonica", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Minerï¿½a", "Mineria", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Hï¿½medos", "Humedos", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Joven-Reciï¿½n", "Joven-Recien", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Rï¿½os", "Rios", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Rotaciï¿½n", "Rotacion", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Agrï¿½cola", "Agricola", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("Herbï¿½cea", "Herbacea", db.cbn$USO_TIERRA)
db.cbn$USO_TIERRA <- gsub("rï¿½os", "rios", db.cbn$USO_TIERRA)

class(db.cbn$USO_TIERRA)
unique(db.cbn$USO_TIERRA)

#write.dbf(db.cbn, "polygon_cbn_cuenca_baker_19s")
#--


# new names land use ----
unique(db.cbn$USO_TIERRA)
db.cbn$id <- 1:nrow(db.cbn)
db.cbn$USO_2 <- "NA"

db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Resto de la cuenca"] <- "Resto de la cuenca"

db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Ciudades-Pueblos-Zonas Industriales"] <- "Ciudades-Pueblos-Zonas Industriales"

db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Terrenos de Uso Agricola" ] <- "Uso Agricola"
  
db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Praderas Perennes"] <- "Praderas"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Pradera Denso" ] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Pradera Abierto"] <- "Matorral" 
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Achaparrado Semidenso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Plantacion Semidenso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Exoticas Asilvestradas Semidenso"] <- "Bosque Exoticas Asilvestradas"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Vegetacion Herbacea en orillas de rios"] <- "Praderas"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Turbales"] <- "Turbales"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Otros Terrenos Humedos"] <- "Otros Terrenos Humedos"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Afloramientos Rocosos"] <- "Afloramientos Rocosos"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Corridas de Lava y Escoriales"] <- "Corridas de Lava y Escoriales"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Otros Terrenos Sin Vegetacion"] <- "Otros Terrenos Sin Vegetacion"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Nieves"] <- "Nieves"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Campos de Hielo"] <- "Campos de Hielo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Lago-Laguna-Embalse-Tranque"] <- "Cuerpos de Agua"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Mineria Industrial"] <- "Mineria Industrial"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Rotacion Cultivo-Pradera"] <- "Rotacion Cultivo-Pradera"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Estepa Patagonica"] <- "Estepa Patagonica"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Pradera Semidenso"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Denso"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Abierto"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Semidenso"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Arborescente Denso"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Arborescente Semidenso"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Arborescente Muy Abierto"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Muy Abierto"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Adulto Abierto"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Renoval Semidenso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Plantacion Joven-Recien Cosechada"] <- "Plantacion"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Adulto Denso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Matorral Arborescente Abierto"] <- "Matorral"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Plantacion"] <- "Plantacion"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosques Exoticas Asilvestradas"] <- "Bosque Exoticas Asilvestradas"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Adulto Semidenso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Renoval Denso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Renoval Abierto"] <- "Bosque Nativo" 
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Adulto-Renoval Semidenso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Achaparrado Denso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Achaparrado Abierto"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Plantacion Abierto"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Exoticas Asilvestradas Abierto"] <- "Bosque Exoticas Asilvestradas"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Nadis Herbaceos y Arbustivos"] <- "Nadis"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Vegas"] <- "Vegas"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Playas y Dunas"] <- "Playas y Dunas"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Areas Sobre Limite Vegetacion"] <- "Areas Sobre Limite Vegetacion"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Derrumbes Sin Vegetacion"] <- "Derrumbes Sin Vegetacion"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Cajas de Rios"] <- "Cajas de Rios"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Glaciares"] <- "Glaciares"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Rios"] <- "Cuerpos de Agua"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Adulto-Renoval Denso"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Bosque Nativo Adulto-Renoval Abierto"] <- "Bosque Nativo"
  
  db.cbn$USO_2[db.cbn$USO_TIERRA%in%"Renoval"] <- "Bosque Nativo"
  
head(db.cbn)
unique(db.cbn$USO_2)
table(db.cbn$USO_2)

db.cbn$USO_TIERRA[db.cbn$USO_2%in%"NA"]
tab <- tapply(db.cbn$area_km2, db.cbn$USO_2, sum)

db.cbn2 <- data.frame(USO_2=names(tab), area_km2=as.vector(unlist(tab)))
db.cbn2

#cbn@data <- db.cbn
var1 <- db.cbn2$USO_2

a.cuenca <- 20944.9
head(db.cbn2)

#db.cbn$area_km2 <- round(cbn@data$area_ha*0.01, 2)
db.cbn2$porc <- round((db.cbn2$area_km2*100)/a.cuenca, 2)


#write.dbf(cbn, "polygon_cbn_cuenca_baker_19s")

t2 <- tapply(db.cbn2$area_km2, var1, sum) ; t2

sum(t2)

value <- tapply(db.cbn2$porc, var1, sum)

class(db.cbn2$USO_2)
db.cbn2$USO_2 <- as.factor(db.cbn2$USO_2)
var1 <- db.cbn2$USO_2
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)+ 
  #scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Black"))) +#scale_fill_brewer(palette="Blues") +
  ggtitle ("Distribuci�n de superficie por\nuso de tierra (2011)") +
  xlab("") + ylab("") 

pie

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos")
#png("piechart_superficie_uso_tierra.png", width = 670, height = 670, units = "px")

pie

dev.off()
