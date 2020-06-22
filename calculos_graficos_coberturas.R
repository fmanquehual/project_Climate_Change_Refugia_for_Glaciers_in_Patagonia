library("RColorBrewer")
library(ggplot2)
require(foreign)
library("gridExtra")

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

# geomorfologica ----
geomorfologica <- read.dbf("polygon_unidades_geomorfologicas_baker_ok_19s.dbf")
head(geomorfologica)

geomorfologica <- geomorfologica[,c(1, 3, 4)]
str(geomorfologica)
head(geomorfologica)

levels(geomorfologica$unidad)
geomorfologica$unidad <- as.character(geomorfologica$unidad)
geomorfologica$unidad[geomorfologica$unidad%in%"Cordillera patagonica andina de fjordos"] <- "Cordillera patagonica andina de fiordos"
unique(geomorfologica$unidad)


# grafico ----
var1 <- geomorfologica$unidad

geomorfologica$porc <- round((geomorfologica$area_km2*100)/sum(geomorfologica$area_km2), 2)

t1 <- tapply(geomorfologica$area_km2, var1, sum) ; t1
t2 <- tapply(geomorfologica$area_ha, var1, sum) ; t2

sum(t1)
sum(t2)

value <- tapply(geomorfologica$porc, var1, sum)

class(geomorfologica$unidad)
geomorfologica$unidad <- as.factor(geomorfologica$unidad)
var1 <- geomorfologica$unidad
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)+ 
  scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Greys"))) +#scale_fill_brewer(palette="Blues") +
  ggtitle ("Distribución de superficie por\nunidad geomorfológica") +
  xlab("") + ylab("") 

pie

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
#png("piechart_superficie_unidad_geomorfologica.png", width = 680, height = 450, units = "px")

pie

dev.off()
# ---



setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

# geologica ----

geologica <- read.dbf("polygon_geologia_baker_19s.dbf")

head(geologica)
str(geologica)

geologica$composicio <- as.character(geologica$composicio)
unique(geologica$composicio)

# grafico ----
var1 <- geologica$composicio

geologica$porc <- round((geologica$area_km2*100)/sum(geologica$area_km2), 2)

t1 <- tapply(geologica$area_km2, var1, sum) ; t1
sum(t1)


value <- tapply(geologica$porc, var1, sum)

class(geologica$composicio)
geologica$composicio <- as.factor(geologica$composicio)
var1 <- geologica$composicio
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)+ 
  scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Greys"))) +#scale_fill_brewer(palette="Blues") +
  ggtitle ("Distribución de superficie por\ncomposición") +
  xlab("") + ylab("") 

pie

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
#png("piechart_superficie_composicion_geologica.png", width = 680, height = 450, units = "px")

pie

dev.off()
# ---



setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

# hotspot ----
ht <- read.dbf("polygon_clip_hotspot_baker_19s.dbf")
head(ht)

a.ht <- ht$area_km2
a.cuenca <- 28050.48 

(a.ht*100)/a.cuenca # area que ocupa el hotspot dentro de la cuenca (chilena) en %



setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
# snaspe ----
pn <- read.dbf("polygon_snaspe_baker_clip_19s.dbf")
pn <- pn[,c('NOMBRE', 'km2')]
head(pn)
str(pn)

pn.cuenca <- data.frame(NOMBRE='RESTO DE LA CUENCA', km2=a.cuenca-sum(pn$km2))
pn <- rbind(pn, pn.cuenca)

pn$NOMBRE <- as.character(pn$NOMBRE)
unique(pn$NOMBRE)

sum(pn$km2[c(1,2,3)]) # superficie total de AP

# grafico ----
var1 <- pn$NOMBRE

pn$porc <- round((pn$km2*100)/a.cuenca, 2)

t1 <- tapply(pn$km2, var1, sum) ; t1
sum(t1) # total cuenca

value <- tapply(pn$porc, var1, sum)

class(pn$NOMBRE)
pn$NOMBRE <- as.factor(pn$NOMBRE)
var1 <- pn$NOMBRE
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)+ 
  scale_fill_manual(values=brewer.pal(n = nrow(db), name = "Greys")) +#scale_fill_brewer(palette="Blues") +
  ggtitle ("Distribución de superficie por\nÁrea protegida") +
  xlab("") + ylab("") 

pie

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
#png("piechart_superficie_areas_protegidas.png", width = 680, height = 450, units = "px")

pie

dev.off()
# ---


setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
# tipos forestales ----

tf <- read.dbf("polygon_cbn_cuenca_baker_19s.dbf")
head(tf)

levels(tf$TIPO_FORES)
class(tf$TIPO_FORES)
tf$TIPO_FORES <- as.character(tf$TIPO_FORES)

tf$TIPO_FORES[tf$TIPO_FORES%in%"Ciprï¿½s de las Guaitecas"] <- "Cipres de las Guaitecas"
unique(tf$TIPO_FORES)

tf$TIPO_FORES[tf$TIPO_FORES%in%NA] <- "Resto de la cuenca"
unique(tf$TIPO_FORES)

# grafico ----
var1 <- tf$TIPO_FORES

tf$porc <- round((tf$area_km2*100)/sum(tf$area_km2), 2)

t1 <- tapply(tf$area_km2, var1, sum) ; t1
sum(t1) # total cuenca

# ---
dim(tf)
tf2 <- subset(tf, TIPO_FORES!='Resto de la cuenca')
dim(tf2)

a.tf <- sum(tf2$area_km2)

a.cuenca <- 28050.48 

(a.tf*100)/a.cuenca # area que ocupa los tipos forestales dentro de la cuenca (chilena) en %

# ---

value <- tapply(tf$porc, var1, sum)

class(tf$TIPO_FORES)
tf$TIPO_FORES <- as.factor(tf$TIPO_FORES)
var1 <- tf$TIPO_FORES
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)+ 
  scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Greys"))) +#scale_fill_brewer(palette="Blues") +
  ggtitle ("Distribución de superficie por\ntipo forestal") +
  xlab("") + ylab("") 

pie

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
#png("piechart_superficie_tipo_forestal.png", width = 680, height = 450, units = "px")

pie

dev.off()
# ---






setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/coberturas_argentina/")
# AP Chi-Arg ----
a.cuenca <- 28050.48
ap <- read.dbf("polygon_areas_protegidas_baker_chi_arg_clip_19s.dbf")

head(ap)
ap$PAIS2 <- "NA"
ap$PAIS2[ap$PAIS%in%"Argentina"] <- "(Arg)"
ap$PAIS2[ap$PAIS%in%"Chile"] <- "(Chi)"
ap$NOMBRE2 <- paste(ap$NOMBRE, ap$PAIS2, sep=" ")

head(ap)
ap <- ap[,c("NOMBRE2","area_km2")]
head(ap)
str(ap)

ap.cuenca <- data.frame(NOMBRE2='Resto de la cuenca', area_km2=a.cuenca-sum(ap$area_km2))
ap <- rbind(ap, ap.cuenca)

ap$NOMBRE2 <- as.character(ap$NOMBRE2)
unique(ap$NOMBRE2)

sum(ap$area_km2[c(1:7)]) # superficie total de AP

# grafico ----
var1 <- ap$NOMBRE2

ap$porc <- round((ap$area_km2*100)/a.cuenca, 2)

t1 <- tapply(ap$area_km2, var1, sum) ; t1

value <- tapply(ap$porc, var1, sum)

class(ap$NOMBRE2)
ap$NOMBRE2 <- as.factor(ap$NOMBRE2)
var1 <- ap$NOMBRE2
name <- levels(var1)

db <- data.frame(uso=name, porc=value)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)+ 
  scale_fill_manual(values=brewer.pal(n = nrow(db), name = "Greys"))+ #scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Greys"))) +#scale_fill_brewer(palette="Blues") +
  ggtitle ("Distribución de superficie por\nárea protegida") +
  xlab("") + ylab("") 

pie

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
#png("piechart_superficie_areas_protegidas_chi_arg.png", width = 680, height = 450, units = "px")

pie

dev.off()
# ---




setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
# CBN CHI ----
a.cuenca <- 28050.48

cbn.ch <- read.dbf("polygon_cbn_cuenca_baker_Chile_19s.dbf")
head(cbn.ch)

levels(cbn.ch$ID_USO)
class(cbn.ch$ID_USO)
cbn.ch$ID_USO <- as.character(cbn.ch$ID_USO)
cbn.ch$USO_SUELO <- "NA"

cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"01"] <- "Areas Urbanas e Industriales" 

  cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"02"] <- "Terrenos Agricolas" 

  cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"03"] <- "Praderas y Matorrales" 

  cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"04"] <- "Bosques" 

  cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"05"] <- "Humedales" 

  cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"06"] <- "Areas Desprovistas de Vegetacion" 

  cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"07"] <- "Nieves Eternas y Glaciares" 

  cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"08"] <- "Cuerpos de Agua" 

  cbn.ch$USO_SUELO[cbn.ch$ID_USO%in%"09"] <- "Areas No Reconocidas"

  head(cbn.ch)
  
  # grafico ----
  var1 <- cbn.ch$USO_SUELO
  
  cbn.ch$porc <- round((cbn.ch$area_km2*100)/a.cuenca, 2)
  
  t1 <- tapply(cbn.ch$area_km2, var1, sum) ; t1
  
  value <- tapply(cbn.ch$porc, var1, sum)
  
  class(cbn.ch$USO_SUELO)
  cbn.ch$USO_SUELO <- as.factor(cbn.ch$USO_SUELO)
  var1 <- cbn.ch$USO_SUELO
  name <- levels(var1)
  
  db <- data.frame(uso=name, porc=value)
  row.names(db) <- 1:nrow(db)
  db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
  db
  
  bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
    geom_bar(width = 1, stat = "identity")
  
  pie <- bp + coord_polar("y", start=0)+ 
    scale_fill_manual(values=brewer.pal(n = nrow(db), name = "Greys"))+ #scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Greys"))) +#scale_fill_brewer(palette="Blues") +
    ggtitle ("Distribución de superficie por\nuso de suelo en el lado chileno") +
    xlab("") + ylab("") 
  
  pie
  
  setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
  #png("piechart_superficie_uso_suelo_lado_chileno.png", width = 680, height = 450, units = "px")
  
  pie
  
  dev.off()
  # ---
  
  
  setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/coberturas_argentina/")
  # CBN ARG ----
  a.cuenca <- 28050.48
  
  cbn.arg <- read.dbf("polygon_cbn_baker_argentina_19s.dbf")
  head(cbn.arg)
  levels(cbn.arg$USO_SUELO)
  
  # grafico ----
  var1 <- cbn.arg$USO_SUELO
  
  cbn.arg$porc <- round((cbn.arg$area_km2*100)/a.cuenca, 2)
  
  t1 <- tapply(cbn.arg$area_km2, var1, sum) ; t1
  
  value <- tapply(cbn.arg$porc, var1, sum)
  
  class(cbn.arg$USO_SUELO)
  cbn.ch$USO_SUELO <- as.factor(cbn.ch$USO_SUELO)
  var1 <- cbn.arg$USO_SUELO
  name <- levels(var1)
  
  db <- data.frame(uso=name, porc=value)
  row.names(db) <- 1:nrow(db)
  db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
  db
  
  bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
    geom_bar(width = 1, stat = "identity")
  
  pie <- bp + coord_polar("y", start=0)+ 
    scale_fill_manual(values=brewer.pal(n = nrow(db)+1, name = "Greys"))+ #scale_fill_manual(values=rev(brewer.pal(n = nrow(db), name = "Greys"))) +#scale_fill_brewer(palette="Blues") +
    ggtitle ("Distribución de superficie por\nuso de suelo en el lado argentino") +
    xlab("") + ylab("") 
  
  pie
  
  setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
  #png("piechart_superficie_uso_suelo_lado_argentino.png", width = 680, height = 450, units = "px")
  
  pie
  
  dev.off()
  # ---
  
  
  
 
  setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/clasificacion_koppen/") 
  # clima koppen ----
  
  clim.pre0.p <- read.dbf("polygon_Beck_KG_V1_present_clip_19s.dbf")
  head(clim.pre0.p)
  
  clim.pre.p <- subset(clim.pre0.p, DN!=0)
  head(clim.pre.p)
  sum(clim.pre.p$area_km2)
  
  clim.pre0.f <- read.dbf("polygon_Beck_KG_V1_future_clip_19s.dbf")
  head(clim.pre0.f)
  
  clim.pre.f <- subset(clim.pre0.f, DN!=0)
  head(clim.pre.f)
  sum(clim.pre.f$area_km2)
  
  cod <- read.csv("codificacion_koppen_para_join.csv")
  names(cod) <- c("DN", "cod", "class")
  
  clim.p <- merge(clim.pre.p, cod, by="DN")
  clim.f <- merge(clim.pre.f, cod, by="DN")
  
  
  
  # presente ----
  
  clim.p$cod <- as.character(clim.p$cod)
  
  name.i <- names(tapply(clim.p$area_km2, clim.p$cod, sum))
  value.i <- unlist(tapply(clim.p$area_km2, clim.p$cod, sum))
  var1 <- name.i
  
  clim.p.new <- data.frame(cod=name.i, area_km2=value.i)
  row.names(clim.p.new) <- 1:nrow(clim.p.new)
  clim.p.new
  
  clim.p.new$porc <- round((clim.p.new$area_km2*100)/sum(clim.p.new$area_km2), 2)
  
  value <- tapply(clim.p.new$porc, var1, sum)

  db <- data.frame(uso=name.i, porc=value)
  row.names(db) <- 1:nrow(db)
  db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
  db
  
  col.p <- c("#D7DF01", "#F7FE2E", "#FF8000", "#BEF781", "#FE2E2E", "#04B404", "#8904B1", "#2EFEF7", "#A4A4A4", "#FFFFFF")
  
  bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
    geom_bar(width = 1, stat = "identity")
  
#  library(viridis) 
  
  pie.p <- bp + coord_polar("y", start=0)+ 
    # scale_color_viridis(discrete = TRUE)+
    # scale_fill_viridis(discrete = TRUE, direction=-1, option = "D") +
    scale_fill_manual(values=col.p) +#+scale_fill_grey(start = 0.8, end = 0.2) +#scale_fill_brewer(palette="Blues") +
    ggtitle ("(A): Distribución de superficie según\nclima actual") +
    xlab("") + ylab("") 
  
  pie.p
  
# fin ---

  
  
  # futuro ----
  
  clim.f$cod <- as.character(clim.f$cod)
  
  name.i <- names(tapply(clim.f$area_km2, clim.f$cod, sum))
  value.i <- unlist(tapply(clim.f$area_km2, clim.f$cod, sum))
  var1 <- name.i
  
  clim.f.new <- data.frame(cod=name.i, area_km2=value.i)
  row.names(clim.f.new) <- 1:nrow(clim.f.new)
  clim.f.new
  
  clim.f.new$porc <- round((clim.f.new$area_km2*100)/sum(clim.f.new$area_km2), 2)
  
  value <- tapply(clim.f.new$porc, var1, sum)
  
  db <- data.frame(uso=name.i, porc=value)
  row.names(db) <- 1:nrow(db)
  db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
  db
  
  col.f <- c("#D7DF01", "#F7FE2E", "#FF8000", "#BEF781", "#FE2E2E", "#04B404", "#8904B1", "#2EFEF7", "#FFFFFF")
  
  bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
    geom_bar(width = 1, stat = "identity")
  
  #  library(viridis) 
  
  pie.f <- bp + coord_polar("y", start=0)+ 
    # scale_color_viridis(discrete = TRUE)+
    # scale_fill_viridis(discrete = TRUE, direction=-1, option = "D") +
    scale_fill_manual(values=col.f) +#+scale_fill_grey(start = 0.8, end = 0.2) +#scale_fill_brewer(palette="Blues") +
    ggtitle ("(B): Distribución de superficie según\nclima futuro") +
    xlab("") + ylab("") 
  
  pie.f
  
  # fin ---
  
  
  
  # diferencia p-f ----
  
  clim.p.new
  clim.f.new
  
  clim.new <- merge(clim.p.new, clim.f.new, by="cod", all = TRUE)
  
  for (i in 1:nrow(clim.new)) {
    if(is.na(clim.new$area_km2.y[i])){clim.new$area_km2.y[i] <- 0}  
  }
  
  clim.new.2 <- clim.new[,c("cod", "area_km2.x", "area_km2.y")]
  names(clim.new.2) <- c("Clima", "area_km2_p", "area_km2_f")
  clim.new.2
  
  clim.new.2$dif_area_km2 <- clim.new.2$area_km2_f-clim.new.2$area_km2_p
  clim.new.2
  
  
  bar.i <- ggplot(data=clim.new.2, aes(x= Clima, y=dif_area_km2, fill = Clima, label=dif_area_km2)) + 
    geom_bar(stat="identity") + scale_fill_manual(values=col.p) +
    geom_text(position = position_dodge(width = 0.5), angle = 0, vjust = -.5, size = 3) +
    xlab("Tipos de Clima") + ylab("Superficie en Km2") +
    ggtitle ("(C): Diferencia entre la superficie futura y actual según el tipo de clima") +
    scale_y_continuous(breaks=seq(-10000, 5000, 2500))
  
  bar.i
  
  #fin ---
  
  
  
  
  # grafico ----
  
  setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
  
  png("3chart_superficie_clima_koppen_presente_futuro.png", width = 680, height = 650, units = "px")
  
  grid.arrange(pie.p, pie.f,                                   # bar plot spaning two columns
               bar.i,                               # box plot and scatter plot
               ncol = 2, nrow = 2, 
               layout_matrix = rbind(c(1,2), c(3,3)))
  dev.off()
  # fin ---
  
  
  
# presente 2 ----
Bp <- sum(clim.p.new[c(1,2),]$area_km2);Bp
Cp <- sum(clim.p.new[c(3,6),]$area_km2);Cp
Dp <- sum(clim.p.new[c(7,8),]$area_km2);Dp
Ep <- sum(clim.p.new[c(9,10),]$area_km2);Ep
general.p <- data.frame(cod=c("B", "C", "D", "E"), area_km2=c(Bp, Cp, Dp, Ep))
general.p$porc <- round((general.p$area_km2*100)/sum(general.p$area_km2), 2)

db <- data.frame(uso=general.p$cod, porc=general.p$porc)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

library(viridis) 

pie.p2 <- bp + coord_polar("y", start=0)+ 
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE, direction=-1, option = "D") +
  ggtitle("(A)") +#: Distribución actual de superficie\n según los principales grupos climáticos") +
   xlab("") + ylab("") 

pie.p2

# fin ---


# futuro 2 -----

Bf <- sum(clim.f.new[c(1,2),]$area_km2);Bf
Cf <- sum(clim.f.new[c(3,6),]$area_km2);Cf
Df <- sum(clim.f.new[c(7,8),]$area_km2);Df
Ef <- sum(clim.f.new[c(9),]$area_km2);Ef
general.f <- data.frame(cod=c("B", "C", "D", "E"), area_km2=c(Bf, Cf, Df, Ef))
general.f$porc <- round((general.f$area_km2*100)/sum(general.f$area_km2), 2)


db <- data.frame(uso=general.f$cod, porc=general.f$porc)
row.names(db) <- 1:nrow(db)
db$leyenda <- paste(db$uso," " , db$porc, "%", sep = "")
db

bp<- ggplot(db, aes(x="", y=porc, fill=leyenda))+
  geom_bar(width = 1, stat = "identity")

#library(viridis) 

pie.f2 <- bp + coord_polar("y", start=0)+ 
   scale_color_viridis(discrete = TRUE)+
   scale_fill_viridis(discrete = TRUE, direction=-1, option = "D") +
  ggtitle("(B)") +#: Distribución futura de superficie\nsegún los principales grupos climáticos") +
  xlab("") + ylab("") 

pie.f2

# fin ---

# grafico 2 ----

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")

#png("piechart_superficie_clima_grupo_principal_koppen_presente_futuro.png", width = 680, height = 450, units = "px")
pdf("piechart_superficie_clima_grupo_principal_koppen_presente_futuro.pdf", width=8, height=3)
grid.arrange(pie.p2, pie.f2,                                   # bar plot spaning two columns
             ncol = 1, nrow = 2, 
             layout_matrix = rbind(c(1,2)))
dev.off()

# fin ---