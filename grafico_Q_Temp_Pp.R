library("gridExtra")
library("ggplot2")

# Funciones ----

# media mensual por año ----

var.mean.por.anho <- function(data.base){
#data.base <- db
ej.f <- c()

for (i in 1:length(unique(data.base$agno))) {
  
  #i <- 3
  if(i==1){
    for (j in unique(data.base$agno)[i]) {
      
      
      ej <- data.frame(agno=j, mes=1:12, mean=c(mean(data.base$valor[data.base$agno==j & data.base$mes==1], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==2], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==3], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==4], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==5], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==6], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==7], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==8], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==9], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==10], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==11], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==12], na.rm = TRUE)))
      
      ej.f <- ej
    }
    
  }else(
    for (j in unique(data.base$agno)[i]) {
      
      ej <- data.frame(agno=j, mes=1:12, mean=c(mean(data.base$valor[data.base$agno==j & data.base$mes==1], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==2], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==3], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==4], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==5], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==6], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==7], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==8], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==9], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==10], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==11], na.rm = TRUE),
                                                mean(data.base$valor[data.base$agno==j & data.base$mes==12], na.rm = TRUE)))
      ej.f <- rbind(ej.f, ej)
    }
  )
}

#class(ej.f)
ej.f$mean[is.na(ej.f$mean)] <- NA

data.base2 <- ej.f

return(data.base2)
}

# fin ---


# suma mensual de pp por año ----

var.sum.por.anho.pp <- function(data.base){
  #data.base <- db
  ej.f <- c()
  
  for (i in 1:length(unique(data.base$agno))) {
    
    #i <- 3
    if(i==1){
      for (j in unique(data.base$agno)[i]) {
        
        
        ej <- data.frame(agno=j, mes=1:12, sum=c(sum(data.base$valor[data.base$agno==j & data.base$mes==1], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==2], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==3], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==4], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==5], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==6], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==7], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==8], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==9], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==10], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==11], na.rm = TRUE),
                                                  sum(data.base$valor[data.base$agno==j & data.base$mes==12], na.rm = TRUE)))
        
        ej.f <- ej
      }
      
    }else(
      for (j in unique(data.base$agno)[i]) {
        
        ej <- data.frame(agno=j, mes=1:12, sum=c(sum(data.base$valor[data.base$agno==j & data.base$mes==1], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==2], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==3], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==4], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==5], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==6], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==7], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==8], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==9], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==10], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==11], na.rm = TRUE),
                                                 sum(data.base$valor[data.base$agno==j & data.base$mes==12], na.rm = TRUE)))
        ej.f <- rbind(ej.f, ej)
      }
    )
  }
  
  #class(ej.f)
  ej.f$sum[is.na(ej.f$sum)] <- NA
  
  data.base2 <- ej.f
  
  return(data.base2)
}

# fin ---


# media mensual con todos los datos ----

var.mean <- function(data.base2){
 
  db.result <- data.frame(mes=1:12, mean=c(mean(data.base2$mean[data.base2$mes==1], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==2], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==3], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==4], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==5], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==6], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==7], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==8], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==9], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==10], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==11], na.rm = TRUE),
                                           mean(data.base2$mean[data.base2$mes==12], na.rm = TRUE)))
  
  db.result$mes.name <- "NA"
  return(db.result)  
}

# fin ---


# media mensual para pp con todos los datos ----

var.mean.pp <- function(data.base2){
  
  db.result <- data.frame(mes=1:12, mean=c(mean(data.base2$sum[data.base2$mes==1], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==2], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==3], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==4], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==5], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==6], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==7], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==8], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==9], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==10], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==11], na.rm = TRUE),
                                           mean(data.base2$sum[data.base2$mes==12], na.rm = TRUE)))
  
  db.result$mes.name <- "NA"
  return(db.result)  
}

# fin ---


# reordenar por el mes con menos caudal ----

new.order <- function(db.result){
  
db.result$mes.name[db.result$mes%in%1] <- "E"
db.result$mes.name[db.result$mes%in%2] <- "F"
db.result$mes.name[db.result$mes%in%3] <- "M"
db.result$mes.name[db.result$mes%in%4] <- "A"
db.result$mes.name[db.result$mes%in%5] <- "M"
db.result$mes.name[db.result$mes%in%6] <- "J"
db.result$mes.name[db.result$mes%in%7] <- "J"
db.result$mes.name[db.result$mes%in%8] <- "A"
db.result$mes.name[db.result$mes%in%9] <- "S"
db.result$mes.name[db.result$mes%in%10] <- "O"
db.result$mes.name[db.result$mes%in%11] <- "N"
db.result$mes.name[db.result$mes%in%12] <- "D"

db.result$new.order <- 0

db.result$new.order[db.result$mes%in%1] <- 5
db.result$new.order[db.result$mes%in%2] <- 6
db.result$new.order[db.result$mes%in%3] <- 7
db.result$new.order[db.result$mes%in%4] <- 8
db.result$new.order[db.result$mes%in%5] <- 9
db.result$new.order[db.result$mes%in%6] <- 10
db.result$new.order[db.result$mes%in%7] <- 11
db.result$new.order[db.result$mes%in%8] <- 12
db.result$new.order[db.result$mes%in%9] <- 1
db.result$new.order[db.result$mes%in%10] <- 2
db.result$new.order[db.result$mes%in%11] <- 3
db.result$new.order[db.result$mes%in%12] <- 4

db.result2 <- db.result[order(db.result$new.order),]

return(db.result2)
}

# fin ---


setwd("C:/Users/Francisco/Downloads/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/series_caudal")

# db <- read.csv("En_Angostura_Chacabuco_series_caudal.csv")
# db$name <- "En_Angostura_Chacabuco"

# db <- read.csv("Rio_Cochrane_series_caudal.csv")
# db$name <- "Rio_Cochrane"

# db <- read.csv("Rio_Murta_series_caudal.csv")
# db$name <- "Rio_Murta"

# db <- read.csv("Rio_ibanhez_series_caudal.csv")
# db$name <- "Rio_Ibanhez"

#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/series_temp_max/")

db.t <- read.csv("temp_mean.csv")
db.t$name <- "Centro Temuco"

# db.t <- read.csv("En_Angostura_Chacabuco_series_temp_max.csv")
# db.t$name <- "En_Angostura_Chacabuco"

# db.t <- read.csv("Lord_Cochrane_series_temp_max.csv")
# db.t$name <- "Lord_Cochrane"

# db.t <- read.csv("Bahia_Murta_series_temp_max.csv")
# db.t$name <- "Bahia_Murta"

# db.t <- read.csv("Puerto_Ibanhez_series_temp_max.csv")
# db.t$name <- "Puerto_Ibanhez"

# setwd("C:/Users/Francisco/Dropbox/Por subir/datos/series_pp/")

db.pp <- read.csv("pp.csv")
db.pp$name <- "Pueblo Nuevo"


# db.pp <- read.csv("En_Angostura_Chacabuco_series_pp.csv")
# db.pp$name <- "En_Angostura_Chacabuco"

# db.pp <- read.csv("Lord_Cochrane_series_pp.csv")
# db.pp$name <- "Lord_Cochrane"

# db.pp <- read.csv("Bahia_Murta_series_pp.csv")
# db.pp$name <- "Bahia_Murta"

# db.pp <- read.csv("Puerto_Ibanhez_series_pp.csv")
# db.pp$name <- "Puerto_Ibanhez"


start <- max(#min(db$agno),
             min(db.t$agno),
             min(db.pp$agno)) ; start

end <- min(#max(db$agno),
             max(db.t$agno),
             max(db.pp$agno)) ; end
# Caudal ----

head(db)
dim(db)
str(db)
table(db$agno)
table(db$dia)


head(db)
dim(db)
str(db)

db$name <- as.factor(db$name)

unique(db$agno)

table(db$agno)
table(db$dia)

db2 <- subset(db, agno>=start & agno<=end)
dim(db2)

table(db2$agno)
table(db2$dia)
summary(db2$valor)

mean.q.pre <- var.mean.por.anho(db2)
mean.q.pre
dim(mean.q.pre)

table(mean.q.pre$agno)
table(mean.q.pre$mes)
summary(mean.q.pre$mean)

#plot(mean.q.pre$mes, mean.q.pre$mean, ylim=c(0, max(mean.q.pre$mean, na.rm = TRUE))+10, main = "Caudal")

mean.q.pre2 <- var.mean(mean.q.pre)
mean.q.pre2
dim(mean.q.pre2)

table(mean.q.pre2$mes)
summary(mean.q.pre2$mean)

#lines(mean.q.pre2$mes, mean.q.pre2$mean, col="blue")

mean.q <- new.order(mean.q.pre2)
mean.q
summary(mean.q$mean)

#plot(mean.q$mean, ylim=c(0, max(mean.q$mean))+10, main = "Caudal promedio mensual")

# fin ---







# Temperatura ----

head(db.t)
dim(db.t)
str(db.t)
table(db.t$agno)
table(db.t$dia)

head(db.t)
dim(db.t)
str(db.t)

table(db.t$agno)
table(db.t$mes)

db.t2 <- subset(db.t, agno>=start & agno<=end)
dim(db.t2)
summary(db.t2$valor)

table(db.t2$agno)
table(db.t2$mes)

temp.max.pre <- var.mean.por.anho(db.t2)
summary(temp.max.pre$mean)

table(temp.max.pre$agno)
table(temp.max.pre$mes)

#plot(temp.max.pre$mes, temp.max.pre$mean, ylim=c(0, max(temp.max.pre$mean, na.rm = TRUE)+5), main = "Temperatura maxima mensual")

temp.max.pre2 <- var.mean(temp.max.pre)
temp.max.pre2
dim(temp.max.pre2)

table(temp.max.pre2$mes)
summary(temp.max.pre2$mean)

#lines(temp.max.pre2$mes, temp.max.pre2$mean, col="red")

t.max <- new.order(temp.max.pre2)
t.max

# para katy!!!!!!!
t.max$new.order2 <- 0

t.max$new.order2[t.max$mes==7] <- 1
t.max$new.order2[t.max$mes==8] <- 2
t.max$new.order2[t.max$mes==9] <- 3
t.max$new.order2[t.max$mes==10] <- 4
t.max$new.order2[t.max$mes==11] <- 5
t.max$new.order2[t.max$mes==12] <- 6
t.max$new.order2[t.max$mes==1] <- 7
t.max$new.order2[t.max$mes==2] <- 8
t.max$new.order2[t.max$mes==3] <- 9
t.max$new.order2[t.max$mes==4] <- 10
t.max$new.order2[t.max$mes==5] <- 11
t.max$new.order2[t.max$mes==6] <- 12

t.max <- t.max[order(t.max$new.order2),]

# fin ---

summary(t.max$mean)

plot(t.max$mean, ylim=c(0, max(t.max$mean))+5, main = "Temperatura maxima promedio mensual")

# fin ---






# Precipitacion ----

head(db.pp)
dim(db.pp)
str(db.pp)
table(db.pp$agno)
table(db.pp$dia)

head(db.pp)
dim(db.pp)
str(db.pp)

table(db.pp$agno)
table(db.pp$mes)

db.pp2 <- subset(db.pp, agno>=start & agno<=end)
dim(db.pp2)
summary(db.pp2$valor)

table(db.pp2$agno)
table(db.pp2$mes)

pp.pre <- var.sum.por.anho.pp(db.pp2)
summary(pp.pre$sum)

table(pp.pre$agno)
table(pp.pre$mes)

#plot(pp.pre$mes, pp.pre$sum, ylim=c(0, max(pp.pre$sum, na.rm = TRUE)+10), main = "Precipitacion media mensual")

pp.pre2 <- var.mean.pp(pp.pre)
pp.pre2
dim(pp.pre2)

table(pp.pre2$mes)
summary(pp.pre2$mean)

#lines(pp.pre2$mes, pp.pre2$mean, col="red")

mean.pp <- new.order(pp.pre2)
mean.pp

# para katy!!!!!!!
mean.pp$new.order2 <- 0

mean.pp$new.order2[mean.pp$mes==7] <- 1
mean.pp$new.order2[mean.pp$mes==8] <- 2
mean.pp$new.order2[mean.pp$mes==9] <- 3
mean.pp$new.order2[mean.pp$mes==10] <- 4
mean.pp$new.order2[mean.pp$mes==11] <- 5
mean.pp$new.order2[mean.pp$mes==12] <- 6
mean.pp$new.order2[mean.pp$mes==1] <- 7
mean.pp$new.order2[mean.pp$mes==2] <- 8
mean.pp$new.order2[mean.pp$mes==3] <- 9
mean.pp$new.order2[mean.pp$mes==4] <- 10
mean.pp$new.order2[mean.pp$mes==5] <- 11
mean.pp$new.order2[mean.pp$mes==6] <- 12

mean.pp <- mean.pp[order(mean.pp$new.order2),]

# fin ---

summary(mean.pp$mean)

plot(mean.pp$mean, ylim=c(0, max(mean.pp$mean))+1, main = "Precipitacion media mensual")

# fin ---



# Grafico ----
# mean.pp$new.order <- as.factor(mean.pp$new.order)
# levels(mean.pp$new.order) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

mean.pp$new.order2 <- as.factor(mean.pp$new.order2)
levels(mean.pp$new.order2) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

mean.q$new.order <- as.character(mean.q$new.order)
mean.q$new.order <- as.numeric(mean.q$new.order)

t.max$new.order2 <- as.character(t.max$new.order2)
t.max$new.order2 <- as.numeric(t.max$new.order2)

labels.i <- c("1"=mean.pp$mes.name[1], "2"=mean.pp$mes.name[2], "3"=mean.pp$mes.name[3], "4"=mean.pp$mes.name[4],
              "5"=mean.pp$mes.name[5], "6"=mean.pp$mes.name[6], "7"=mean.pp$mes.name[7], "8"=mean.pp$mes.name[8],
              "9"=mean.pp$mes.name[9], "10"=mean.pp$mes.name[10], "11"=mean.pp$mes.name[11], "12"=mean.pp$mes.name[12])

#scaleFactor <- max(mean.pp$mean) / max(t.max$mean)
#scaleFactor <- 4.472942
scaleFactor <- 6#katy
  
e1 <- ggplot()+
  geom_bar(mapping=aes(x=mean.pp$new.order2, y=mean.pp$mean), width = 1, stat = "identity", colour="#190707", fill="#084B8A") + 
  # labs(y="Precipitación (mm)                                                Caudal (m3/s)", x="Meses") +
  labs(y="Precipitación (mm)", x="Meses") +
  scale_x_discrete(labels=labels.i) + 
  # geom_line(aes(x=mean.q$new.order, y=mean.q$mean)) +
  # geom_point(aes(x=mean.q$new.order, y=mean.q$mean)) +
  geom_line(aes(x=t.max$new.order2, y=t.max$mean* scaleFactor), colour="#DF0101", size=1)+  
  geom_point(aes(x=t.max$new.order2, y=t.max$mean* scaleFactor))+
  scale_y_continuous(limits = c(0, 200), sec.axis =sec_axis(~./scaleFactor, name = "Temperatura (C°)")) +
  theme(axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")
 ) +
 # ggtitle( paste( unique(gsub("_", " ", db$name)), "con datos desde", start, "a", end, sep = " ") ) 
  labs(title="Precipitación y temperatura de la ciudad de Temuco", 
          subtitle = "Elaborado con datos del GHCN para el periodo 1953-2019 en temperatura y\nDGA con periodo 1963-2019 para la precipitación")
  
e1

plot.new()
grid.arrange(e1, e2,
             e3, e4,
             ncol = 2, nrow = 2)


setwd("C:/Users/Francisco/Documents/katy/")

png("grafico_Pp_TempMeanPvaldivia.png", width = 520, height = 520, units = "px")
e1

dev.off()


t.max.out <- t.max[c(1,2,3)]
mean.pp.out <- mean.pp[c(1,2,3)]

write.csv(t.max.out, "db_temp_media_Temuco.csv", row.names = F)
write.csv(mean.pp.out, "db_pp_media_Pueblo_Nuevo.csv", row.names = F)








# Grafico final ---
# 
# setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
# 
# #png("grafico_Q_Pp_TempMax.png", width = 720, height = 720, units = "px")
# par(mar=c(3,5,5,5))
# 
# plot(mean.q$mean,ann=FALSE, type = "o", col = "black",
#      ylim=c(0, 1000), axes = FALSE)
# axis(2, at=seq(0, 1400, by=200))
# axis(1, at=c(1:12), labels=mean.q$mes.name)
# axis(2, at=seq(0,200,by=100), col = "blue", lwd=2)
# 
# mtext("Caudal (m3/s)", side=2, line = 2.5, font = 2, at=300)
# mtext("Precipitación (mm)", side=2, line = 2.5, at= 100, font = 2)
# 
# lines(mean.pp$mean, type="h", lwd=7, col="blue")#Creamos un nuevo grÃ¡fico
# 
# par(new=TRUE)
# plot(t.max$mean, axes=FALSE,ann=FALSE, type = "o", col = "red",
#      ylim=c(0, max(t.max$mean)+5))
# axis(4, at=seq(0, 25, by=5), col = "red")
# 
# mtext("Temperatura (°C)", side=4, line = 2.5, font = 2)
# 
# legend("topleft", legend = c("Q", "T° Max", "Pp"), col=c("black", "red", "blue"),
#        lty = c(1,1,1), lwd=c(1,1,7), pch = c(1, 1, NA), merge = TRUE)
# #dev.off()
# 
# fin ---






























#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################

# mapa 1----

library(rgeos)
library(rgdal)
library(raster)
library(sf)

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

lim <- readOGR(".", "polygon_cuenca_baker_chile_grass_19s")
lim@data$id <- 1:nrow(lim@data)

a <- readOGR(".", "lines_hidrografia_baker_nombres_19s")
a@data$id <- 1:nrow(a@data)

p1 <- readOGR(".", "points_estaciones_fluviometricas_seleccionadas_19s")
p2 <- readOGR(".", "points_estaciones_metereologicas_seleccionadas_19s")

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Seminario de investigacion/coberturas/")

lagos.pre <- readOGR(".", "polygon_cuerpos_agua_19s")
lagos <- crop(lagos.pre, lim)
lagos@data$id <- 1:nrow(lagos@data)

#En el caso de polígonos y lineas es necesario aplicar la función fortify para generar un listado de coordenadas, esto es un data.frame util para plotear.
zona_estudio.f <- fortify(lim, id="id")
#Se cambia el nombre del campo long y lat por x e y, para facilitar la nomenclatura de los ejes en el gráfico a crear.
names(zona_estudio.f) <- c("x","y", "order","hole","piece","id", "group")
head(zona_estudio.f)

lagos.gg <- fortify(lagos, id="id")
names(lagos.gg) <- c("x","y", "order","hole","piece","id", "group")
head(lagos.gg)

a.gg <- fortify(a, id="id")
names(a.gg) <- c("x","y", "order","piece","id", "group")
head(a.gg)

library(ggsn)

map <- ggplot() + 
  geom_polygon(data=zona_estudio.f, aes(x=x, y=y, group = group),colour="white", fill="#A4A4A4" ) +
  geom_path(data=a.gg, aes(x=x, y=y, group=group), color = '#0489B1', size=1) +
  geom_polygon(data=lagos.gg, aes(x=x, y=y, group = group),colour="#084B8A", fill="#084B8A" ) +
  geom_point(data=p1@data, aes(x=x, y=y), color="#FE9A2E",size=3.4)+ 
  geom_point(data=p2@data, aes(x=x, y=y), color="#BFFF00",size=2.6) +
  north(x.min = 320000, x.max = 345000,
        y.min = 4680000, y.max = 4710000,
  #       location = "toprgiht", scale = 0.1) +
  # scalebar(x.min = 200000, x.max = 300000,
  #          y.min = 4680000, y.max = 4700000,dist = 40000, dist_unit = "km",st.size=4, height=0.5, transform = TRUE, model = 'WGS84')
  
map 



library(gridExtra)

grid.arrange(map, e,                                   # bar plot spaning two columns
             ncol = 2, nrow = 1, 
             layout_matrix = rbind(c(1,2)))
#dev.off()









# mapa 2----

library(rgeos)
library(rgdal)
library(raster)

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")

lim <- readOGR(".", "polygon_cuenca_baker_chile_grass_19s")
a <- readOGR(".", "lines_hidrografia_baker_nombres_19s")
p <- readOGR(".", "points_estaciones_monitoreo_19s")

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Seminario de investigacion/coberturas/")

lagos.pre <- readOGR(".", "polygon_cuerpos_agua_19s")
lagos <- crop(lagos.pre, lim)



# Grafico ----
#setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
png("mapa_estaciones_moni_grafico_Q_Pp_T.png", width = 680, height = 450, units = "px")

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
par(oma=c(0,0,0,2), mar=c(2,2,2,2))

plot(lim, border="red", axes=TRUE)
plot(a, add=T, col="blue")
plot(lagos, col="blue", add=T)
plot(p, add=T, pch=16, col="red", cex=1.3)
text(p, p@data$id, pos=3, font=2, cex=1.2, halo=TRUE, hc='black', col='white', hw=0.2)
mtext("A)", font = 2, line = -1.2, adj = 0.98)

legend("topleft", legend = c("Limite Cuenca", "Lagos-Lagunas"), 
       fill = c(NA,4), border = c("red", NA), bty = "n")

legend("bottomright", legend = c("Estaciones Monitoreo", "Red Hídrica"), 
       col = c("red","blue"), bty = "n", lty=c(NA,1), pch = c(16, NA))

# ---

plot(mean.q$mean,ann=FALSE, type = "o", col = "black",
     ylim=c(0, max(mean.q$mean)+100), axes = FALSE)
axis(2, at=seq(0, 1400, by=200))
axis(1, at=c(1:12), labels=mean.q$mes.name)
axis(2, at=seq(0,200,by=100), col = "blue", lwd=1)

mtext("Caudal (m3/s)", side=2, line = 2.5, font = 2, at=800)
mtext("Precipitación (mm)", side=2, line = 2.5, at= 100, font = 2)

lines(mean.pp$mean, type="h", lwd=7, col="blue")#Creamos un nuevo grÃ¡fico

par(new=TRUE)
plot(t.max$mean, axes=FALSE,ann=FALSE, type = "o", col = "red",
     ylim=c(0, max(t.max$mean)+5))
axis(4, at=seq(0, 25, by=5), col = "red")

mtext("Temperatura (°C)", side=4, line = 2.5, font = 2)

legend("topleft", legend = c("Q", "T° Max", "Pp"), col=c("black", "red", "blue"),
       lty = c(1,1,1), lwd=c(1,1,7), pch = c(1, 1, NA), merge = TRUE)
mtext("B)", font = 2, line = -1.2, adj = 0.98)

#dev.off()
# fin ---
