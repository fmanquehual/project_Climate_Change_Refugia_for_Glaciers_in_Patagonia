library("gridExtra")
library("ggplot2")

# Funciones ----

# agrega anho y mes por dato ----

new.var <- function(data.base){
  
  data.base$date <- as.character(data.base$date)
  data.base$agno <- 0
  data.base$mes <- 0
  
  for (i in 1:nrow(data.base)) {
    data.base$agno[i] <- as.numeric(unlist(strsplit(data.base$date[i], split="-"))[1])
  }
  
  for (j in 1:nrow(data.base)) {
    data.base$mes[j] <- as.numeric(unlist(strsplit(data.base$date[j], split="-"))[2])
  }
  return(data.base)
}

# fin ---


# media mensual por aÃ±o ----

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


# suma mensual de pp por aÃ±o ----

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





# lectura datos ----

#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_Bajo_Nhadis/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_desague_lago_Bertrand/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Cochrane/")
setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Nef/")

db0 <- read.csv("q_m3s_mon.csv")
#db0$name <- "Rio_Baker_Bajo_Nhadis"
#db0$name <- "Rio_Baker_desague_Lago_Bertrand"
#db0$name <- "Rio_Cochrane"
db0$name <- "Rio_Nef"

db01 <- new.var(db0)
db <- na.omit(db01)
head(db)

#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_Bajo_Nhadis/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_desague_lago_Bertrand/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Cochrane/")
setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Nef/")

db.t0 <- read.csv("tmax_cr2met_mon.csv")
#db.t0$name <- "Rio_Baker_Bajo_Nhadis"
#db.t0$name <- "Rio_Baker_desague_Lago_Bertrand"
#db0$name <- "Rio_Cochrane"
db0$name <- "Rio_Nef"

db.t01 <- new.var(db.t0)
db.t <- na.omit(db.t01)
head(db.t)

#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_Bajo_Nhadis/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_desague_lago_Bertrand/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Cochrane/")
setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Nef/")

db.pp0 <- read.csv("precip_cr2met_mon.csv")
#db.pp0$name <- "Rio_Baker_Bajo_Nhadis"
#db.pp0$name <- "Rio_Baker_desague_Lago_Bertrand"
#db0$name <- "Rio_Cochrane"
db0$name <- "Rio_Nef"

db.pp01 <- new.var(db.pp0)
db.pp <- na.omit(db.pp01)
head(db.pp)




start <- max(min(db$agno),
             min(db.t$agno),
             min(db.pp$agno)) ; start

end <- min(max(db$agno),
           max(db.t$agno),
           max(db.pp$agno)) ; end





# Caudal ----

head(db)
dim(db)
str(db)
table(db$agno)

db$name <- as.factor(db$name)


db2 <- subset(db, agno>=start & agno<=end)
dim(db2)

table(db2$agno)
summary(db2$valor)

mean.q.pre <- var.mean.por.anho(db2)
head(mean.q.pre)
head(db)
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
summary(mean.q.pre2$mean)

#lines(temp.max.pre2$mes, temp.max.pre2$mean, col="red")

t.max <- new.order(temp.max.pre2)
t.max
summary(t.max$mean)

#plot(t.max$mean, ylim=c(0, max(t.max$mean))+5, main = "Temperatura maxima promedio mensual")

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
summary(mean.pp$mean)

#plot(mean.pp$mean, ylim=c(0, max(mean.pp$mean))+1, main = "Precipitacion media mensual")


mean.pp$begin.data <- start
mean.pp$end.data <- end

t.max$begin.data <- start
t.max$end.data <- end

mean.q$begin.data <- start
mean.q$end.data <- end
# fin ---



# save to results ----

# setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_Bajo_Nhadis/")
# write.csv(mean.pp, "pp_mean.csv", row.names = FALSE)
# write.csv(t.max, "t.max_mean.csv", row.names = FALSE)
# write.csv(mean.q, "q_mean.csv", row.names = FALSE)

# setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_desague_lago_Bertrand/")
# write.csv(mean.pp, "pp_mean.csv", row.names = FALSE)
# write.csv(t.max, "t.max_mean.csv", row.names = FALSE)
# write.csv(mean.q, "q_mean.csv", row.names = FALSE)

# setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Cochrane/")
# write.csv(mean.pp, "pp_mean.csv", row.names = FALSE)
# write.csv(t.max, "t.max_mean.csv", row.names = FALSE)
# write.csv(mean.q, "q_mean.csv", row.names = FALSE)

# setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Nef/")
# write.csv(mean.pp, "pp_mean.csv", row.names = FALSE)
# write.csv(t.max, "t.max_mean.csv", row.names = FALSE)
# write.csv(mean.q, "q_mean.csv", row.names = FALSE)

# fin ---













# Grafico ----



# lectura datos ----
rm(list=ls())
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_Bajo_Nhadis/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Baker_desague_lago_Bertrand/")
#setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Cochrane/")
setwd("C:/Users/Francisco/Dropbox/Por subir/datos/datos_de_CAMELS/Rio_Nef/")

mean.q <- read.csv("q_mean.csv")
mean.q

mean.pp <- read.csv("pp_mean.csv")
mean.pp

t.max <- read.csv("t.max_mean.csv")
t.max

mean.pp$new.order <- as.factor(mean.pp$new.order)
levels(mean.pp$new.order) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

mean.q$new.order <- as.character(mean.q$new.order)
mean.q$new.order <- as.numeric(mean.q$new.order)

t.max$new.order <- as.character(t.max$new.order)
t.max$new.order <- as.numeric(t.max$new.order)

mean.pp$mes.name <- as.character(mean.pp$mes.name)

labels.i <- c("1"=mean.pp$mes.name[1], "2"=mean.pp$mes.name[2], "3"=mean.pp$mes.name[3], "4"=mean.pp$mes.name[4],
              "5"=mean.pp$mes.name[5], "6"=mean.pp$mes.name[6], "7"=mean.pp$mes.name[7], "8"=mean.pp$mes.name[8],
              "9"=mean.pp$mes.name[9], "10"=mean.pp$mes.name[10], "11"=mean.pp$mes.name[11], "12"=mean.pp$mes.name[12])

name0 <- unique(gsub("_", " ", getwd()))
name1 <- unlist(strsplit(name0, split="/"))
name2 <- gsub("Nh", "Ñ", name1[length(name1)]) ; name2
name3 <- gsub("l", "L", name2)
name4 <- as.character(name3)

start <- unique(mean.q$begin.data) ; start
end <- unique(mean.q$end.data) ; end

if(name4=="Rio Baker desague Lago Bertrand" | name4=="Rio Baker Bajo Ñadis"){ymax.i <- 1500}else(ymax.i <- 400)
if(name4=="Rio Baker desague Lago Bertrand" | name4=="Rio Baker Bajo Ñadis"){scaleFactor <- 15}else(scaleFactor <- 20)


e1 <- ggplot()+
  geom_bar(mapping=aes(x=mean.pp$new.order, y=mean.pp$mean), width = 1, stat = "identity", fill=rgb(0.1,0.3,0.5,0.7), colour="#000000")+#, colour="#190707", fill="#084B8A") + # activalo y tendras los graph originales 
  labs(y="Precipitación (mm)                                                Caudal (m3/s)", x="Meses") +
  scale_x_discrete(labels=labels.i) + 
  geom_line(aes(x=mean.q$new.order, y=mean.q$mean))+#, colour="#FFFF00", size=1) +
  geom_point(aes(x=mean.q$new.order, y=mean.q$mean))+#, colour="#999999") +
  geom_line(aes(x=t.max$new.order, y=t.max$mean* scaleFactor), colour="#DF0101", size=1)+  
  geom_point(aes(x=t.max$new.order, y=t.max$mean* scaleFactor))+
  scale_y_continuous(limits = c(0, ymax.i), sec.axis =sec_axis(~./scaleFactor, name = "Temperatura (°C)")) +
  theme(axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")
  )# +
  #ggtitle( paste( name4, "con datos desde", start, "a", end, sep = " ") ) # activalo para el graph original

e1


# Exportar grafico ----
name.export0 <- unlist(strsplit(getwd(), split="/"))[length(unlist(strsplit(getwd(), split="/")))]
#name.export <- paste("grafico_", name.export0, "_con_datos_desde_", start, "_a_", end, ".png", sep = "") ; name.export # original
name.export <- paste("grafico_", name.export0, "_con_datos_desde_", start, "_a_", end, ".pdf", sep = "") ; name.export

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")

#png(name.export, width = 450, height = 450, units = "px")
pdf(name.export, width = 5, height = 5)
e1

dev.off()
# fin ---



# grid.arrange(e1, e2,
#              e3, e4,
#              ncol = 2, nrow = 2)
# 
# 
