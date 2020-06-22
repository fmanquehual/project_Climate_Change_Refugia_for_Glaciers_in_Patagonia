library(raster)

setwd("C:/Users/Pancho/Documents/Practica_profesional/coberturas/")
r <- raster("dem_baker_grande_clip_19s.tif")
#plot(r)

coor.cell <- coordinates(r)
value.cell <- values(r)

db <- as.data.frame(coor.cell)
db$value <- value.cell
db$id <- 1:nrow(db)

# 8.2 Raster con ID ----

r.id <- r
r.id[] <- 1:nrow(db)
r.id

# funcion ----

consulta.db <- function(i){
  id.i <- db$id[db$id==i]
  col.i <- colFromCell(r.id, i)
  row.i <- rowFromCell(r.id, i)
  v <- db$value[db$id==i]
  x <- db$x[db$id==i]
  y <- db$y[db$id==i]
  answer <- data.frame(id=id.i, row=row.i, col=col.i, value=v, coor.x=x, coor.y=y)
  return( answer )
}

consulta.db(20000)

# fin ---

db$id[db$value%in%min(db$value, na.rm = T)] # 44355108
db$id[db$value%in%max(db$value, na.rm = T)] # 38656832

consulta.db(38656832)

v <- 3600:3627
db$id[db$value%in%v]
db$value[db$value%in%v]

consulta.db(38644274)
consulta.db(38650554)
consulta.db(38656833)
consulta.db(38663111)
# ---


# grafico rangos altitud ----
head(db)
db <- db[,c("value", "id")]
dim(db)

db2 <- subset(db, value!="NA")
head(db2)
dim(db2)

db2$rango <- -99
db2 <- db2[,c("value", "rango")]
head(db2)

summary(db2$value)

r1 <- min(db2$value):0
db2$rango[db2$value%in%r1] <- 0

r2 <- 1:500
db2$rango[db2$value%in%r2] <- 500

r3 <- 501:1000
db2$rango[db2$value%in%r3] <- 1000

r4 <- 1001:1500
db2$rango[db2$value%in%r4] <- 1500

r5 <- 1501:2000
db2$rango[db2$value%in%r5] <- 2000

r6 <- 2001:2500
db2$rango[db2$value%in%r6] <- 2500

r7 <- 2501:3000
db2$rango[db2$value%in%r7] <- 3000

r8 <- 3001:3500
db2$rango[db2$value%in%r8] <- 3500

r9 <- 3501:4000
db2$rango[db2$value%in%r9] <- 4000

head(db2)

db3 <- db2[c(0:2000),]
hist(db3$rango)

hist(db2$rango)
hist(r)
