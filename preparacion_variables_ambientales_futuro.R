library(rgdal)
library(rgeos)
library(raster)

# coberturas listas:
# - ccsm4 2070 rcp85 SI 
# - ccsm4 2070 rcp45 NO 
# - ccsm4 2050 rcp85 SI
# - ccsm4 2050 rcp45 NO

# - ipsl 2070 rcp85  SI
# - ipsl 2070 rcp45  NO
# - ipsl 2050 rcp85  SI
# - ipsl 2050 rcp45  NO

# - miroc 2070 rcp85 SI
# - miroc 2070 rcp45 NO
# - miroc 2050 rcp85 SI
# - miroc 2050 rcp45 NO

# - csiro 2070 rcp45  NO
# - csiro 2070 rcp85  NO
# - csiro 2050 rcp45  NO
# - csiro 2050 rcp85  NO

rm(list = ls())
dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " #coordenadas geograficas WGS84

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
#marco <- readOGR(".", "polygon_marco_trabajo_geo") # PARA EL BAKER
marco <- readOGR(".", "polygon_marco_trabajo_nuevo_geo")
#marco.geo <- spTransform(marco, CRSobj = wgs84) # PARA EL BAKER

# var.i: puede ser "tmax", "tmin" o "pp"
# anho.i: puede ser "2050" o "2070"
# rcp.i: puede ser "60" o "85"
directory.wc <- function(modelo.i, var.i, anho.i, rcp.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"
  #worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_WorldClim/"
  worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/CSIRO_MK3_60_30_arcseconds"
  # modelo.i2 <- paste(modelo.i, '30_arcseconds', sep = "") # ACTIVALO si es Worldclim
   result <- paste(worldclim, anho.i, var.i, rcp.i, sep = "/")
  return(result)
}

# fin ---

## WORLDCLIME 2.0 ------------------------------------------------------------------------------------------------
#modelo.j <- "CCSM4_"
#modelo.j <- "IPSL_"
#modelo.j <- "MIROC_"
modelo.j <- "CSIRO_"

var.j <- "pp"
anho.j <- "2070"
rcp.j <- "85" # DESCARGAR EL 45 PARA NUEVA AREA DE ESTUDIO

name.summer.out <- paste(modelo.j, var.j, "_mean_", "summer_", anho.j, "_rcp_", rcp.j, "_geo",sep = "") ; name.summer.out
name.winter.out <- paste(modelo.j, var.j, "_mean_", "winter_", anho.j, "_rcp_", rcp.j, "_geo",sep = "") ; name.winter.out

# Lectura de datos pp ----

setwd(directory.wc(modelo.i = modelo.j, var.i = var.j, anho.i = anho.j, rcp.i = rcp.j)) 

r.stack.pp <- stack(dir()[1], dir()[5], dir()[6], dir()[7], dir()[8], dir()[9], 
                    dir()[10], dir()[11], dir()[12], dir()[2], dir()[3], dir()[4])

names(r.stack.pp)
# fin ---




# Preparacion de coberturas ----

r.stack.pp.clip <- crop(r.stack.pp, marco)
r.stack.pp.clip <- stack(r.stack.pp.clip)


# fin ---



# calculo ----
seq.summer <- c(1:3, 10:12) ; seq.summer 
seq.winter <- setdiff(1:12, seq.summer) ; seq.winter

r.stack.pp.summer <- r.stack.pp.clip[[seq.summer]]
r.stack.pp.winter <- r.stack.pp.clip[[seq.winter]]

sort(minValue(r.stack.pp.summer))
e <- -32767
r.stack.pp.summer[r.stack.pp.summer[]%in%e] <- NA # ES PARA CHELSA
sort(minValue(r.stack.pp.summer))

sort(minValue(r.stack.pp.winter))
r.stack.pp.winter[r.stack.pp.winter[]%in%e] <- NA # ES PARA CHELSA
sort(minValue(r.stack.pp.winter))


r.summer <- mean(r.stack.pp.summer)
minValue(r.summer)
plot(r.summer)

r.winter <- mean(r.stack.pp.winter)
minValue(r.winter)
plot(r.winter)



# grafico
# breakpoints <- seq(0, 240, by=30) # ACTIVAR PARA WORLDCLIM
breakpoints <- seq(0, 450, by=50)  # ACTIVAR PARA CHELSA

winter.min.text <- paste("Min:", round(minValue(r.winter),2), sep=" ")
winter.max.text <- paste("Max:", round(maxValue(r.winter),2), sep=" ")

summer.min.text <- paste("Min:", round(minValue(r.summer),2), sep=" ")
summer.max.text <- paste("Max:", round(maxValue(r.summer),2), sep=" ")

plot(r.winter,breaks=breakpoints,col=hcl.colors(9, palette = "viridis", rev = TRUE), main="Winter")
legend("bottomright", legend = c(winter.min.text, winter.max.text), bty="n")

plot(r.summer,breaks=breakpoints,col=hcl.colors(9, palette = "viridis", rev = TRUE), main="Summer")
legend("bottomright", legend = c(summer.min.text, summer.max.text), bty="n")
# fin ---


# output ----

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/VAR_NUEVA_AREA_ESTUDIO/RCP85/")
writeRaster(r.winter, filename=name.winter.out, format="GTiff", overwrite=TRUE)
#writeRaster(r.summer, filename=name.summer.out, format="GTiff", overwrite=TRUE)

# fin ---















rm(list = ls())
dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " #coordenadas geograficas WGS84

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
#marco <- readOGR(".", "polygon_marco_trabajo_geo") # PARA EL BAKER
marco <- readOGR(".", "polygon_marco_trabajo_nuevo_geo")
#marco.geo <- spTransform(marco, CRSobj = wgs84) # PARA EL BAKER

# var.i: puede ser "tmax", "tmin" o "pp"
# anho.i: puede ser "2050" o "2070"
# rcp.i: puede ser "60" o "85"
directory.wc <- function(modelo.i, var.i, anho.i, rcp.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"
  #worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_WorldClim/"
  worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/CSIRO_MK3_60_30_arcseconds"
  #modelo.i2 <- paste(modelo.i, '30_arcseconds', sep = "") # ACTIVALO si es Worldclim
  result <- paste(worldclim, anho.i, var.i, rcp.i, sep = "/")
  return(result)
}

#modelo.j <- "CCSM4_"
#modelo.j <- "IPSL_"
#modelo.j <- "MIROC_"
modelo.j <- "CSIRO_"

var.j <- "tmin"
anho.j <- "2070"
rcp.j <- "85"

name.summer.out <- paste(modelo.j, var.j, "_mean_", "summer_", anho.j, "_rcp_", rcp.j, "_geo",sep = "") ; name.summer.out
name.winter.out <- paste(modelo.j, var.j, "_mean_", "winter_", anho.j, "_rcp_", rcp.j, "_geo",sep = "") ; name.winter.out

# Lectura de datos tmin ----

setwd(directory.wc(modelo.i = modelo.j, var.i = var.j, anho.i = anho.j, rcp.i = rcp.j)) 

r.stack.tmin <- stack(dir()[1], dir()[5], dir()[6], dir()[7], dir()[8], dir()[9], 
                      dir()[10], dir()[11], dir()[12], dir()[2], dir()[3], dir()[4])
names(r.stack.tmin)
# fin ---


# Preparacion de coberturas ----

r.stack.tmin.clip <- crop(r.stack.tmin, marco)
r.stack.tmin.clip <- stack(r.stack.tmin.clip)

# fin ---



# calculo ----
seq.summer <- c(1:3, 10:12) ; seq.summer 
seq.winter <- setdiff(1:12, seq.summer) ; seq.winter

r.stack.tmin.summer <- r.stack.tmin.clip[[seq.summer]]
r.stack.tmin.winter <- r.stack.tmin.clip[[seq.winter]]

sort(minValue(r.stack.tmin.summer))
sort(minValue(r.stack.tmin.winter))

r.summer.pre <- mean(r.stack.tmin.summer)
r.summer <- overlay(r.summer.pre, # ACTIVAR SI ES WORLDCLIM y chelsa
                    fun = function(r1) { return( r1/10 ) })
minValue(r.summer)
plot(r.summer)

r.winter.pre <- mean(r.stack.tmin.winter)
r.winter <- overlay(r.winter.pre,
                    fun = function(r1) { return( r1/10 ) })
minValue(r.winter)
plot(r.winter)

breakpoints <- seq(-20, 15, by=5) 
# colors <- c("red","white","white","white","white","blue") 

winter.min.text <- paste("Min:", round(minValue(r.winter),2), sep=" ")
winter.max.text <- paste("Max:", round(maxValue(r.winter),2), sep=" ")

summer.min.text <- paste("Min:", round(minValue(r.summer),2), sep=" ")
summer.max.text <- paste("Max:", round(maxValue(r.summer),2), sep=" ")

plot(r.winter,breaks=breakpoints,col=rev(heat.colors(7)), main="Winter")
legend("bottomright", legend = c(winter.min.text, winter.max.text), bty="n")

plot(r.summer,breaks=breakpoints,col=rev(heat.colors(7)), main="Summer")
legend("bottomright", legend = c(summer.min.text, summer.max.text), bty="n")
# fin ---


# output ----

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/VAR_NUEVA_AREA_ESTUDIO/RCP85/")
writeRaster(r.winter, filename=name.winter.out, format="GTiff", overwrite=TRUE)
#writeRaster(r.summer, filename=name.summer.out, format="GTiff", overwrite=TRUE)

# fin ---


















rm(list = ls())
dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " #coordenadas geograficas WGS84

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
#marco <- readOGR(".", "polygon_marco_trabajo_geo") # PARA EL BAKER
marco <- readOGR(".", "polygon_marco_trabajo_nuevo_geo")
#marco.geo <- spTransform(marco, CRSobj = wgs84) # PARA EL BAKER

# var.i: puede ser "tmax", "tmin" o "pp"
# anho.i: puede ser "2050" o "2070"
# rcp.i: puede ser "60" o "85"
directory.wc <- function(modelo.i, var.i, anho.i, rcp.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"
  #worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_WorldClim/"
  worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/CSIRO_MK3_60_30_arcseconds"
  #modelo.i2 <- paste(modelo.i, '30_arcseconds', sep = "") # ACTIVAR ai es Worldclim
  result <- paste(worldclim, anho.i, var.i, rcp.i, sep = "/")
  return(result)
}

#modelo.j <- "CCSM4_"
#modelo.j <- "IPSL_"
#modelo.j <- "MIROC_"
modelo.j <- "CSIRO_"

var.j <- "tmax"
anho.j <- "2070"
rcp.j <- "85"

name.summer.out <- paste(modelo.j, var.j, "_mean_", "summer_", anho.j, "_rcp_", rcp.j, "_geo",sep = "") ; name.summer.out
name.winter.out <- paste(modelo.j, var.j, "_mean_", "winter_", anho.j, "_rcp_", rcp.j, "_geo",sep = "") ; name.winter.out

# Lectura de datos tmax ----

setwd(directory.wc(modelo.i = modelo.j, var.i = var.j, anho.i = anho.j, rcp.i = rcp.j)) 

r.stack.tmax <- stack(dir()[1], dir()[5], dir()[6], dir()[7], dir()[8], dir()[9], 
                      dir()[10], dir()[11], dir()[12], dir()[2], dir()[3], dir()[4])
names(r.stack.tmax)
# fin ---


# Preparacion de coberturas ----

r.stack.tmax.clip <- crop(r.stack.tmax, marco)
r.stack.tmax.clip <- stack(r.stack.tmax.clip)

# fin ---



# calculo ----
seq.summer <- c(1:3, 10:12) ; seq.summer 
seq.winter <- setdiff(1:12, seq.summer) ; seq.winter

r.stack.tmax.summer <- r.stack.tmax.clip[[seq.summer]]
r.stack.tmax.winter <- r.stack.tmax.clip[[seq.winter]]

sort(minValue(r.stack.tmax.summer))
sort(minValue(r.stack.tmax.winter))

r.summer.pre <- mean(r.stack.tmax.summer)
r.summer <- overlay(r.summer.pre,
                    fun = function(r1) { return( r1/10 ) })
minValue(r.summer)
plot(r.summer)

r.winter.pre <- mean(r.stack.tmax.winter)
r.winter <- overlay(r.winter.pre,
                    fun = function(r1) { return( r1/10 ) })
minValue(r.winter)
plot(r.winter)

breakpoints <- seq(-15, 25, by=5) 
# colors <- c("red","white","white","white","white","blue") 

winter.min.text <- paste("Min:", round(minValue(r.winter),2), sep=" ")
winter.max.text <- paste("Max:", round(maxValue(r.winter),2), sep=" ")

summer.min.text <- paste("Min:", round(minValue(r.summer),2), sep=" ")
summer.max.text <- paste("Max:", round(maxValue(r.summer),2), sep=" ")

plot(r.winter,breaks=breakpoints,col=rev(heat.colors(8)), main="Winter")
legend("bottomright", legend = c(winter.min.text, winter.max.text), bty="n")

plot(r.summer,breaks=breakpoints,col=rev(heat.colors(8)), main="Summer")
legend("bottomright", legend = c(summer.min.text, summer.max.text), bty="n")
# fin ---


# output ----

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/VAR_NUEVA_AREA_ESTUDIO/RCP85/")
#writeRaster(r.winter, filename=name.winter.out, format="GTiff", overwrite=TRUE)
writeRaster(r.summer, filename=name.summer.out, format="GTiff", overwrite=TRUE)

# fin ---


