library(rgdal)
library(rgeos)
library(raster)
library(lattice)
library(trend)


wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# funciones ----

# var.i: puede ser "tmax", "tmin" o "pp"
# anho.i: puede ser desde 1993 a 2010
directory <- function(var.i, anho.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"
  chelsea <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea"
  result <- paste(chelsea, anho.i, paste(var.i, anho.i, sep="_"), sep = "/")
  return(result)
}

# var.i: puede ser "tmax", "tmin" o "pp"
directory.wc <- function(var.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"
  worldclim <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_WorldClim/WorldClim20_30_arcseconds_1970_2000"
  result <- paste(worldclim, paste(var.i, sep="_"), sep = "/")
  return(result)
}
  
serie.i <- function(r.stack.i){
 serie <-  0:((length(names(r.stack.i))/12)-1)
 
seq <- c((1:3)+12*serie[1], # anho 1980
                (10:12)+12*serie[1], # anho 1980
                (1:3)+12*serie[2], # anho
                (10:12)+12*serie[2], # anho
                (1:3)+12*serie[3], # anho
                (10:12)+12*serie[3], # anho
                (1:3)+12*serie[4], # anho
                (10:12)+12*serie[4], # anho
                (1:3)+12*serie[5], # anho
                (10:12)+12*serie[5], # anho
                (1:3)+12*serie[6], # anho
                (10:12)+12*serie[6], # anho
                (1:3)+12*serie[7], # anho
                (10:12)+12*serie[7], # anho
                (1:3)+12*serie[8], # anho
                (10:12)+12*serie[8], # anho
                (1:3)+12*serie[9], # anho
                (10:12)+12*serie[9], # anho
                (1:3)+12*serie[10], # anho
                (10:12)+12*serie[10], # anho
                (1:3)+12*serie[11], # anho 
                (10:12)+12*serie[11], # anho
(1:3)+12*serie[12], # anho 
(10:12)+12*serie[12], # anho
(1:3)+12*serie[13], # anho 
(10:12)+12*serie[13], # anho
(1:3)+12*serie[14], # anho 
(10:12)+12*serie[14], # anho
(1:3)+12*serie[15], # anho 
(10:12)+12*serie[15], # anho
(1:3)+12*serie[16], # anho 
(10:12)+12*serie[16], # anho
(1:3)+12*serie[17], # anho 
(10:12)+12*serie[17], # anho
(1:3)+12*serie[18], # anho 
(10:12)+12*serie[18], # anho
(1:3)+12*serie[19], # anho 
(10:12)+12*serie[19], # anho
(1:3)+12*serie[20], # anho 
(10:12)+12*serie[20], # anho
(1:3)+12*serie[21], # anho 
(10:12)+12*serie[21], # anho
(1:3)+12*serie[22], # anho 
(10:12)+12*serie[22], # anho
(1:3)+12*serie[23], # anho 
(10:12)+12*serie[23], # anho
(1:3)+12*serie[24], # anho 
(10:12)+12*serie[24], # anho
(1:3)+12*serie[25], # anho 
(10:12)+12*serie[25]) # anho 2005 ORIGINAL DE AQUI HACIA ABAJO
# (1:3)+12*serie[26], # anho 
# (10:12)+12*serie[26], # anho
# (1:3)+12*serie[27], # anho 
# (10:12)+12*serie[27], # anho
# (1:3)+12*serie[28], # anho 
# (10:12)+12*serie[28], # anho
# (1:3)+12*serie[29], # anho 
# (10:12)+12*serie[29], # anho
# (1:3)+12*serie[30], # anho 
# (10:12)+12*serie[30], # anho
# (1:3)+12*serie[31], # anho 2010
# (10:12)+12*serie[31]) # anho 2010


return(seq) }
# fin ---


# marco trabajo ----

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
# marco <- readOGR(".", "polygon_marco_trabajo_geo") # original
# marco.geo <- spTransform(marco, CRSobj = wgs84) # original

marco <- readOGR(".", "polygon_marco_trabajo_nuevo_geo") # nueva area de estudio
plot(marco, axes=TRUE)
# fin ---


# # DEM cuenca ----
# 
# dem <- raster("dem_baker_grande_clip_19s.tif")
# plot(dem)
# 
# # fin ---





## CHELSEA ------------------------------------------------------------------------------------------------

# Lectura de datos pp ----
r.stack.pp <- stack()

for (i in 1980:2005) { #1980:2010 ORIGINAL
  
#  i <- 2001
  setwd(directory("pp", i)) 
  if(i==1980){
  r.stack.pp.pre <- stack(dir()[1], dir()[2], dir()[3], dir()[4], dir()[5], dir()[6], 
                      dir()[7], dir()[8], dir()[9], dir()[10], dir()[11], dir()[12])

  }else(  r.stack.pp.pre <- stack(dir()[1], dir()[2], dir()[3], dir()[4], dir()[5], dir()[6], 
                                  dir()[7], dir()[8], dir()[9], dir()[10], dir()[11], dir()[12]))
  r.stack.pp <- stack(r.stack.pp, r.stack.pp.pre)
}
names(r.stack.pp)
minValue(r.stack.pp)

# fin ---




# Preparacion de coberturas ----

r.stack.pp.clip <- crop(r.stack.pp, marco) #marco.geo) # original
r.stack.pp.clip <- stack(r.stack.pp.clip)


# r.stack.pp.clip.19s <- projectRaster(r.stack.pp.clip2, crs=crs(dem))
# setMinMax(r.stack.pp.clip.19s)
# fin ---



# calculo ----

seq.summer <- serie.i(r.stack.pp.clip)
length(seq.summer)

seq.winter <- setdiff(c(1:length(names(r.stack.pp.clip))), seq.summer)
length(seq.winter)
  
r.stack.pp.summer <- r.stack.pp.clip[[seq.summer]]
r.stack.pp.winter <- r.stack.pp.clip[[seq.winter]]

sort(minValue(r.stack.pp.summer))
sort(minValue(r.stack.pp.winter))

# setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/resultados/")
# writeRaster(r.stack.pp.summer, filename='pp_summer_set_raster_1980_2010_nueva_area_estudio_geo', format="GTiff", overwrite=TRUE)
# writeRaster(r.stack.pp.winter, filename='pp_winter_set_raster_1980_2010_nueva_area_estudio_geo', format="GTiff", overwrite=TRUE)


# r.summer <- overlay(r.stack.pp.summer,
#               fun = function(r1) { return( mean(r1) ) })


# # Pendiente ----
# r.stack.pp.winter
# sort(minValue(r.stack.pp.winter))
# 
# names(r.stack.pp.winter)
# r.winter.1980 <- mean(r.stack.pp.winter[[1:6]])
# 
# plot(r.winter.1980)
# 
# j <-  length(names(r.stack.pp.winter))/6 ; j
# 
# for (i in 1:j){ 
# #  i <- 31
#   ej0 <- mean( r.stack.pp.winter[[ (1+(6*(i-1))):(6+(6*(i-1))) ]] )
#   out <- paste('raster_', i, '<-ej0', sep = '')  
#   eval(parse(text=out))
#   
#   print( paste(i, ' objeto(s) generado(s) de ', length(serie), sep = '') )
# }
# 
# 
# 
# # Calculo de pendiente ----
# 
# r.ej <- r.stack.pp.winter[[1]]
# r.ej[] <- NA
# valores_pixel <- c()
# 
# #i.i <- 2167 # colorcar esto en el loop 'i' para generar un grafico correcto
# i.i <- 1:ncell(r.stack.pp.winter)
# 
# for (i in i.i) { # De la serie de tiempo 'j', se extraen los valores para un pixel 'i'
#   # <- 1
#   
#   for (j in serie) {
#     #   j <- 2
#     
#     out2 <- paste('raster_', j, sep = '')  
#     valor <- eval(parse(text=out2))[i]
#     
#     if(j==1){valores_pixel <- valor}else(valores_pixel <- c(valores_pixel, valor))
#   }
#   
#   ej.ts <- ts(valores_pixel, start = 2011, freq = 1)
#   
#   slope.i <- sens.slope(ej.ts,) ; slope.i
#   
#   r.ej[i] <- slope.i$estimates
#   
#   print( paste('Pixel ', i, ' listo de ', length(i.i), sep = '' ) )
# }
# 
# plot(r.ej)
# plot(marco, add=T)
# 
# r.out <- mask(r.ej, marco)
# plot(r.out)
# 
# setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_predict/")
# 
# #writeRaster(r.out, filename=name.out, format="GTiff", overwrite=TRUE)
# 
# # Grafico de tendencia ----
# 
# valores_pixel
# plot(ej.ts)
# 
# ej.ts <- ts(valores_pixel, start = 2011, freq = 1)
# xyplot(ej.ts~c(2011:2060),  
#        col='darkgreen',
#        ylab='Temperatura máxima (K)',
#        xlab='Serie de Tiempo', 
#        main= paste('Valores para el pixel numero ', i.i, sep=''),
#        sub= paste('Sens Slope = ', round(slope.i$estimates, 3), sep = ''),
#        as.table=T,
#        type = c("g", "p", "smooth"),
#        col.line ="black")# la linea me indica una tendencia.
# 
# 
# # fin ---




# situacion promedio al 2010 ----
r.summer <- mean(r.stack.pp.summer)
minValue(r.summer)
plot(r.summer)

r.winter <- mean(r.stack.pp.winter)
minValue(r.winter)
plot(r.winter)

breakpoints <- seq(0, 405, by=45) 
#colors <- c("red","white","white","white","white","blue") 

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

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/resultados/")
# writeRaster(r.winter, filename="pp_winter_mean_1980_2010_nueva_area_estudio_geo", format="GTiff", overwrite=TRUE)
# writeRaster(r.summer, filename="pp_summer_mean_1980_2010_nueva_area_estudio_geo", format="GTiff", overwrite=TRUE)


# NO LO BORRES!!!!!!!  NO LO BORRES!!!!!!!  NO LO BORRES!!!!!!!  NO LO BORRES!!!!!!!  NO LO BORRES!!!!!!!

# for (i in 1:length(names(r.stack.clip.19s))) {     # NO LO BORRES!!!!! ES UN LOOP QUE GUARDA CADA RASTER GENERADO!!!!
#   name <- paste(names(r.stack.clip.19s[[i]]), "clip_19s", sep = "_")
#   
#   setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/pp_2010/pp_2010_19s/")
#   writeRaster(r.stack.clip[[i]], filename=name, format="GTiff", overwrite=TRUE)
#   
# } 

# fin ---






















# Lectura de datos tmin ----
r.stack.tmin <- stack()

for (i in 1980:2010) {
  
  #  i <- 2001
  setwd(directory("tmin", i)) 
  if(i==1980){
    r.stack.tmin.pre <- stack(dir()[1], dir()[2], dir()[3], dir()[4], dir()[5], dir()[6], 
                            dir()[7], dir()[8], dir()[9], dir()[10], dir()[11], dir()[12])
    
  }else(  r.stack.tmin.pre <- stack(dir()[1], dir()[2], dir()[3], dir()[4], dir()[5], dir()[6], 
                                  dir()[7], dir()[8], dir()[9], dir()[10], dir()[11], dir()[12]))
  r.stack.tmin <- stack(r.stack.tmin, r.stack.tmin.pre)
}
names(r.stack.tmin)
sort(minValue(r.stack.tmin))
# fin ---




# Preparacion de coberturas ----

r.stack.tmin.clip <- crop(r.stack.tmin, marco)#marco.geo) # original
r.stack.tmin.clip <- stack(r.stack.tmin.clip)
sort(minValue(r.stack.tmin.clip))

#r.stack.tmin.clip.19s <- projectRaster(from = r.stack.tmin.clip, crs=utm19)

# fin ---



# calculo ----
seq.summer <- serie.i(r.stack.tmin)
length(seq.summer)

seq.winter <- setdiff(c(1:length(names(r.stack.tmin))), seq.summer)
length(seq.winter)

r.stack.tmin.summer <- r.stack.tmin.clip[[seq.summer]]
r.stack.tmin.winter <- r.stack.tmin.clip[[seq.winter]]

# setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/resultados/")
# writeRaster(r.stack.tmin.summer, filename='tmin_summer_set_raster_1980_2010_nueva_area_estudio_geo', format="GTiff", overwrite=TRUE)
# writeRaster(r.stack.tmin.winter, filename='tmin_winter_set_raster_1980_2010_nueva_area_estudio_geo', format="GTiff", overwrite=TRUE)


#r.summer.pre <- (r.stack.tmin.summer*10)-273.15
r.summer.pre <- mean(r.stack.tmin.summer)
r.summer <- overlay(r.summer.pre,
                    fun = function(r1) { return( (r1/10)-273.15 ) })
minValue(r.summer)
plot(r.summer)

r.winter.pre <- mean(r.stack.tmin.winter)
r.winter <- overlay(r.winter.pre,
                    fun = function(r1) { return( (r1/10)-273.15 ) })
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

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/resultados/")
# writeRaster(r.winter, filename="tmin_winter_mean_1980_2010_nueva_area_estudio_geo", format="GTiff", overwrite=TRUE)
# writeRaster(r.summer, filename="tmin_summer_mean_1980_2010_nueva_area_estudio_geo", format="GTiff", overwrite=TRUE)

# fin ---

















# Lectura de datos tmax ----
r.stack.tmax <- stack()

for (i in 1980:2005) { #1980:2010 ORIGINAL
  
  #  i <- 2001
  setwd(directory("tmax", i)) 
  if(i==1980){
    r.stack.tmax.pre <- stack(dir()[1], dir()[2], dir()[3], dir()[4], dir()[5], dir()[6], 
                              dir()[7], dir()[8], dir()[9], dir()[10], dir()[11], dir()[12])
    
  }else(  r.stack.tmax.pre <- stack(dir()[1], dir()[2], dir()[3], dir()[4], dir()[5], dir()[6], 
                                    dir()[7], dir()[8], dir()[9], dir()[10], dir()[11], dir()[12]))
  r.stack.tmax <- stack(r.stack.tmax, r.stack.tmax.pre)
}
names(r.stack.tmax)

# fin ---




# Preparacion de coberturas ----

r.stack.tmax.clip <- crop(r.stack.tmax, marco)#marco.geo) # original
r.stack.tmax.clip <- stack(r.stack.tmax.clip)

#r.stack.tmax.clip.19s <- projectRaster(from = r.stack.tmax.clip, crs=utm19)

# fin ---



# calculo ----
seq.summer <- serie.i(r.stack.tmax.clip)
length(seq.summer)

seq.winter <- setdiff(c(1:length(names(r.stack.tmax.clip))), seq.summer)
length(seq.winter)

r.stack.tmax.summer <- r.stack.tmax.clip[[seq.summer]]
r.stack.tmax.winter <- r.stack.tmax.clip[[seq.winter]]

# setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/resultados/")
# writeRaster(r.stack.tmax.summer, filename='tmax_summer_set_raster_1980_2010_nueva_area_estudio_geo', format="GTiff", overwrite=TRUE)
# writeRaster(r.stack.tmax.winter, filename='tmax_winter_set_raster_1980_2010_nueva_area_estudio_geo', format="GTiff", overwrite=TRUE)

#r.summer.pre <- (r.stack.tmin.summer*10)-273.15
r.summer.pre <- mean(r.stack.tmax.summer)
r.summer <- overlay(r.summer.pre,
                    fun = function(r1) { return( (r1/10)-273.15 ) })

plot(r.summer)

r.winter.pre <- mean(r.stack.tmax.winter)
r.winter <- overlay(r.winter.pre,
                    fun = function(r1) { return( (r1/10)-273.15 ) })
plot(r.winter)

breakpoints <- seq(-15, 20, by=5) 
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

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/variables_ambientales_chelsea/resultados/")
# writeRaster(r.summer, filename="tmax_summer_mean_1980_2005_nueva_area_estudio_geo", format="GTiff", overwrite=TRUE)

# writeRaster(r.winter, filename="tmax_winter_mean_1980_2010_nueva_area_estudio_geo", format="GTiff", overwrite=TRUE) # ORIGINAL
# writeRaster(r.summer, filename="tmax_summer_mean_1980_2010_nueva_area_estudio_geo", format="GTiff", overwrite=TRUE) # ORIGINAL

# fin ---
















