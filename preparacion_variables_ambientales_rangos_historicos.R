library(rgdal)
library(rgeos)
library(raster)
library(lattice)
library(trend)

rm(list = ls())
dev.off()

# var.i: puede ser "tmax", "tmin" o "pp"
directory.wc <- function(var.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"
  pre.folder <- "C:/Users/Usuario/Documents/Francisco/var_predictoras/historico_CHELSA"
  result <- paste(pre.folder, var.i, 'set', sep = "/")
  return(result)
}

var.j <- "tmax"
anho.inicio <- '1980'
anho.termino <- '2010'
periodo.j <- paste(anho.inicio, anho.termino, sep = '_') ; periodo.j

name.out <- paste('sen_slope', var.j, periodo.j, 'geo.tif', sep = '_') ; name.out
name.out2 <- paste('p_value','sen_slope', var.j, periodo.j, 'geo.tif', sep = '_') ; name.out2

# Lectura de datos tmax ----
setwd( directory.wc(var.i = var.j) )
r.stack <- stack(dir()[1])
names(r.stack)


# calculo ----

seq.i <- 1:(length(names(r.stack))/6) ; seq.i
r.i <- stack()

for (i in seq.i) {
  #i <- 50

  r.i.pre1 <- mean(r.stack[[ (1+(6*(i-1))):(6+(6*(i-1))) ]])
  
  if(var.j=='pp'){ r.i <- r.i.pre1            # para precipitacion
  }else(           r.i <- overlay(r.i.pre1,   # de Kelvin a Celsius
                   fun = function(r1) { return( (r1/10)-273.15 ) })
  )
  
  out <- paste('raster_', i, '<-r.i', sep = '')  
  plot( eval(parse(text=out)) )
  
  print( paste(i, 'objeto(s) de', max(seq.i) ) )
}

# fin ---





# Prep. series de tiempo ----
for (i in seq.i) { 
  #i <- 1
  
  out <- paste('raster_', i, sep = '')  
  
  ej0 <- as.data.frame( eval(parse(text=out)) )# convierte los valores del raster en df
  ej1 <- as.vector(unlist(ej0))# Transforma cada df en un vector con los valores del raster
  
  out <- paste('vector_raster_', i, '<-ej1', sep = '')  
  eval(parse(text=out))
  
  print( paste(i, 'objeto(s) generado(s) de', max(seq.i), sep=' ') )
}

# fin ---




# Calculo de pendiente ----

r.ej <- raster_1
r.ej[] <- NA

p.value <- raster_1
p.value[] <- NA

valores_pixel <- c()

#i.i <- 2167 # colorcar esto en el loop 'i' para generar un grafico correcto
i.i <- 1:ncell(raster_1)

for (i in i.i) { # De la serie de tiempo 'j', se extraen los valores para un pixel 'i'
  #i <- 1
  
  for (j in seq.i) {
    #j <- 14
    
    out2 <- paste('vector_raster_', j, sep = '')  
    valor <- eval(parse(text=out2))[i]
    
    if(j==1){valores_pixel <- valor}else(valores_pixel <- c(valores_pixel, valor))
  }
  
  ej.ts <- ts(valores_pixel, start = 1980, freq = 1)
  
  slope.i <- sens.slope(ej.ts,) ; slope.i
  
  r.ej[i] <- slope.i$estimates
  p.value[i] <- slope.i$p.value
  
  print( paste('Pixel ', i, ' listo de ', length(i.i), sep = '' ) )
}

# setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
# marco <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')

plot(r.ej)
r.out <- r.ej

plot(p.value)
r.out2 <- p.value

# plot(marco, add=T)
# 
# r.out <- mask(r.ej, marco)
# plot(r.out)

setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/")
#writeRaster(r.out, filename=name.out, format="GTiff", overwrite=TRUE)
#writeRaster(r.out2, filename=name.out2, format="GTiff", overwrite=TRUE)








# Grafico de tendencia ----

valores_pixel
plot(ej.ts)

ej.ts <- ts(valores_pixel, start = 1980, freq = 1)
xyplot(ej.ts~c(anho.inicio:anho.termino),  
       col='darkgreen',
       ylab='Temperatura máxima (°C)',
       xlab='Serie de Tiempo', 
       main= paste('Valores para el pixel numero ', max(i.i), sep=''),
       sub= paste('Sens Slope = ', round(slope.i$estimates, 3), sep = ''),
       as.table=T,
       type = c("g", "p", "smooth"),
       col.line ="black")# la linea me indica una tendencia.

# fin ---


# Pendiente de Sen:

# Esta macro calcula la pendiente de Sen, que es una alternativa no paramétrica 
# para estimar una pendiente para una serie de tiempo univariada. Este enfoque 
# implica el cálculo de pendientes para todos los pares de puntos de tiempo ordinales 
# (i.e., ordenados) y luego utilizar la mediana de estas pendientes como una estimación de la 
# pendiente general. La pendiente de Sen es insensible a valores atípicos y 
# puede utilizarse para detectar si hay una tendencia en los datos.

# Fuente: https://support.minitab.com/es-mx/minitab/18/macro-library/macro-files/nonparametrics-macros/senslope/

# No parametrico: no dehen ajustarse a ninguna distribución (e.g., ~ N).
# Fuente: https://help.xlstat.com/s/article/cual-es-la-diferencia-entre-pruebas-parametricas-y-no-parametricas?language=es
