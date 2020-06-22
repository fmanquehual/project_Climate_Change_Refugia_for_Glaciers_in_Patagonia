library(lattice)
library(trend)
library(rgdal)
library(rgeos)
library(raster)


wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84



# funciones ----

# tmin, tmax, pp
# GCMs: CSIRO, MIROC, CCSM4, IPSL
directory <- function(GCM.i, var.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"
  pre.directory <- "C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_para_calculo_pendiente"
  result <- paste(pre.directory, GCM.i, var.i, sep = "/")
  return(result)
}

# fin ---




# Lectura de datos ----

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
marco <- readOGR(".", "polygon_marco_trabajo_19s")

# variables: tmin, tmax, pp
# GCMs: CSIRO, MIROC, CCSM4, IPSL
GCM.j <- 'CSIRO'
var.j <- 'pp'
periodo <- '2011_2060'

setwd(directory(GCM.j, var.j))

name.out <- paste('sen_slope', GCM.j, 'rcp85', var.j, periodo, '19s.tif', sep = '_') ; name.out

dir() # 1 es anho 2011 y 70 es anho 2080
r.stack <- stack(dir()[1:50])
names(r.stack)

# fin ---





# Reclasificacion ---- 
m = c(NA, NA, 0)

rclmat = matrix(m, ncol=3, byrow=TRUE)
r.stack1 <- reclassify(r.stack, rclmat)
r.stack1 <- stack(r.stack1)
plot(r.stack1[[1:4]])


# r.stack2 <- overlay(r.stack1, # para temperaturas
#                     fun = function(r1) { return( r1-273.15 ) })

r.stack2 <- overlay(r.stack1[[1]], # para pp
                    fun = function(r1) { return( (r1*86400) ) })

  plot(r.stack2)

# fin ---









# Prep. series de tiempo ----
for (i in 1:length(r.stack2@layers)) { 
  #i <- 2
  ej0 <- as.data.frame(r.stack2[[i]])# convierte los valores del raster en df
  ej1 <- as.vector(unlist(ej0))# Transforma cada df en un vector con los valores del raster
  
  out <- paste('raster_', i, '<-ej1', sep = '')  
  eval(parse(text=out))
  
  print( paste(i, ' objeto(s) generado(s) de ', length(r.stack2@layers))  )
}

# fin ---








# Calculo de pendiente ----

r.ej <- r.stack[[1]]
r.ej[] <- NA
valores_pixel <- c()

#i.i <- 2167 # colorcar esto en el loop 'i' para generar un grafico correcto
i.i <- 1:ncell(r.stack2)

for (i in i.i) { # De la serie de tiempo 'j', se extraen los valores para un pixel 'i'
#i <- 1575

  for (j in 1:length(r.stack@layers)) {
  #j <- 5
  
  out2 <- paste('raster_', j, sep = '')  
  valor <- eval(parse(text=out2))[i]
    
  if(j==1){valores_pixel <- valor}else(valores_pixel <- c(valores_pixel, valor))
  }
  
  ej.ts <- ts(valores_pixel, start = 2011, freq = 1)
  
  slope.i <- sens.slope(ej.ts,) ; slope.i

  r.ej[i] <- slope.i$estimates
  
  print( paste('Pixel ', i, ' listo de ', length(i.i), sep = '' ) )
}

plot(r.ej)
plot(marco, add=T)

r.out <- mask(r.ej, marco)
plot(r.out)

setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/var_predict_glacier_presence/var_predict/")

#writeRaster(r.out, filename=name.out, format="GTiff", overwrite=TRUE)


# fin ---






# Grafico de tendencia ----

valores_pixel
plot(ej.ts)

ej.ts <- ts(valores_pixel, start = 2011, freq = 1)
xyplot(ej.ts~c(2011:2060),  
  col='darkgreen',
  ylab='Temperatura máxima (K)',
  xlab='Serie de Tiempo', 
  main= paste('Valores para el pixel numero ', i.i, sep=''),
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







# # ejemplo de calculo de pendiente ----
# data(maxau)
# maxau
# sens.slope(maxau[,"s"])
# mk.test(maxau[,"s"])
# 
# #En pocas palabras, cuando su análisis de tendencia le da una tendencia 
# #significativa (positiva o negativa) la pendiente de Sen es que para capturar 
# #la magnitud de esa tendencia. Digamos que su prueba de MK reveló que la temperatura 
# #aumentó anualmente entre 1950-2000, aquí la pendiente de Sen le dirá en promedio cuánto
# #ha cambiado la temperatura cada año.
# 
# # Fuente: https://www.researchgate.net/post/what_is_Sens_slope_magnitude
# 
# db <- data.frame(x=as.vector(unlist(maxau[,"s"])), y=c(1965:2009))
# str(db)
# zyp.sen(y~x, db)
# 
# require(lattice)
# 
# xyplot(#ht~dap|esp, data=db, 
#   maxau[,"s"]~c(1965:2009),  
#   col='darkgreen',
#   ylab='Valores MAXAU',
#   xlab='Serie de Tiempo', 
#   as.table=T,
#   type = c("g", "p", "smooth"),
#   col.line ="black")# la linea me indica una tendencia.
# 
# 
# 
# library(zyp)
# df=data.frame(x=c(1,2,3,4,5),y=c(3,6,8,1,9))
# df$anho <- 2001:2005
# 
# xyplot(#ht~dap|esp, data=db, 
#   df$x~df$anho,  
#   col='darkgreen',
#   ylab='Valores zyp',
#   xlab='Serie de Tiempo', 
#   as.table=T,
#   type = c("g", "p", "smooth"),
#   col.line ="black")# la linea me indica una tendencia.
# 
# zyp.sen(y~x,df)
# # fin ---