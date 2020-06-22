library('raster')
library('rgdal')
library('rgeos')

rm(list=ls())
dev.off()

# funciones ----

# f1 ---
busqueda.carpeta.trabajo <- function(modelo.i, rcp.i, anho.i, var.i){ # permite generar nuevos directorios de trabajo desde la carpeta "variables_ambientales_chelsea"

  pre.folder <- "C:/Users/Usuario/Documents/Francisco/var_predictoras"
  result <- paste(pre.folder, modelo.i, rcp.i, anho.i, var.i, sep = "/")
  return(result)

}


# f2 ---
read.layers.CSIRO <- function(modelo.i, rcp.i, anho.i, var.i, rango.j){

  setwd(busqueda.carpeta.trabajo(modelo.i, rcp.i, anho.i, var.i))
  
  if(modelo.i!='CSIRO_2050_2070'){ print('Esta función solo funciona para el GCM CSIRO') }
  
  if( var.i=='pp' ){ var.k <- 'pr' } else( if( var.i=='tmax' ){ var.k <- 'tasmax' } else( var.k <- 'tasmin' ) )

  if( var.i=='pp' ){ tif.i <- '.tif' } else( tif.i <- '_V1.2.tif' )
  
  for (i in rango.j) {

    if(anho.i==2050){r.j <- paste('CHELSA_', var.k, '_mon_CSIRO-Mk3-6-0_', rcp.i, '_r1i1p1_g025.nc_', i, '_2041-2060', tif.i, sep = '')} else(
      r.j <- paste('CHELSA_', var.k, '_mon_CSIRO-Mk3-6-0_', rcp.i, '_r1i1p1_g025.nc_', i, '_2061-2080', tif.i, sep = '') )

    if(i==rango.j[1]){ r.stack <- raster(r.j) } else( r.stack <- stack(r.stack, raster(r.j)) )
  }
  
  return(r.stack)
}


# f3 ---

read.layers.others <- function(modelo.i, modelo.name.i, rcp.i, anho.i, var.i, rango.j){
  
  if(modelo.i=='CSIRO_2050_2070'){ print('Esta función fue hecha para los GCMs CCSM4, IPSL y MIROC') }
  
  setwd(busqueda.carpeta.trabajo(modelo.i, rcp.i, anho.i, var.i))

  if( var.i=='pp' ){ var.k <- 'pr' } else( if( var.i=='tmax' ){ var.k <- 'tx' } else( var.k <- 'tn' ) )
  
  if( modelo.name.i=='CCSM4' ){ modelo.k <- 'cc' } else( if( modelo.name.i=='IPSL' ){ modelo.k <- 'ip' } else( modelo.k <- 'mr' ) )

  for (i in rango.j) {
    
    r.j <- paste(modelo.k, str_sub(rcp.i, 4, 5), var.k, str_sub(anho.i, 3, 4), i, '.tif', sep = '') 
    
    if(i==rango.j[1]){ r.stack <- raster(r.j) } else( r.stack <- stack(r.stack, raster(r.j)) )
  } 
  
  return(r.stack)
}

# fin ---




# lectura de coberturas ----

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')

marco <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')

#modelo.name.j <- 'CSIRO'
#modelo.name.j <- 'CCSM4'
#modelo.name.j <- 'IPSL'
modelo.name.j <- 'MIROC'

modelo.j <- paste(modelo.name.j, '_2050_2070', sep = '')

#var.j <- 'pp'
#var.j <- 'tmin'
var.j <- 'tmax'

#rcp.j <- 'rcp45'
rcp.j <- 'rcp85'

anho.j <- 2050
#anho.j <- 2070

estacion.j <- 'summer'
#estacion.j <- 'winter'

if(estacion.j=='summer'){rango.j <- c(1:3, 10:12)} else(rango.j <- c(4:9))

name.out <- paste(modelo.name.j, '_', var.j, '_', estacion.j, '_mean_', rcp.j, '_', anho.j, sep = '') ; name.out

# r.stack <- read.layers.CSIRO(modelo.j, rcp.j, anho.j, var.j, rango.j)
# names(r.stack)

r.stack <- read.layers.others(modelo.j, modelo.name.j, rcp.j, anho.j, var.j, rango.j)
names(r.stack)

minValue(r.stack)
maxValue(r.stack)

# fin ---




# Preparacion de coberturas ----

r.stack.clip <- crop(r.stack, marco)
r.stack.clip <- stack(r.stack.clip)

# calculo 

if(var.j=='pp'){ r.i <- mean(r.stack.clip) } else(
  
  r.i <- overlay(r.stack.clip,   # de Kelvin a Celsius
                 fun = function(r1) { return( mean(r1/10) ) })
)

#if(modelo.name.j=='CSIRO'){r.winter[r.i[]%in%-32767] <- NA}

maxValue(r.i)
minValue(r.i)
plot(r.i)

winter.min.text <- paste("Min:", round(minValue(r.i),2), sep=" ")
winter.max.text <- paste("Max:", round(maxValue(r.i),2), sep=" ")

titulo.i <- paste(var.j, estacion.j, rcp.j, anho.j, sep = ' ')

if(var.j=='pp'){ col.i <- hcl.colors(9, palette = "viridis", rev = TRUE) } else(
  col.i <- heat.colors(9, alpha = 1, rev = TRUE)
)

plot(r.i, col=col.i, main = titulo.i)
legend("bottomright", legend = c(winter.min.text, winter.max.text), bty="n")

# fin ---


# output ----

name.out

setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/output/2050_2070/")
writeRaster(r.i, filename=name.out, format="GTiff", overwrite=TRUE)
