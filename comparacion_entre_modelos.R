library('raster')

# funciones ----

# modelo: 'CSIRO', 'IPSL', 'CCSM4', 'MIROC'
# rcp: 'rcp45', 'rcp85'
# anho.i: '2050', '2070'

# f1
var.futuro <- function(modelo, rcp, anho){

modelo.i <- modelo
rcp.i <- rcp
anho.i <- anho
  
# variables climaticas ---

if(modelo=='CSIRO'){folder.i <- '2050_2070_CSIRO'} else(folder.i <- '2050_2070')
directory.pre.i <- 'C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok'
directory.i <- paste(directory.pre.i, folder.i, sep = '/')

setwd(directory.i)

if(anho.i==2050){anho.j <- '2011_2060'} else(anho.j <- '2011_2080')

k1 <- grep(pattern = modelo.i, dir(), value = T)
k2 <- grep(pattern = rcp.i, k1, value = T)
k3 <- grep(pattern = anho.i, k2, value = T)

stack.clim.f <- stack( k3 )

# variables de pendiente futuro ---

folder.i <- 'sen_slope_futuro'

directory.i <- paste(directory.pre.i, folder.i, sep = '/')
setwd(directory.i)

k1 <- grep(pattern = modelo.i, dir(), value = T)
k2 <- grep(pattern = rcp.i, k1, value = T)
k3 <- grep(pattern = anho.j, k2, value = T)

stack.sen.f <- stack(k3)

# variables topograficas ---

folder.i <- 'topografico'

directory.i <- paste(directory.pre.i, folder.i, sep = '/')
setwd(directory.i)

stack.topo <- stack( dir() )

# output ---

var.amb <- stack(stack.clim.f, stack.sen.f, stack.topo)
return(var.amb)
}

# f2
var.referencia <- function(){

# variables de pendiente referencia ---

folder.i <- 'historico'

directory.pre.i <- 'C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok'
directory.i <- paste(directory.pre.i, folder.i, sep = '/')

setwd(directory.i)

stack.clim.r <- stack( dir() )

# variables de pendiente referencia ---

folder.i <- 'sen_slope_referencia'

directory.i <- paste(directory.pre.i, folder.i, sep = '/')
setwd(directory.i)

stack.sen.r <- stack( dir() )

# variables topograficas ---
folder.i <- 'topografico'

directory.i <- paste(directory.pre.i, folder.i, sep = '/')
setwd(directory.i)

stack.topo <- stack( dir() )

# output ---

var.amb <- stack(stack.clim.r, stack.sen.r, stack.topo)
return(var.amb)
}

# f3
eval.stack <- function(stack.i){
  stack.j <- stack.i
  names(stack.j) <- c('IPSL', 'MIROC', 'CCSM4', 'CSIRO')
  
  mean.i <- cellStats(stack.j, stat = 'mean', na.rm=TRUE)  
  sd.i <- cellStats(stack.j, stat = 'sd', na.rm=TRUE)  
  min.i <- cellStats(stack.j, stat = 'min', na.rm=TRUE)
  max.i <- cellStats(stack.j, stat = 'max', na.rm=TRUE)
  
  label.i <- paste('SD = ', round(sd.i, 2), sep = '') ; label.i
  
  boxplot(stack.j, ylab = label.y, xlab = 'GCMs', main = title.j)
  points(mean.i, col='red', pch=16)
  text(x=1, y=(max(max.i)+0.5), labels=label.i[1], cex=0.8)
  text(x=2, y=(max(max.i)+0.5), labels=label.i[2], cex=0.8)
  text(x=3, y=(max(max.i)+0.5), labels=label.i[3], cex=0.8)
  text(x=4, y=(max(max.i)+0.5), labels=label.i[4], cex=0.8)
}

# fin ---





# lectura coberturas ----

rcp.k <- 'rcp85'
anho.k <- 2050
  
ipsl <- var.futuro('IPSL', rcp.k, anho.k)
miroc <- var.futuro('MIROC', rcp.k, anho.k)
csiro <- var.futuro('CSIRO', rcp.k, anho.k)
ccsm4 <- var.futuro('CCSM4', rcp.k, anho.k)
plot(ccsm4)

referencia <- var.referencia()
plot(referencia)

# fin ---




# Comparacion entre modelos ----

# variables climaticas ---

# mayor a menor sensibilidad a CO2
pp <- stack(ipsl[[1]], miroc[[1]], ccsm4[[1]], csiro[[1]])
tmax <- stack(ipsl[[2]], miroc[[2]], ccsm4[[2]], csiro[[2]])
tmin <- stack(ipsl[[3]], miroc[[3]], ccsm4[[3]], csiro[[3]])

# plots
title.j <- paste('Comparacion', rcp.k, anho.k, sep = ' ') ; title.j

label.y <- 'Temperatura minima (C)'
eval.stack(tmin)

label.y <- 'Temperatura maxima (C)'
eval.stack(tmax)

label.y <- 'Precipitacion (mm)'
eval.stack(pp)


# pendiente Sen ---

# mayor a menor sensibilidad a CO2
pp.slope <- stack(ipsl[[4]], miroc[[4]], ccsm4[[4]], csiro[[4]])
tmax.slope <- stack(ipsl[[5]], miroc[[5]], ccsm4[[5]], csiro[[5]])
tmin.slope <- stack(ipsl[[6]], miroc[[6]], ccsm4[[6]], csiro[[6]])

# plots
title.j <- paste('Comparacion', rcp.k, anho.k, sep = ' ') ; title.j

label.y <- 'pendiente temperatura minima'
eval.stack(tmin.slope)

label.y <- 'pendiente temperatura maxima'
eval.stack(tmax.slope)

label.y <- 'pendiente precipitacion'
eval.stack(pp.slope)

# fin ---

