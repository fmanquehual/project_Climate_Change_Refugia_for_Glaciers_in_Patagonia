library(rgdal)
library(rgeos)
library(raster)

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# Lectura coberturas ----

# marco de trabajo
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')

marco <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')

# variables con dimensiones y mascara de interes
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/2050_2070/')

r.obj <- raster('CCSM4_pp_winter_mean_rcp45_2050.tif')
plot(r.obj)

# variables promedio 1980 y 2011 CSIRO
setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/output/historico/")

r.1980.2011.CSIRO <- stack( dir() )
names(r.1980.2011.CSIRO)

# variables promedio 2050 y 2070 CSIRO
setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/output/2050_2070_CSIRO/")

r.2050.2070.CSIRO <- stack( dir() )
names(r.2050.2070.CSIRO)

# variables promedio 2050 y 2070 otros
setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/output/2050_2070/")

r.2050.2070.otros <- stack( dir() )
names(r.2050.2070.otros)

# variables de pendiente slope periodo futuro
setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/output/coberturas_clip/PENDIENTE_VARIABLES/")

r.pendiente.f <- stack( dir() )
names(r.pendiente.f)

# variables de pendiente slope periodo referencia
setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/historico_CHELSA/sen_slope_y_p_value/sen_slope/")

r.pendiente.r <- stack( dir() )
names(r.pendiente.r)

# variables topograficas
setwd("C:/Users/Usuario/Documents/Francisco/var_predictoras/topografico/")

r.topo <- stack( dir() )
names(r.topo)

# fin ---







# tamanho pixel y clip ----

obj.i <- c('r.1980.2011.CSIRO', 'r.2050.2070.CSIRO', 'r.2050.2070.otros', 'r.pendiente.f', 'r.pendiente.r', 'r.topo')

for (i in 1:length(obj.i) ) {
  #i <- 5
  obj.j <- eval(parse(text=obj.i[i]))
  
  obj.j.resample <- resample(obj.j, r.obj, method='bilinear')
  obj.j.clip <- crop(obj.j.resample, r.obj)
  obj.j.mask <- mask(obj.j.clip, r.obj)
  obj.j.mask <- stack(obj.j.mask)
  
  out <- paste( obj.i[i], '.mask', '<-obj.j.mask', sep = '' )
  eval(parse(text=out))
  print( paste('cobertura', i, 'listo de', length(obj.i), sep=' ') )
}

plot(r.1980.2011.CSIRO.mask)
names(r.1980.2011.CSIRO.mask)

# fin ---





# output ----

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/historico/')

for ( i in 1:length(names(r.1980.2011.CSIRO.mask)) ) {
  #i <- 2
  r.j <- r.1980.2011.CSIRO.mask[[i]]
  name.out <- paste( names(r.j), '_ok.tif', sep = '' )
  
  writeRaster(r.j, filename=name.out, format="GTiff", overwrite=TRUE)
}

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/2050_2070_CSIRO/')

for ( i in 1:length(names(r.2050.2070.CSIRO.mask)) ) {
#i <- 2
  r.j <- r.2050.2070.CSIRO.mask[[i]]
  name.out <- paste( names(r.j), '_ok.tif', sep = '' )

  writeRaster(r.j, filename=name.out, format="GTiff", overwrite=TRUE)
}


setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/2050_2070/')

for ( i in 1:length(names(r.2050.2070.otros.mask)) ) {
  #i <- 2
  r.j <- r.2050.2070.otros.mask[[i]]
  name.out <- paste( names(r.j), '_ok.tif', sep = '' )
  
  writeRaster(r.j, filename=name.out, format="GTiff", overwrite=TRUE)
}


setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/sen_slope_referencia/')

for ( i in 1:length(names(r.pendiente.r.mask)) ) {
  #i <- 2
  r.j <- r.pendiente.r.mask[[i]]
  name.out <- paste( names(r.j), '_ok.tif', sep = '' )
  
  writeRaster(r.j, filename=name.out, format="GTiff", overwrite=TRUE)
}


setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/sen_slope_futuro/')

for ( i in 1:length(names(r.pendiente.f.mask)) ) {
  #i <- 2
  r.j <- r.pendiente.f.mask[[i]]
  name.out <- paste( names(r.j), '_ok.tif', sep = '' )
  
  writeRaster(r.j, filename=name.out, format="GTiff", overwrite=TRUE)
}


setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/topografico_preliminar/')

for ( i in 1:length(names(r.topo.mask)) ) {
  #i <- 2
  r.j <- r.topo.mask[[i]]
  name.out <- paste( names(r.j), '_ok.tif', sep = '' )
  
  writeRaster(r.j, filename=name.out, format="GTiff", overwrite=TRUE)
}

# fin ---




# otros ----

# ej <- stack(r.2050.2070.CSIRO.mask,
# r.2050.2070.otros.mask,
# r.pendiente.f.mask,
# r.pendiente.r.mask,
# r.topo.mask)
# 
# names(ej)
