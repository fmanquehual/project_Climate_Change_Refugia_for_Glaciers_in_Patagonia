library('raster')
library('rgdal')
library('rgeos')

rm(list=ls())
dev.off()

# lectura de coberturas ----
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')

lim <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')
#plot(lim, axes=TRUE, border='red')  

# variables con dimensiones y mascara de interes
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/2050_2070/')
r.obj <- raster('CCSM4_pp_winter_mean_rcp45_2050.tif')

# fin ---


# RECORTE PENDIENTE ----
# modelo.i <- 'CSIRO'
# modelo.i <- 'CCSM4'
# modelo.i <- 'IPSL'
# modelo.i <- 'MIROC'

# anho.final <- 2060
#anho.final <- 2080

# rcp.i <- 'rcp45'
#rcp.i <- 'rcp85'

modelo.i <- c('CSIRO', 'CCSM4', 'IPSL', 'MIROC')
anho.final <- c(2060, 2080)
rcp.i <- c('rcp45', 'rcp85')

for (i in 1:4) {
  #i <- 1
  for (j in 1:2) {
    #j <- 1
    for (k in 1:2) {
      #k <- 1
      setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/sen_slope_y_p_value/')
      
name1 <- paste('sen_slope_', modelo.i[i], '_', rcp.i[k],'_pp_2011_', anho.final[j], '_geo.tif', sep = '')
name2 <- paste('sen_slope_', modelo.i[i], '_', rcp.i[k],'_tmin_2011_', anho.final[j], '_geo.tif', sep = '')
name3 <- paste('sen_slope_', modelo.i[i], '_', rcp.i[k],'_tmax_2011_', anho.final[j], '_geo.tif', sep = '')

r1 <- raster(name1) ; r1
r2 <- raster(name2) ; r2
r3 <- raster(name3) ; r3

rr1 <- resample(r1, r.obj, method='bilinear')
rr2 <- resample(r2, r.obj, method='bilinear')
rr3 <- resample(r3, r.obj, method='bilinear')

r1.1 <- crop(rr1, lim)
r1.2 <- crop(rr2, lim)
r1.3 <- crop(rr3, lim)

var.stack <- stack(r1.1, r1.2, r1.3)
plot(var.stack)

name.out1 <- paste('clip', name1, sep = '_')
name.out2 <- paste('clip', name2, sep = '_')
name.out3 <- paste('clip', name3, sep = '_')

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/coberturas_clip/PENDIENTE_VARIABLES/')
writeRaster(var.stack[[1]], filename=name.out1, format="GTiff", overwrite=TRUE)
writeRaster(var.stack[[2]], filename=name.out2, format="GTiff", overwrite=TRUE)
writeRaster(var.stack[[3]], filename=name.out3, format="GTiff", overwrite=TRUE)
}
  }
}

# fin ---








rm(list=ls())
dev.off()

# lectura de coberturas ----
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')

lim <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')
#plot(lim, axes=TRUE, border='red')  

# variables con dimensiones y mascara de interes
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/2050_2070/')
r.obj <- raster('CCSM4_pp_winter_mean_rcp45_2050.tif')

# fin ---


# RECORTE P-VALUE ---

# modelo.i <- 'CSIRO'
# modelo.i <- 'CCSM4'
# modelo.i <- 'IPSL'
# modelo.i <- 'MIROC'

# anho.final <- 2060
#anho.final <- 2080

# rcp.i <- 'rcp45'
#rcp.i <- 'rcp85'

modelo.i <- c('CSIRO', 'CCSM4', 'IPSL', 'MIROC')
anho.final <- c(2060, 2080)
rcp.i <- c('rcp45', 'rcp85')

for (i in 1:4) {
  #i <- 1
  for (j in 1:2) {
    #j <- 1
    for (k in 1:2) {
      #k <- 1
      setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/sen_slope_y_p_value/')
      
      name1 <- paste('p_value_sen_slope_', modelo.i[i], '_', rcp.i[k],'_pp_2011_', anho.final[j], '_geo.tif', sep = '')
      name2 <- paste('p_value_sen_slope_', modelo.i[i], '_', rcp.i[k],'_tmin_2011_', anho.final[j], '_geo.tif', sep = '')
      name3 <- paste('p_value_sen_slope_', modelo.i[i], '_', rcp.i[k],'_tmax_2011_', anho.final[j], '_geo.tif', sep = '')
      
      r1 <- raster(name1) ; r1
      r2 <- raster(name2) ; r2
      r3 <- raster(name3) ; r3
      
      rr1 <- resample(r1, r.obj, method='bilinear')
      rr2 <- resample(r2, r.obj, method='bilinear')
      rr3 <- resample(r3, r.obj, method='bilinear')
      
      r1.1 <- crop(rr1, lim)
      r1.2 <- crop(rr2, lim)
      r1.3 <- crop(rr3, lim)
      
      var.stack <- stack(r1.1, r1.2, r1.3)
      plot(var.stack)
      
      name.out1 <- paste('clip', name1, sep = '_')
      name.out2 <- paste('clip', name2, sep = '_')
      name.out3 <- paste('clip', name3, sep = '_')
      
      setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/coberturas_clip/P_VALUE/')
      writeRaster(var.stack[[1]], filename=name.out1, format="GTiff", overwrite=TRUE)
      writeRaster(var.stack[[2]], filename=name.out2, format="GTiff", overwrite=TRUE)
      writeRaster(var.stack[[3]], filename=name.out3, format="GTiff", overwrite=TRUE)
    }
  }
}

# fin ---