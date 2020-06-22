library('maxnet')

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/')

r1 <- raster('sen_slope_CSIRO_rcp85_pp_2011_2060_geo.tif')
r2 <- raster('sen_slope_CSIRO_rcp85_tmax_2011_2060_geo.tif')
r3 <- raster('sen_slope_CSIRO_rcp85_tmin_2011_2060_geo.tif')

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/')

r4 <- raster('sen_slope_CSIRO_rcp85_pp_2011_2060_geo.tif')
r5 <- raster('sen_slope_CSIRO_rcp85_tmax_2011_2060_geo.tif')
r6 <- raster('sen_slope_CSIRO_rcp85_tmin_2011_2060_geo.tif')

plot(r4)

r14 <- r1-r4
plot(r14)

r25 <- r2-r5
plot(r25)

r36 <- r3-r6
plot(r36)
