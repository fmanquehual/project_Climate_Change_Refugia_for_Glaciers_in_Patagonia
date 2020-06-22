require(ncdf4) #para tratamiento de datos nc
require(fields) #para visualizaci?n
require(maps) #para mapa base
library(raster)
library(RColorBrewer)
library(lubridate)

rm(list=ls())
dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/historico_GCMs/')

m.cdf <- nc_open('tasmax_global_CCSM4_historical_r1i1p1_CSIRO-CCAM-NRM50_v1_mon_1970-200911-seasavg.nc')
print(m.cdf)

m.cdf$natts

lat_variable <- 'lat'
lon_variable <- 'lon'
time_variable <- 'time'
nc_variable <- 'tasmax_october'

yrs <- 1970:2009
date.i <- ymd(yrs, truncated = 2L)

variable <- ncvar_get(m.cdf,nc_variable) #extracci?n valores variable
lats <- ncvar_get(m.cdf,lat_variable)
lons <- ncvar_get(m.cdf,lon_variable)
# lats <- c(-48.31578827, -40.73684311)
# lons <- c(-75, -71.25)
times <- ncvar_get(m.cdf,time_variable) # numero de anhos

length(lats)

dims_variable <- dim(variable) #extracci?n dimensiones variable (son 3)
#dates = as.Date(times, origin="1970-01-01 12:00:00") #generaci?n fechas
dates <- date.i
tmp_mat <- matrix(variable, nrow=dims_variable[1],ncol=dims_variable[2])

# ---

numero_de_matrices <- dim(variable)[3]

for (i in 1:numero_de_matrices) {
#  i <- 1
  r.i <- raster(xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), nrow=dims_variable[1],ncol=dims_variable[2])
  r.i[] <- variable[,,i] # llamo a la matriz i
  r.i[] <- r.i[]-273.15
  names(r.i) <- dates[i]
  r.j <- projectRaster(r.i, crs=wgs84, res = 0.5)
  
  if(i==1){r.out <- stack(r.j)} else(r.out <- stack(r.out, r.j))
  }

plot(r.out)

ej <- mean(r.out)
plot(ej, col = rev(brewer.pal(10, "RdBu")))

