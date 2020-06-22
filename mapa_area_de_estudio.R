library('raster')
library('rgdal')
library('rgeos')
library('prettymapr')

# colores ----

# low = "#A9D0F5", high = "#084B8A" (Pp, desde casi blanco hasta azul oscuro)
# low = "#FFFF00", high = "#B40404" (temp, desde amarillo hasta rojo oscuro)
# para mas colores: https://html-color-codes.info/Codigos-de-Cores-HTML/

rm(list=ls())
dev.off()

##
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/inventario_glaciares_2015/Glaciares_Nacional/')
g.shp <- readOGR('.', 'polygon_glaciares_marco_trabajo_nuevo_geo')

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/topografico/')
dem <- raster('dem_res_30m_nueva_area_estudio_geo.tif')

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
relieve <- raster('relieve_nueva_area_estudio.tif')
climas <- raster('climas_koppen_nueva_area_estudio_reclas_geo.tif')
climas.f <- raster('climas_koppen_futuro_nueva_area_estudio_reclas_geo.tif')

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
g <- raster('glaciares_marco_trabajo_nuevo_geo.tif') 
oceano <- readOGR('.', 'polygon_oceano_geo')

lim.sur <- readOGR('.', 'polygon_zona_glaciologica_sur_chile_argentina_geo')
lim.austral <- readOGR('.', 'polygon_zona_glaciologica_austral_chile_argentina_geo')


axes.map <- function(l.i, raster.i){
  if(l.i=='lon'){l.j <- 1:2} else(l.j <- 3:4)
  if(l.i=='lon'){n.i <- 1} else(n.i <- 1)
  if(l.i=='lon'){sufijo <- '°W'} else(sufijo <- '°S')  
  l <- extent(relieve)[l.j]
  l.min <- min(l)
  l.max <- max(l)
  l.min.j <- round(l.min, 0)-1
  l.max.j <- round(l.max, 0)+1
  seq.l.pre <- seq(l.min.j, l.max.j, by=n.i)
  seq.l <- paste( str_sub(seq.l.pre, 2, 3), sufijo, sep = '' )
  out.i <- data.frame(value=seq.l.pre, sufijo=seq.l)
  
  return(out.i)
}


lat.i <- axes.map('lat', relieve)
lon.i <- axes.map('lon', relieve)

#colors.i <- colorRampPalette(c("#0d5f63", "#9dbf36", "#f1e751", "#e9912c"))(4)
#arg.i <- list(at = c(5, 1, 2, 3, 4) , labels = c('Glaciar', 'E', 'D', 'C', 'B'))
col.i <- c("#1B515A", "#5D7E0E", "#B8AC34", "#C07B0E")
col.j <- c("#C07B0E", "#B8AC34", "#5D7E0E", "#1B515A")
col.lim.sur <- '#C11A11'
col.lim.austral <- '#2B85AB'
colors.i <- colorRampPalette(col.i)(4)

line.i <- 0.5
cex.i <- 1

setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setwd('C:/Users/Usuario/Desktop/')
# pdf('ej.pdf', height = 10, width = 15)
cairo_ps("ubicacion_area_estudio_2_mapas.eps", width = 10, height = 6.35)
# par(mar=c(4,4,0,0)+0.1)

par(mar=c(2,4,2,0)+0.1, mfrow=c(1,2))

# presente
plot(climas, alpha = 1, col=colors.i, useRaster=TRUE, asp = 1, legend = FALSE, axes=FALSE)
#plot(relieve, col = grey(1:100/100), axes=FALSE, legend=FALSE, box=FALSE, useRaster=TRUE, asp = 1) 
plot(oceano, col='#0b243b', add=TRUE) 
plot(g, alpha = 1, col='#FFFFFF', add=TRUE, legend=FALSE, useRaster=TRUE, asp = 1) 
plot(lim.sur, border=col.lim.sur, lwd=1.5, lty=2, add=TRUE)
plot(lim.austral, border=col.lim.austral, lwd=1.5, lty=2, add=TRUE)
mtext(text = 'A', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i)
#grid() 
axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1) 
axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1) 
#plot(climas, alpha = 0.5, col=colors.i, add=TRUE, useRaster=TRUE, asp = 1, legend = FALSE) #, zlim=c(1,5), axis.args=arg.i, legend.shrink=0.2)
legend("bottomright", title=NULL, text.font = 2, legend = c('B', 'C', 'D', 'E', 'Glacier', 'Ocean', 'South', 'Austral'), 
       fill=c(col.j, "#FFFFFF", '#0b243b', NA, NA), border = c(col.j, "black", '#0b243b', NA, NA), 
       lty = c(NA, NA, NA, NA, NA, NA, 2, 2), merge = TRUE, col = c(col.lim.sur, col.lim.austral), horiz=FALSE, cex=0.8) 
addscalebar(label.col = 'white', plotepsg = 4326) 
addnortharrow(pos = "topleft", cols = c("white", "white"), border = 'white', text.col = 'white', scale = 0.7) 

# futuro
plot(climas.f, alpha = 1, col=colors.i, useRaster=TRUE, asp = 1, legend = FALSE, axes=FALSE)
#plot(relieve, col = grey(1:100/100), axes=FALSE, legend=FALSE, box=FALSE, useRaster=TRUE, asp = 1) 
plot(oceano, col='#0b243b', add=TRUE) 
plot(g, alpha = 1, col='#FFFFFF', add=TRUE, legend=FALSE, useRaster=TRUE, asp = 1) 
plot(lim.sur, border=col.lim.sur, lwd=1.5, lty=2, add=TRUE)
plot(lim.austral, border=col.lim.austral, lwd=1.5, lty=2, add=TRUE)
mtext(text = 'B', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i)
#grid() 
axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1) 
axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1) 
#plot(climas, alpha = 0.5, col=colors.i, add=TRUE, useRaster=TRUE, asp = 1, legend = FALSE) #, zlim=c(1,5), axis.args=arg.i, legend.shrink=0.2)
legend("bottomright", title=NULL, text.font = 2, legend = c('B', 'C', 'D', 'E', 'Glacier', 'Ocean', 'South', 'Austral'), 
       fill=c(col.j, "#FFFFFF", '#0b243b', NA, NA), border = c(col.j, "black", '#0b243b', NA, NA), 
       lty = c(NA, NA, NA, NA, NA, NA, 2, 2), merge = TRUE, col = c(col.lim.sur, col.lim.austral), horiz=FALSE, cex=0.8) 

addscalebar(label.col = 'white', plotepsg = 4326) 
addnortharrow(pos = "topleft", cols = c("white", "white"), border = 'white', text.col = 'white', scale = 0.7) 

dev.off()
