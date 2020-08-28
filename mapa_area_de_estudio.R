library(raster)
library(rgdal)
library(rgeos)
library(prettymapr)
library(stringr)

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
marco.grande <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')
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
# cairo_ps("ubicacion_area_estudio_2_mapas.eps", width = 10, height = 6.35)
# par(mar=c(4,4,0,0)+0.1)

# par(mar=c(2,4,2,0)+0.1, mfrow=c(1,2))

# presente
plot(climas, alpha = 1, col=colors.i, useRaster=TRUE, asp = 1, legend = FALSE, axes=FALSE)
plot(oceano, col='#0b243b', add=TRUE) 
plot(g, alpha = 1, col='#FFFFFF', add=TRUE, legend=FALSE, useRaster=TRUE, asp = 1) 
plot(lim.sur, border=col.lim.sur, lwd=1.5, lty=2, add=TRUE)
plot(lim.austral, border=col.lim.austral, lwd=1.5, lty=2, add=TRUE)
mtext(text = 'A', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i)
axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1) 
axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1) 
addscalebar(style = 'ticks', linecol = 'white', label.col = 'white', pos = 'bottomright', plotepsg = 4326) 

# futuro
plot(climas.f, alpha = 1, col=colors.i, useRaster=TRUE, asp = 1, legend = FALSE, axes=FALSE)
plot(oceano, col='#0b243b', add=TRUE) 
plot(g, alpha = 1, col='#FFFFFF', add=TRUE, legend=FALSE, useRaster=TRUE, asp = 1) 
plot(lim.sur, border=col.lim.sur, lwd=1.5, lty=2, add=TRUE)
plot(lim.austral, border=col.lim.austral, lwd=1.5, lty=2, add=TRUE)
mtext(text = 'B', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i)
axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1) 
addscalebar(style = 'ticks', linecol = 'white', label.col = 'white', pos = 'bottomright', plotepsg = 4326) 
addnortharrow(pos = "topleft", cols = c("white", "white"), border = 'white', text.col = 'white', scale = 0.7) 

plot.new()

legend("topleft", title=NULL, text.font = 2, 
       legend = c('Study area', 'Arid', 'Temperate', 'Cold', 'Polar', 'Glaciers', 'Ocean'), 
       fill=c('transparent', col.j, "transparent", '#0b243b'), 
       border = c('red', col.j, "black", '#0b243b'),
       horiz=FALSE, cex=0.8, ncol = 4) 

legend("bottomright", title=NULL, text.font = 2, 
       legend = c('South glaciological region', 'Austral glaciological region'), 
       lty = 2, 
       col = c(col.lim.sur, col.lim.austral), 
       horiz=FALSE, cex=0.8, merge = TRUE) 

dev.off()

# fin ---








library(ggplotify)
library(cowplot)
library(prettymapr)
library(ggplot2)

setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
sud <- readOGR('.', 'sudamerica_clip')


setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/export/')
ocean.sud <- readOGR('.', 'polygon_oceano__sudamerica_geo')


# union de mapas ----
e1 <- as.grob(~c(plot(climas, alpha = 1, col=colors.i, useRaster=TRUE, asp = 0.695, legend = FALSE, axes=FALSE),
                 plot(oceano, col='#0b243b', add=TRUE), 
                 plot(g, alpha = 1, col='#FFFFFF', add=TRUE, legend=TRUE, useRaster=TRUE, asp = 1), 
                 plot(lim.sur, border=col.lim.sur, lwd=2, lty=2, add=TRUE),
                 plot(lim.austral, border=col.lim.austral, lwd=2, lty=2, add=TRUE),
                 mtext(text = 'A', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i),
                 axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1, padj=-1),
                 axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1, hadj=0.7), 
                 addscalebar(style = 'ticks', linecol = 'white', label.col = 'white', pos = 'bottomright', plotepsg = 4326) 
))

e2 <- as.grob(~c(#plot(ocean.sud, col='white', border='white', ylim=c(-53.5, 10.5)),
  plot(sud, col='gray', border='white', lwd=0.5, box=0),
  plot(ocean.sud, add=TRUE, col='white', border='black', lwd=0.4),
  plot(marco.grande, add=TRUE, border='red', lwd=1.4)))

e3 <- as.grob(~c(plot(climas.f, alpha = 1, col=colors.i, useRaster=TRUE, asp = 0.695, legend = FALSE, axes=FALSE),
                 plot(oceano, col='#0b243b', add=TRUE),
                 plot(g, alpha = 1, col='#FFFFFF', add=TRUE, legend=TRUE, useRaster=TRUE, asp = 1), 
                 plot(lim.sur, border=col.lim.sur, lwd=2, lty=2, add=TRUE),
                 plot(lim.austral, border=col.lim.austral, lwd=2, lty=2, add=TRUE),
                 mtext(text = 'B', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i),
                 axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1, padj=-1),
                 addscalebar(style = 'ticks', linecol = 'white', label.col = 'white', pos = 'bottomright', plotepsg = 4326),
                 addnortharrow(pos = "topleft", cols = c("white", "white"), border = 'white', text.col = 'white', scale = 0.7) 
))

e5 <- as.grob(~c(plot.new(),
                 legend("bottomleft", title=NULL, text.font = 2, 
                       legend = c('Study area', 'Arid', 'Temperate', 'Cold', 'Polar', 'Glaciers', 'Ocean'), 
                       fill=c('transparent', col.j, "transparent", '#0b243b'), 
                       border = c('red', col.j, "black", '#0b243b'),
                       horiz=FALSE, cex=0.8, ncol = 4, bty = 'n', y.intersp = 1.2
                       )
))

e6 <- as.grob(~c(plot.new(),
                 legend("bottomleft", title=NULL, text.font = 2, 
                        legend = c('South glaciological region', 'Austral glaciological region'), 
                        lty = 2, bty = 'n', y.intersp = 1.2, 
                        col = c(col.lim.sur, col.lim.austral), 
                        horiz=FALSE, cex=0.8, merge = TRUE) 
))
                 
vp=viewport(x=.227, y=.759, width=.44, height=.33)
e4 <- as.grob(~c(grid.newpage(),
                 grid.draw(e1),
                 pushViewport(vp),
                 grid.draw(e2),
                 upViewport()))

# fin ---


# plot out ----

setwd('C:/Users/Usuario/OneDrive/plots_paper/')

jpeg('ubicacion_area_estudio_3_mapas.jpg', width = 720, height = 530, units = "px", pointsize = 13,
     quality = 100, type = 'cairo', res = 95)

ggdraw(xlim = c(0, 30), ylim = c(0, 20), clip = 'on') +
  draw_plot(e4, x = -1.6, y = -1.5, width = 20.5, height = 23) +
  draw_plot(e3, x = 12.1, y = -1.5, width = 20.5, height = 23) +
  draw_plot(e5, x = -2.5, y = -4, width = 28, height = 21) +
  draw_plot(e6, x = 14, y = -4, width = 28, height = 21)

dev.off()

