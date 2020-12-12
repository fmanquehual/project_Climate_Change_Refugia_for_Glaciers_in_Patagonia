library('raster')
library('rgdal')
library('rgeos')
library('gridExtra')
library('cowplot')
library('stringr')
library('prettymapr')
library('ggplot2')
library('ggspatial')

rm(list=ls())
dev.off()

# lectura de coberturas ----
setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')

#setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/')
lim <- readOGR('.', 'polygon_marco_trabajo_nuevo_geo')
plot(lim, axes=TRUE, border='red')  


#setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/CCSM4/rcp85/pp/2011/')
#setwd('C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/sen_slope/')
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/sen_slope_y_p_value/')
dir()

modelo.i <- 'CCSM4'
modelo.j <- 'MIROC'
modelo.k <- 'IPSL'
modelo.l <- 'CSIRO'

var.i <- 'tmax'

r1 <- raster(paste('sen_slope_', modelo.i, '_rcp85_', var.i, '_2011_2080_geo.tif', sep = ''))
r2 <- raster(paste('sen_slope_', modelo.j, '_rcp85_', var.i, '_2011_2080_geo.tif', sep = ''))
r3 <- raster(paste('sen_slope_', modelo.k, '_rcp85_', var.i, '_2011_2080_geo.tif', sep = ''))
r4 <- raster(paste('sen_slope_', modelo.l, '_rcp85_', var.i, '_2011_2080_geo.tif', sep = ''))

r1.1 <- crop(r1, lim)
r1.2 <- crop(r2, lim)
r1.3 <- crop(r3, lim)
r1.4 <- crop(r4, lim)

var.stack <- stack(r1.1, r1.2, r1.3, r1.4)
plot(var.stack)

r5 <- raster(paste('p_value_sen_slope_', modelo.i, '_rcp85_', var.i, '_2011_2080_geo.tif', sep = ''))
r6 <- raster(paste('p_value_sen_slope_', modelo.j, '_rcp85_', var.i, '_2011_2080_geo.tif', sep = ''))
r7 <- raster(paste('p_value_sen_slope_', modelo.k, '_rcp85_', var.i, '_2011_2080_geo.tif', sep = ''))
r8 <- raster(paste('p_value_sen_slope_', modelo.l, '_rcp85_', var.i, '_2011_2080_geo.tif', sep = ''))

r1.5 <- crop(r5, lim)
r1.6 <- crop(r6, lim)
r1.7 <- crop(r7, lim)
r1.8 <- crop(r8, lim)

p.value.stack <- stack(r1.5, r1.6, r1.7, r1.8)
plot(p.value.stack)

# fin ---





# funciones ----

plot.clim <- function(raster.i, var.i, value.min=NULL, value.max=NULL, letter.i=NULL){
  
if(var.i == 'pp'){col.i <- c('#A9D0F5', '#084B8A')} else(col.i <- c('#FFFF00', '#B40404'))
if(var.i == 'pp'){legend.name <- 'mm'} else(legend.name <- '?C')
if(is.null(letter.i)){letter <- NULL} else(letter <- letter.i)
if(is.null(value.min)){value.min.i <- minValue(raster.i)} else(value.min.i <- value.min)
if(is.null(value.max)){value.max.i <- maxValue(raster.i)} else(value.max.i <- value.max)

low.i <- col.i[1]
high.i <- col.i[2]

ggplot() +
  layer_spatial(raster.i) +
  theme_classic() +
  coord_sf(expand = FALSE) +
  ggtitle(letter.i) +
  scale_fill_continuous(legend.name, low = low.i, high = high.i,
                      space = "Lab", na.value = "white", guide = "colourbar",
                      aesthetics = "fill", limits=c((value.min.i-1), value.max.i)) +
  theme(legend.position = 'right', text = element_text(size=8)) +
  scale_x_continuous(breaks = c(-75, -73, -71))
}



# ---

plot.slope <- function(raster.i, var.i, letter.i=NULL){
  
  if(var.i == 'pp'){a <- scale_fill_viridis_c('Pendiente', na.value = 'white')} else(a <- scale_fill_viridis_c('Pendiente', na.value = 'white', option = 'inferno'))
  if(is.null(letter.i)){letter <- NULL} else(letter <- letter.i)
  
  ggplot() +
    layer_spatial(raster.i) +
    theme_classic() +
    coord_sf(expand = FALSE) +
    ggtitle(letter.i) +
    a +
    theme(legend.position = 'right', text = element_text(size=8)) +
    scale_x_continuous(breaks = c(-75, -73, -71))
}




# ---

plot.p_value <- function(raster.i, letter.i=NULL){
  
  if(is.null(letter.i)){letter <- NULL} else(letter <- letter.i)
  
  values.p_value <- c(0, 0.05, 0.5, 1)
  
    ggplot() +
      layer_spatial(raster.i) +
      theme_classic() +
      coord_sf(expand = FALSE) +
      ggtitle(letter.i) +
      scale_colour_viridis_c('p-value', space = "Lab", na.value = "white", guide = "colourbar",
                            aesthetics = "fill", limits=c(values.p_value[1], values.p_value[4]),
                            breaks=values.p_value[c(2:4)], option = 'cividis', direction = -1) +
      theme(legend.position = 'right', text = element_text(size=8)) +
      scale_x_continuous(breaks = c(-75, -73, -71))
}

# fin ---


slope1 <- plot.slope(var.stack[[1]], 'tmax')
slope2 <- plot.slope(var.stack[[2]], 'tmax')
slope3 <- plot.slope(var.stack[[3]], 'tmax')
slope4 <- plot.slope(var.stack[[3]], 'tmax')

p_value1 <- plot.p_value(p.value.stack[[1]])
p_value2 <- plot.p_value(p.value.stack[[2]])
p_value3 <- plot.p_value(p.value.stack[[3]])
p_value4 <- plot.p_value(p.value.stack[[4]])

# grid.arrange(top = 'CCSM4 RCP45 2011-2080',
#              clim1, p_value1,
#              clim2, p_value2,
#              clim3, p_value3,
#              ncol = 2, nrow = 3, widths=c(5, 2))

p <- plot_grid(slope1, p_value1,
          slope2, p_value2,
          slope3, p_value3,
          slope4, p_value4,
          labels=c("A", "B", 'C', 'D', 'E', 'F', 'G', 'H'), ncol = 2, nrow = 4)
p

setwd('C:/Users/Usuario/OneDrive/plots_paper/')

setEPS()
postscript(file = "predic_MAXENT_CSIRO.eps", height = 25/2.54, width = 20/2.54)  # Una figura en cm
#par(mar=c(5,4,4,2)+0.1) # The defualt margin
par(mar=c(4,4,0,0)+0.1)
p
dev.off()