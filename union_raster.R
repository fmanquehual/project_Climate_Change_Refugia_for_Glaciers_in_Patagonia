library('raster')
library('cowplot')
library('ggplot2')
library('ggspatial')

rm(list=ls())
dev.off()

# funciones ----
colours.i <- c('#E7E4E4', '#CAC8C8', '#315669', '#181C4D')

plot.map <- function(raster.i){
  p.i <- ggplot() +
    layer_spatial(raster.i) +
    scale_fill_gradientn('Probability', colours = colours.i, na.value = 'white') +
    theme_classic() +
    theme(legend.key.size = unit(0.7, "cm"), legend.position = 'bottom', legend.direction = 'horizontal') +
    coord_sf(expand = FALSE) +
    annotation_scale(location = "bl", text_col = 'black', style = 'ticks', line_col = 'black') + #, line_width = 1.5, text_cex = 1.05) +
    annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"))
  return(p.i)  
}

plot.map.without.legend <- function(raster.i){
  p.i <- ggplot() +
    layer_spatial(raster.i) +
    scale_fill_gradientn('Prob', colours = colours.i, na.value = 'white') +
    theme_classic() +
    theme(legend.position = 'none', text = element_text(size=8) ) +
    coord_sf(expand = FALSE) +
    annotation_scale(location = "bl", text_col = 'black', style = 'ticks', line_col = 'black') + #, line_width = 1.5, text_cex = 1.05) +
    annotation_north_arrow(location = "tl", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
  return(p.i)  
}

# fin ---

setwd('C:/Users/Usuario/Documents/Francisco/predicciones_maxent/')

r.ref1 <- raster('prediccion_periodo_referencia_sur.tif')
r.ref2 <- raster('prediccion_periodo_referencia_austral.tif')
u0 <- merge(r.ref1, r.ref2)

r1 <- raster('prediccion_csiro_2050_rcp45_zona_sur.tif')
r2 <- raster('prediccion_csiro_2050_rcp45_zona_austral.tif')
u1 <- merge(r1, r2)

r3 <- raster('prediccion_csiro_2050_rcp85_zona_sur.tif')
r4 <- raster('prediccion_csiro_2050_rcp85_zona_austral.tif')
u2 <- merge(r3, r4)

r5 <- raster('prediccion_csiro_2070_rcp45_zona_sur.tif')
r6 <- raster('prediccion_csiro_2070_rcp45_zona_austral.tif')
u3 <- merge(r5, r6)

r7 <- raster('prediccion_csiro_2070_rcp85_zona_sur.tif')
r8 <- raster('prediccion_csiro_2070_rcp85_zona_austral.tif')
u4 <- merge(r7, r8)

#plot(u1>0.435)

p0 <- plot.map(u0)
p1 <- plot.map.without.legend(u1)
p2 <- plot.map.without.legend(u2)
p3 <- plot.map.without.legend(u3)
p4 <- plot.map.without.legend(u4)

p.group <- plot_grid(p1, p2,
                     p3, p4,
                     labels=c('B', 'C', 'D', 'E'), ncol = 2, nrow = 2)
p.out <- plot_grid(p0, p.group,
                  labels=c('A', ''), ncol = 2, nrow = 1)

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "predic_MAXENT_CSIRO.eps", height = 16/2.54, width = 20/2.54)  # Una figura en cm
# par(mar=c(4,4,0,0)+0.1)
p.out
# dev.off()
