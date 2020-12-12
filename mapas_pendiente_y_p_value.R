library('raster')
library('gridExtra')
library('cowplot')
library('stringr')
library('prettymapr')
library('ggplot2')
library('ggspatial')

rm(list=ls())
dev.off()

# f1
raster.to.db <- function(raster.i){
  r1 <- as.vector(as.matrix(raster.i))
  r2 <- na.omit(r1)
  db.r <- data.frame(values=r2)
  return(db.r)
}

# f2
plot.slope <- function(raster.i, var.i, letter.i=NULL){
  
  if(var.i == 'pp'){a <- scale_fill_viridis_c('Slope', na.value = 'white')} else(a <- scale_fill_viridis_c('Slope', na.value = 'white', option = 'inferno'))
  if(is.null(letter.i)){letter <- NULL} else(letter <- letter.i)
  
  ggplot() +
    layer_spatial(raster.i) +
    theme_classic() +
    coord_sf(expand = FALSE) +
    ggtitle(letter.i) +
    a +
    theme(legend.position = 'right', text = element_text(size=14),
          axis.text = element_text(face="bold", size=13)) +
    scale_x_continuous(breaks = c(-75, -73, -71)) +
    annotation_scale(location = "bl", text_col = 'black', style = 'ticks', line_col = 'black') +#, text_cex = 1, height =  unit(0.35,"cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_orienteering(text_col = "black", line_col = "black",
                                                                                                   fill = c("white", "black")),
                           height = unit(1, "cm"), width = unit(1, "cm"))
}

# f3
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
    theme(legend.position = 'right', text = element_text(size=14),
          axis.text = element_text(face="bold", size=13)) +
    scale_x_continuous(breaks = c(-75, -73, -71)) +
    annotation_scale(location = "bl", text_col = 'black', style = 'ticks', line_col = 'black') +#, text_cex = 1, height =  unit(0.35,"cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_orienteering(text_col = "black", line_col = "black",
                                                                                                   fill = c("white", "black")),
                           height = unit(1, "cm"), width = unit(1, "cm"))
}

# fin ---



# periodo referencia ----
setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/sen_slope_referencia/')
ref <- stack("sen_slope_pp_1980_2010_geo_ok.tif", 
             "sen_slope_tmin_1980_2010_geo_ok.tif",
             "sen_slope_tmax_1980_2010_geo_ok.tif")
plot(ref)

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/sen_slope_futuro/')
f <- stack("clip_sen_slope_CSIRO_rcp85_pp_2011_2080_geo_ok.tif", 
           "clip_sen_slope_CSIRO_rcp85_tmin_2011_2080_geo_ok.tif",
           "clip_sen_slope_CSIRO_rcp85_tmax_2011_2080_geo_ok.tif")
plot(f)

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/historico_CHELSA/sen_slope_y_p_value/p_value/')
pv.ref0 <- stack('p_value_sen_slope_pp_1980_2010_geo.tif',
                 'p_value_sen_slope_tmin_1980_2010_geo.tif',
                 'p_value_sen_slope_tmax_1980_2010_geo.tif')

pv.ref.pre0 <- resample(pv.ref0, ref[[1]], method='bilinear')
pv.ref <- mask(pv.ref.pre0, ref[[1]])

plot(pv.ref)  

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/output/coberturas_clip/P_VALUE/')
pv.f.pre <- stack("clip_p_value_sen_slope_CSIRO_rcp85_pp_2011_2080_geo.tif", 
                  "clip_p_value_sen_slope_CSIRO_rcp85_tmin_2011_2080_geo.tif",
                  "clip_p_value_sen_slope_CSIRO_rcp85_tmax_2011_2080_geo.tif")

pv.f.pre2 <- resample(pv.f.pre, ref[[1]], method='bilinear')
pv.f <- mask(pv.f.pre2, ref[[1]])
plot(pv.f)  


# fin ---





# plot pp ----
slope1 <- plot.slope(ref[[1]], 'pp')
slope2 <- plot.slope(f[[1]], 'pp')

p_value1 <- plot.p_value(pv.ref[[1]])
p_value2 <- plot.p_value(pv.f[[1]])

p.pp <- plot_grid(slope1, p_value1,
               slope2, p_value2,
               labels=c("AUTO"), ncol = 2, nrow = 2)
p.pp


# output
setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "mapa_pendiente_y_p_value_csiro_pp_ref_vs_futuro_2011_2080_rcp85.eps", height = 25/2.54, width = 20/2.54)  # Una figura en cm
# #par(mar=c(5,4,4,2)+0.1) # The defualt margin
# par(mar=c(4,4,0,0)+0.1)

# tiff('mapa_pendiente_y_p_value_csiro_pp_ref_vs_futuro_2011_2080_rcp85_300dpi.tiff',
#       width=8, height=11, units="in", res=300)

p.pp

dev.off()

# fin ---




# plot tmin ----
slope1 <- plot.slope(ref[[2]], 'tmin')
slope2 <- plot.slope(f[[2]], 'tmin')

p_value1 <- plot.p_value(pv.ref[[2]])
p_value2 <- plot.p_value(pv.f[[2]])

p.tmin <- plot_grid(slope1, p_value1,
                  slope2, p_value2,
                  labels=c("AUTO"), ncol = 2, nrow = 2)
p.tmin


# output
setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "mapa_pendiente_y_p_value_csiro_tmin_ref_vs_futuro_2011_2080_rcp85.eps", height = 25/2.54, width = 20/2.54)  # Una figura en cm
# #par(mar=c(5,4,4,2)+0.1) # The defualt margin
# par(mar=c(4,4,0,0)+0.1)

# tiff('mapa_pendiente_y_p_value_csiro_tmin_ref_vs_futuro_2011_2080_rcp85_300dpi.tiff',
#      width=8, height=11, units="in", res=300)

p.tmin

dev.off()

# fin ---




# plot tmax ----
slope1 <- plot.slope(ref[[3]], 'tmax')
slope2 <- plot.slope(f[[3]], 'tmax')

p_value1 <- plot.p_value(pv.ref[[3]])
p_value2 <- plot.p_value(pv.f[[3]])

p.tmax <- plot_grid(slope1, p_value1,
                    slope2, p_value2,
                    labels=c("AUTO"), ncol = 2, nrow = 2)
p.tmax


# output
setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "mapa_pendiente_y_p_value_csiro_tmax_ref_vs_futuro_2011_2080_rcp85.eps", height = 25/2.54, width = 20/2.54)  # Una figura en cm
# #par(mar=c(5,4,4,2)+0.1) # The defualt margin
# par(mar=c(4,4,0,0)+0.1)

# tiff('mapa_pendiente_y_p_value_csiro_tmax_ref_vs_futuro_2011_2080_rcp85_300dpi.tiff',
#      width=8, height=11, units="in", res=300)

p.tmax

dev.off()

# fin ---

# output plot ----
# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "predic_MAXENT_CSIRO.eps", height = 25/2.54, width = 20/2.54)  # Una figura en cm
# #par(mar=c(5,4,4,2)+0.1) # The defualt margin
# par(mar=c(4,4,0,0)+0.1)
# p
# dev.off()

# fin ---