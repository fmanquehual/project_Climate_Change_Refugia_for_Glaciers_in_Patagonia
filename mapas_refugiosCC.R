library('rgdal')
library('rgeos')
library('raster')
library('rasterVis')
library('stringr')
library('prettymapr')

rm(list=ls())
dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# funciones ----

# f1
axes.map <- function(l.i, raster.i){
  if(l.i=='lon'){l.j <- 1:2} else(l.j <- 3:4)
  if(l.i=='lon'){n.i <- 1} else(n.i <- 1)
  if(l.i=='lon'){sufijo <- '°W'} else(sufijo <- '°S')  
  l <- extent(raster.i)[l.j]
  l.min <- min(l)
  l.max <- max(l)
  l.min.j <- round(l.min, 0)
  l.max.j <- round(l.max, 0)
  seq.l.pre <- seq(l.min.j, l.max.j, by=n.i)
  seq.l <- paste( str_sub(seq.l.pre, 2, 3), sufijo, sep = '' )
  out.i <- data.frame(value=seq.l.pre, sufijo=seq.l)
  
  return(out.i)
}

# fin ---



# lectura coberturas ----


setwd("C:/Users/Usuario/Documents/Francisco/predicciones_maxent/refugiosCC/")

ref <- readOGR('.', 'polygon_referencia')
ref.19s <- spTransform(ref, utm19)
ref.19s <- gBuffer(ref.19s, width = 0)
ref <- spTransform(ref.19s, wgs84)
EJ <- data.frame(ID=c(1))
ref <- SpatialPolygonsDataFrame(ref, data = EJ, match.ID = FALSE)

predict.2050.rcp45 <- readOGR('.', 'polygon_refugioCC_csiro_2050_rcp45')
predict.2050.rcp85 <- readOGR('.', 'polygon_refugioCC_csiro_2050_rcp85')
predict.2070.rcp45 <- readOGR('.', 'polygon_refugioCC_csiro_2070_rcp45')
predict.2070.rcp85 <- readOGR('.', 'polygon_refugioCC_csiro_2070_rcp85')

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/todas_las_variables_predictoras_ok/topografico/')
dem <- raster('clip_dem_res_30m_nueva_area_estudio_geo.tif')
# ---


lat.i <- axes.map('lat', dem)
lon.i <- axes.map('lon', dem)

col.i <- c("#07283A", "#4D9DCA")
colors.i <- colorRampPalette(col.i)(100)

col.ref <- 'white'  
col.rcc <- '#10DB3B'
col.oceano <- 'black'
col.border.ref <- 'gray'
col.border.rcc <- '#096A1E'

line.i <- 0.5
cex.i <- 1


  setwd('C:/Users/Usuario/OneDrive/plots_paper/')
 # pdf('ej13.pdf', width = 7, height = 12.2)
  setEPS()
  postscript(file = "mapa_refugiosCC.eps", height = 12.2, width = 7)  # Una figura en cm
  par(mfrow=c(2,2),mai=c(0.5,0.5,0.3,0))
  
  # 1 
  plot(dem, col=colors.i, axes=FALSE, box=FALSE, legend = FALSE, colNA=col.oceano)
  mtext(text = 'A', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i)
  plot(ref, col=col.ref, border=col.border.ref, add=TRUE) 
  plot(predict.2050.rcp45, col=col.rcc, border=col.border.rcc, add=TRUE) 
  axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)
  axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)
  legend("bottomright", title=NULL, text.font = 2, c('Glacier', 'RCC'), 
         fill = c(col.ref, col.rcc), border = c(col.border.ref, col.border.rcc), 
         horiz=FALSE, cex=0.7, bg = 'white') 
  addscalebar(plotepsg=4326, pos = 'bottomleft', style = 'ticks', label.col = 'yellow', linecol = 'yellow') 
  addnortharrow(pos = "topleft", cols = c("white", "white"), text.col = "white", border = "white", scale = 0.7) 
  
  # 2
  plot(dem, col=colors.i, axes=FALSE, box=FALSE, legend = FALSE, colNA=col.oceano)
  mtext(text = 'B', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i)
  plot(ref, col=col.ref, border=col.border.ref, add=TRUE) 
  plot(predict.2070.rcp45, col=col.rcc, border=col.border.rcc, add=TRUE) 
  axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)
  axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)
  legend("bottomright", title=NULL, text.font = 2, c('Glacier', 'RCC'), 
         fill = c(col.ref, col.rcc), border = c(col.border.ref, col.border.rcc), 
         horiz=FALSE, cex=0.7, bg = 'white') 
  addscalebar(plotepsg=4326, pos = 'bottomleft', style = 'ticks', label.col = 'yellow', linecol = 'yellow') 
  addnortharrow(pos = "topleft", cols = c("white", "white"), text.col = "white", border = "white", scale = 0.7) 
  
  # 3
  plot(dem, col=colors.i, axes=FALSE, box=FALSE, legend = FALSE, colNA=col.oceano)
  mtext(text = 'C', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i)
  plot(ref, col=col.ref, border=col.border.ref, add=TRUE) 
  plot(predict.2050.rcp85, col=col.rcc, border=col.border.rcc, add=TRUE) 
  axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)
  axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)
  legend("bottomright", title=NULL, text.font = 2, c('Glacier', 'RCC'), 
         fill = c(col.ref, col.rcc), border = c(col.border.ref, col.border.rcc), 
         horiz=FALSE, cex=0.7, bg = 'white') 
  addscalebar(plotepsg=4326, pos = 'bottomleft', style = 'ticks', label.col = 'yellow', linecol = 'yellow') 
  addnortharrow(pos = "topleft", cols = c("white", "white"), text.col = "white", border = "white", scale = 0.7) 
  
  # 4
  plot(dem, col=colors.i, axes=FALSE, box=FALSE, legend = FALSE, colNA=col.oceano)
  mtext(text = 'D', side = 3, line = line.i, adj=0, outer = FALSE, col = 'black', font = 2, cex = cex.i)
  plot(ref, col=col.ref, border=col.border.ref, add=TRUE) 
  plot(predict.2070.rcp85, col=col.rcc, border=col.border.rcc, add=TRUE) 
  axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)
  axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)
  legend("bottomright", title=NULL, text.font = 2, c('Glacier', 'RCC'), 
         fill = c(col.ref, col.rcc), border = c(col.border.ref, col.border.rcc), 
         horiz=FALSE, cex=0.7, bg = 'white') 
  addscalebar(plotepsg=4326, pos = 'bottomleft', style = 'ticks', label.col = 'yellow', linecol = 'yellow') 
  addnortharrow(pos = "topleft", cols = c("white", "white"), text.col = "white", border = "white", scale = 0.7) 
  
  # legend
  plot(dem, col=colors.i, legend.only=TRUE, legend.shrink=1, legend.width=1, 
  axis.args=list(at=seq(0, 3700, 500),
                 labels=seq(0, 3700, 500),
                 cex.axis=0.7),
  legend.args=list(text='Elevation', side=4, font=2, line=2.5, cex=1))
  
  dev.off()
  
  # fin ---
  
  
  
  
  
  # otros ----
  
  # coord.poly <- function(polygon.i){
  #   polygon.i@data$id <- rownames(polygon.i@data)
  #   polygon.i.data <- fortify(polygon.i, region = 'id')
  #   polygon.i.df <- merge(polygon.i.data, polygon.i@data, by = "id")
  #   return(polygon.i.df)  
  # }
  
  # db.relieve <- as.data.frame(relieve, xy = TRUE) 
  # head(db.relieve)
  # names(db.relieve) <- c('x', 'y', 'relieve')
  # 
  # db.dem <- as.data.frame(dem, xy = TRUE) 
  # head(db.dem)
  # names(db.dem) <- c('x', 'y', 'altitud')
  # 
  # predict.2050.rcp45.db <- coord.poly(predict.2050.rcp45)
  # ref.db <- coord.poly(ref)
  # ref.db$Glacier <- as.factor(1)
  # head(ref.db)
  # 
  # 
  # oceano.db <- coord.poly(oceano)
  # 
  # # 1 ---
  # map <- ggplot() +
  #   geom_raster(data = db.dem, aes(x = x, y = y, fill = altitud)) +
  #   theme_classic() +
  #   theme(legend.position = 'right') +#, legend.key.size = unit(1, "cm") , axis.text = element_text(face="bold", size=14), legend.text = element_text(size = 14)) +
  #   labs(tag = '\n', y = '', x = '') +
  #   scale_fill_continuous('Elevation', #low = '#665821', high = '#B0B18D',
  #                         space = "Lab", na.value = "white", guide = "colourbar",
  #                         aesthetics = "fill") +
  #   
  #   geom_polygon(data = ref.db, aes(x=long, y = lat, group = group), fill = '#C3C3C0', color = "#C3C3C0") +
  #   geom_polygon(data = predict.2050.rcp45.db, aes(x=long, y = lat, group = group), fill = '#27F3F3', color = "#27F3F3") + # verde: #17DE20
  #   annotation_scale(location = "bl", text_col = 'yellow', style = 'ticks', line_col = 'yellow') +#, text_cex = 1, height =  unit(0.35,"cm")) + 
  #   annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_orienteering(text_col = "black", line_col = "black",
  #                                                                                                  fill = c("white", "black")), 
  #                          height = unit(1, "cm"), width = unit(1, "cm")) +
  #   coord_sf(crs=4326, expand = FALSE) 
  # 
  # map
