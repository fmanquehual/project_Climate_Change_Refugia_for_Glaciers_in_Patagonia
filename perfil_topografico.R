library(rgdal)
library(rgeos)
library(raster)

setwd("C:/Users/Francisco/Dropbox/Por subir/datos/")
# direccion NORTE-SUR ----

ns <- read.csv("perfil_topografico_N_S.csv")
max(ns$ALTITUD)
ns$DISTANCIA_M[ns$ALTITUD%in%max(ns$ALTITUD)]
summary(ns$ALTITUD)

plot(ns$DISTANCIA_M, ns$ALTITUD, type = "l", xlab="Distancia (m)", ylab="Altitud",
     main="Perfil Topográfico Sur-Norte")

# fin ---



# direccion SUROESTE-NORESTE ----

so.ne <- read.csv("perfil_topografico_SO_NE.csv")
max(so.ne$ALTITUD)
so.ne$DISTANCIA_M[so.ne$ALTITUD%in%max(so.ne$ALTITUD)]

plot(so.ne$DISTANCIA_M, so.ne$ALTITUD, type = "l", xlab="Distancia (m)", ylab="Altitud",
     main="Perfil Topográfico Suroeste-Noreste")

# fin ---




setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/")
# coberturas ----
lim <- readOGR(".", "polygon_cuenca_baker_chile_grass_19s")
p <- readOGR(".", "points_perfil_topografico_19s")
l <- readOGR(".", "lines_perfil_topografico_19s")
r <- raster("dem_baker_grande_clip_19s.tif")

setwd("C:/Users/Francisco/Dropbox/Por subir/mapas_y_graficos/")
#png("perfil_topografico.png", width = 680, height = 420, units = "px")

layout(matrix(c(1,2,
                1,3), 2, 2, byrow = TRUE))
par(oma=c(4,0,0,2), mar=c(0,4,2,0))

plot(lim, border="red", axes=TRUE)
contour(r, add=TRUE)
plot(l, add=TRUE, pch=16, col="red", lty=2)
plot(p, add=TRUE, pch=16, col="blue", cex=1.3)
text(p, p@data$NUMERO, pos=3, font=2, cex=1.2, halo=TRUE, hc='black', col='white', hw=0.2)
mtext("A)", font = 2, line = -1.2, adj = 0.02)

legend("topright", legend = c("Limite cuenca", "Extremos de transectos"), 
       fill=NULL, border = c("red", NA), pch=c(NA, 16), col=c(NA, "blue"), bty = "n")

legend("bottomright", legend = c("Curvas de nivel cada 500 m", "Transecto"), lty=c(1,2),
       col = c("black", "red"), bty = "n")

plot(so.ne$DISTANCIA_M, so.ne$ALTITUD, type = "l", xlab="", ylab="")
title("Perfil Topogr�fico", font=2)
mtext("B)", font = 2, line = -1.2, adj = 0.98)
title("SO-NE (3 a 4)", font = 2, line = -1, adj=0.2)
mtext("Altitud", side=2, line = 2.5, font = 2)


plot(ns$DISTANCIA_M, ns$ALTITUD, type = "l", xlab="", ylab="")
mtext("C)", font = 2, line = -1.2, adj = 0.98)
title("S-N (1 a 2)", font = 2, line = -1, adj=0.2)
mtext("Altitud", side=2, line = 2.5, font = 2)
mtext("Distancia (m)", side = 1, line = 2.5, font = 2)

dev.off()

# fin ---



# calculos ----

idx <- which(l@data$direccion=="SO_NE")
l.so.ne <- l[idx,]
plot(l.so.ne)
gLength(l.so.ne)

idx <- which(l@data$direccion=="S_N")
l.s.n <- l[idx,]
plot(l.s.n)
gLength(l.s.n)
