library(stringr)
library(ggplot2)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/predicciones_maxent/datos/')

# sur ----
db.ref.sur <- read.csv('resultados_modelo_referencia_sur.csv', sep = ';')
names(db.ref.sur)

idx <- which(str_detect(names(db.ref.sur), 'gain.without'))
db.ref.sur.gain.without <- db.ref.sur[,idx]
names(db.ref.sur.gain.without)

idy <- which(str_detect(names(db.ref.sur), 'gain.with.only'))
db.ref.sur.gain.with <- db.ref.sur[,idy]
names(db.ref.sur.gain.with)


#rowNames <- row.names(t(db.ref.sur2))
rowNames <- c('Altitude', 'Aspect', 'Slope', 
              'Precipitation in winter', 
              'Slope precipitation\nin winter',
              'Slope maximum\ntemperature in summer', 
              'Slope minimum\ntemperature in winter', 
              'Maximum temperature\nin summer', 
              'Minimum temperature\nin winter')

values.x <- as.vector(t(db.ref.sur.gain.without))
values.y <- as.vector(t(db.ref.sur.gain.with))

db.ref.sur.gain.without2 <- data.frame(type='All variables except the variable named in y-axis', Variable=rowNames, values=values.x)
db.ref.sur.gain.without2[order(db.ref.sur.gain.without2$values),]

db.ref.sur.gain.with2 <- data.frame(type='Only variable named in y-axis', Variable=rowNames, values=values.y)
db.ref.sur.gain.with2

db.ref.sur2 <- rbind(db.ref.sur.gain.with2, db.ref.sur.gain.without2)
db.ref.sur2$zona <- 'South'

# fin ---



# austral ----
db.ref.austral <- read.csv('resultados_modelo_referencia_austral.csv', sep = ';')
names(db.ref.austral)

idx <- which(str_detect(names(db.ref.austral), 'gain.without'))
db.ref.austral.gain.without <- db.ref.austral[,idx]
names(db.ref.austral.gain.without)

idy <- which(str_detect(names(db.ref.austral), 'gain.with.only'))
db.ref.austral.gain.with <- db.ref.austral[,idy]
names(db.ref.austral.gain.with)

rowNames <- c('Altitude', 'Aspect', 'Slope', 
              'Precipitation in winter', 
              'Slope precipitation\nin winter',
              'Slope maximum\ntemperature in summer', 
              'Slope minimum\ntemperature in winter', 
              'Maximum temperature\nin summer', 
              'Minimum temperature\nin winter')

values.x <- as.vector(t(db.ref.austral.gain.without))
values.y <- as.vector(t(db.ref.austral.gain.with))

db.ref.austral.gain.without2 <- data.frame(type='All variables except the variable named in y-axis', Variable=rowNames, values=values.x)
db.ref.austral.gain.without2[order(db.ref.austral.gain.without2$values),]

db.ref.austral.gain.with2 <- data.frame(type='Only variable named in y-axis', Variable=rowNames, values=values.y)
db.ref.austral.gain.with2

db.ref.austral2 <- rbind(db.ref.austral.gain.with2, db.ref.austral.gain.without2)
db.ref.austral2$zona <- 'Austral'
# fin ---

x.i <- as.vector(unlist(db.ref.sur['Regularized.training.gain']))
y.i <- as.vector(unlist(db.ref.austral['Regularized.training.gain']))

all <- data.frame(type = c('All variables', 'All variables'), Variable = c('', ''),
                       values = c(x.i, y.i), zona = c('South', 'Austral') ) 
dbf <- rbind(db.ref.sur2, db.ref.austral2, all)
dbf$zona <- factor(dbf$zona, levels = c('South', 'Austral'))
dbf$Variable <- factor(dbf$Variable, levels = c('', 'Slope', 'Aspect', 'Altitude',
                                                'Slope minimum\ntemperature in winter', 
                                                'Slope maximum\ntemperature in summer', 
                                                'Slope precipitation\nin winter',
                                                'Minimum temperature\nin winter', 
                                                'Maximum temperature\nin summer', 
                                                'Precipitation in winter'))

# plot ---
p.out <- ggplot(dbf, aes(x = Variable, y = values)) +
  geom_col(aes(fill = type), width = 0.5, position = position_dodge(0.2, preserve = 'single')) +
  labs(y = 'Regularized training gain', x = 'Environmental variables') +
  scale_fill_manual('Model created with:', values = c("#C0C1C2", "#18406A", '#C5421F'), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(text = element_text(size=18), legend.position = 'bottom', legend.justification = "left",
        panel.spacing = unit(1, "lines")) + 
  guides(fill=guide_legend(ncol=2, title.position = "top")) +
  facet_wrap(~zona, scales = "free_x", dir = 'h') +
  coord_flip() 

p.out

setwd('C:/Users/Usuario/OneDrive/plots_paper/')
jpeg('jackknife_modelo_sur_austral.jpg', width = 1850, height = 1400, units = "px", pointsize = 12,
     quality = 100, type = 'cairo', res = 200)

p.out

dev.off()

# setwd('C:/Users/Usuario/OneDrive/plots_paper/')
# setEPS()
# postscript(file = "jackknife_modelo_sur_austral.eps", height = 10, width = 14)  # Una figura en cm
# par(mar=c(4,4,0,0)+0.1)
# p.out
# dev.off()

