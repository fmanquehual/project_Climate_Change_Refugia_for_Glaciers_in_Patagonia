library(ggplot2)
library(cowplot)

rm(list=ls())
dev.off()

# Zona sur ----

# Cargar RData!

m1.auc.dif <- mean(m1.auc.train) - mean(m1.auc.test)
m2.auc.dif <- mean(m2.auc.train) - mean(m2.auc.test)
m3.auc.dif <- mean(m3.auc.train) - mean(m3.auc.test)
m4.auc.dif <- mean(m4.auc.train) - mean(m4.auc.test)
m5.auc.dif <- mean(m5.auc.train) - mean(m5.auc.test)
m6.auc.dif <- mean(m6.auc.train) - mean(m6.auc.test)
m7.auc.dif <- mean(m7.auc.train) - mean(m7.auc.test)
m8.auc.dif <- mean(m8.auc.train) - mean(m8.auc.test)

auc.dif <- c(m1.auc.dif, m2.auc.dif, m3.auc.dif, m4.auc.dif, m5.auc.dif, m6.auc.dif, m7.auc.dif, m8.auc.dif)

auc.eval <- c( mean(m1.auc.test), mean(m2.auc.test), mean(m3.auc.test), mean(m4.auc.test),
               mean(m5.auc.test), mean(m6.auc.test), mean(m7.auc.test), mean(m8.auc.test))

coef.regu <- 1:8

db.sur <-data.frame(coefficients.regularizaation=coef.regu, auc.mean=auc.eval, 
                    dif.auc.calibration.evaluation=auc.dif, zone='south') 
db.sur

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_RCC/datos/')
# write.csv(db.sur, 'desempenho_modelos_zona_sur.csv', row.names = FALSE)

# fin ---




rm(list=ls())
dev.off()

# Zona austral ----

# Cargar RData!

m1.auc.dif <- mean(m1.auc.train) - mean(m1.auc.test)
m2.auc.dif <- mean(m2.auc.train) - mean(m2.auc.test)
m3.auc.dif <- mean(m3.auc.train) - mean(m3.auc.test)
m4.auc.dif <- mean(m4.auc.train) - mean(m4.auc.test)
m5.auc.dif <- mean(m5.auc.train) - mean(m5.auc.test)
m6.auc.dif <- mean(m6.auc.train) - mean(m6.auc.test)
m7.auc.dif <- mean(m7.auc.train) - mean(m7.auc.test)
m8.auc.dif <- mean(m8.auc.train) - mean(m8.auc.test)

auc.dif <- c(m1.auc.dif, m2.auc.dif, m3.auc.dif, m4.auc.dif, m5.auc.dif, m6.auc.dif, m7.auc.dif, m8.auc.dif)

auc.eval <- c( mean(m1.auc.test), mean(m2.auc.test), mean(m3.auc.test), mean(m4.auc.test),
               mean(m5.auc.test), mean(m6.auc.test), mean(m7.auc.test), mean(m8.auc.test))

coef.regu <- 1:8

db.austal <-data.frame(coefficients.regularizaation=coef.regu, auc.mean=auc.eval, 
                    dif.auc.calibration.evaluation=auc.dif, zone='austral') 
db.austal

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_RCC/datos/')
# write.csv(db.austal, 'desempenho_modelos_zona_austral.csv', row.names = FALSE)

# fin ---



# Plots ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_RCC/datos/')

db.sur <- read.csv('desempenho_modelos_zona_sur.csv')
db.austral <- read.csv('desempenho_modelos_zona_austral.csv')


p1.sur <- ggplot(db.sur) + 
  geom_line(aes(x = coefficients.regularizaation, y = auc.mean), col = 'red') + 
  geom_point(aes(x = coefficients.regularizaation, y = auc.mean), col = 'black') +
  labs(x = 'Model', y = 'AUC mean') +
  scale_x_discrete(limits = db.sur$coefficients.regularizaation) + 
  theme_bw()


p2.sur <- ggplot(db.sur) + 
  geom_line(aes(x = coefficients.regularizaation, y = dif.auc.calibration.evaluation), colour="#208BB0")+  # AZUL
  geom_point(aes(x = coefficients.regularizaation, y = dif.auc.calibration.evaluation)) +
  labs(x = 'Model', y = 'Calibration - Evaluation') +
  scale_x_discrete(limits = db.sur$coefficients.regularizaation) +
  theme_bw()


p1.austral <- ggplot(db.austral) + 
  geom_line(aes(x = coefficients.regularizaation, y = auc.mean), col = 'red') + 
  geom_point(aes(x = coefficients.regularizaation, y = auc.mean), col = 'black') +
  labs(x = 'Model', y = 'AUC mean') +
  scale_x_discrete(limits = db.sur$coefficients.regularizaation) + 
  theme_bw()


p2.austral <- ggplot(db.austral) + 
  geom_line(aes(x = coefficients.regularizaation, y = dif.auc.calibration.evaluation), colour="#208BB0")+  # AZUL
  geom_point(aes(x = coefficients.regularizaation, y = dif.auc.calibration.evaluation)) +
  labs(x = 'Model', y = 'Calibration - Evaluation') +
  scale_x_discrete(limits = db.sur$coefficients.regularizaation) +
  theme_bw()


setwd('C:/Users/Usuario/OneDrive/plots_paper/')
p0 <- plot_grid(p1.sur, p2.sur,
                p1.austral, p2.austral,
                labels="AUTO", ncol = 2, nrow = 2)
p0
# dev.off()
