setwd("C:/Users/Francisco/Documents/disco_duro_extraible/documento_pc_antiguo/Practica_profesional/coberturas/clasificacion_koppen/")

head(legend_to_r)

db <- legend_to_r[,c(1,2,3,4,5)]
db

db$V6 <- paste(db$V4, db$V5, sep = ", ")
db$V7 <- paste(db$V3, db$V6, sep=", ")
db

db2 <- db[,c(1,2,7)]
db2

db2$V7
db2$V7 <- gsub("   ", "", db2$V7)
db2$V7 <- gsub(" ", "", db2$V7)

db2$V7 <- gsub(",", ", ", db2$V7)

db2$V7 <- gsub("Tropical, rainforest, ", "Tropical, rainforest", db2$V7)
db2$V7 <- gsub("Tropical, monsoon, ", "Tropical, monsoon", db2$V7)
db2$V7 <- gsub("Tropical, savannah, ", "Tropical, savannah", db2$V7)
db2$V7 <- gsub("Polar, tundra, ", "Polar, tundra", db2$V7)
db2$V7 <- gsub("Polar, frost, ", "Polar, frost", db2$V7)

db2$V7 <- gsub("drysummer", "dry summer", db2$V7)
db2$V7 <- gsub("drywinter", "dry winter", db2$V7)
db2$V7 <- gsub("nodryseason", "no dry season", db2$V7)

db2$V7 <- gsub("hotsummer", "hot summer", db2$V7)
db2$V7 <- gsub("warmsummer", "warm summer", db2$V7)
db2$V7 <- gsub("coldsummer", "cold summer", db2$V7)
db2$V7 <- gsub("verycoldwinter", "very cold winter", db2$V7)

colnames(db2) <- c("id", "cod", "class")
db3 <- db2
db3

#write.csv(db3, "codificacion_koppen_para_join.csv", row.names = F)
