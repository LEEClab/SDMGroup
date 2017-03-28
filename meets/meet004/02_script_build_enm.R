### script build enm ###

# Thadeu Sobral de Souza - thadeusobral@gmail.com 
# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###-----------------------------------------------------------------------------------------###
### script build enm ### 
###-----------------------------------------------------------------------------------------###

# 1. limpara a memoria e carregar os pacotes 
# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
memory.limit(size = 17500000000000) 

# instalar e carregar pacotes
# install.packages(c("raster", "rgdal", "dismo", "gam", "randomForest", "kernlab", "rJava", 
# 		   	   "vegan"), dep = T)

# carregar pacotes
library(raster) # manejo de arquivos sig 
library(rgdal) # manejo de arquivos sig
library(dismo) # construir enms
library(gam) # construir enms
library(randomForest) # construir enms
library(kernlab) # algoritmo svm
library(rJava) # funcionamento do java
library(vegan) # diversas analises multivariadas

# verificar pacotes carregados
search()

###-----------------------------------------------------------------------------------------###

# 2. dados de entrada
# diretorio da pasta de dados de entrada
setwd("D:/github/SDMGroup/meets/meet004/occurrences")

# importando os pontos de ocorrencia 
points <- read.table("pablo_plants.txt", h = T)
head(points, 10)

am <- shapefile("D:/github/SDMGroup/meets/meet004/south/south_america_diss_gcs_wgs84.shp")
am

plot(am, col = "gray80", axes = T, xlim = c(-60, -60), main = "Mapa Pablo Plants")
points(points$Long, points$Lat, col = points$id, axes = T, pch = 20)

# diretorio da pasta de dados de entrada
setwd("D:/github/SDMGroup/meets/meet004/selection")

# importando as variaveis .asc
asc <- list.files(patt = "asc")
asc

asc.0k <- grep("0k", asc, value = T)
asc.0k

asc.6k <- grep("6k", asc, value = T)
asc.6k

asc.21k <- grep("21k", asc, value = T)
asc.21k

env.stack.0k <- stack(asc.0k)
names(env.stack.0k) <- paste0("bio", c("01", "02", "03", "16", "17"))
env.stack.0k

env.stack.6k <- stack(asc.6k)
names(env.stack.6k) <- paste0("bio", c("01", "02", "03", "16", "17"))
env.stack.6k

env.stack.21k <- stack(asc.21k)
names(env.stack.21k) <- paste0("bio", c("01", "02", "03", "16", "17"))
env.stack.21k

plot(env.stack.0k)
plot(env.stack.6k)
plot(env.stack.21k)

plot(env.stack.0k[[1]])
points(points[, 2], points[, 3], pch = 20)


# extraindo os valores de cada celula e adicionando as coordenadas
values <- values(env.stack.0k)[, 1]
head(values, 10)

id <- 1:ncell(env.stack.0k)
head(id, 10)

coord <- xyFromCell(env.stack.0k, id)
head(coord, 10)

plot(env.stack.0k[[1]])
points(coord, pch = "o", cex = 0.1)

coords <- data.frame(coord, values)
coords

coords <- na.omit(coords)
coords

coords <- coords[, -3]
head(coords)

colnames(coords) <- c("long", "lat")
head(coords, 10)

plot(env.stack.0k[[1]])
points(coords, pch = "o", cex = 0.1)

###-----------------------------------------------------------------------------------------###

# verificar maxent
jar <- paste(system.file(package = "dismo"), "/java/maxent.jar", sep = "")
file.exists(jar)

###-----------------------------------------------------------------------------------------###


# ENMs

# diretorio de saida dos enms
setwd("..")
getwd()
dir.create("_saidas_enm_2")
setwd("./_saidas_enm_2")
getwd()

# aogcms
AOGCM <- "CCSM"
AOGCM

# enms
for(i in 1:length(levels(points[, 1]))){ # for para cada especie

eval.Bioclim <- NULL # somente presenca - envelope
eval.Gower <- NULL # somente presenca - distancia
eval.Maha <- NULL # somente presenca - distancia
eval.Maxent <- NULL # presenca/pseudo-ausencia
eval.SVM <- NULL # presenca/pseudo-ausencia

eval.names <- NULL

# selecionando presença e ausencia da especie
	id.specie <- levels(points[, 1])[i]
	pr.specie <- points[which(points[, 1] == id.specie), 2:3]
	id.background <- sample(nrow(coords), nrow(pr.specie))
	bc.specie <- coords[id.background, ]
	

for(r in 1:5){	# numero de replicas
## preparando os modelos
# data treino e teste	
	pr.sample.train <- sample(nrow(pr.specie), round(0.75 * nrow(pr.specie)))
	bc.sample.train <- sample(nrow(bc.specie), round(0.75 * nrow(bc.specie)))
	test <- na.omit(prepareData(x = env.stack.0k, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
	train <- na.omit(prepareData(x = env.stack.0k, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))

 
####### ALGORITMOS
## Bioclim	
	Bioclim <- bioclim(train[which(train[, 1] == 1), -1])	
	
 writeRaster(predict(env.stack.0k, Bioclim), paste(AOGCM, "_Bioclim_0k_", id.specie, r, ".asc", sep = ""), format = "ascii")	
 writeRaster(predict(env.stack.6k, Bioclim), paste(AOGCM, "_Bioclim_6k_", id.specie, r, ".asc", sep = ""), format = "ascii")
 writeRaster(predict(env.stack.21k, Bioclim), paste(AOGCM, "_Bioclim_21k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 
	eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
	idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
	eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim]+eBioclim@TNR[idBioclim]-1))
	eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)

## Gower	
	Gower <- domain(train[which(train[, 1] == 1), -1])	

 writeRaster(predict(env.stack.0k, Gower), paste(AOGCM, "_Gower_0k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 writeRaster(predict(env.stack.6k, Gower), paste(AOGCM, "_Gower_6k_", id.specie, r, ".asc", sep = ""), format = "ascii")
 writeRaster(predict(env.stack.21k, Gower), paste(AOGCM, "_Gower_21k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 
	eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
	idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
	eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower]+eGower@TNR[idGower]-1))
	eval.Gower <- rbind(eval.Gower, eval.Gower.sp)

## Maha	
	Maha <- mahal(train[which(train[, 1] == 1), -1])	
	
 writeRaster(predict(env.stack.0k, Maha), paste(AOGCM, "_Maha_0k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 writeRaster(predict(env.stack.6k, Maha), paste(AOGCM, "_Maha_6k_", id.specie, r, ".asc", sep = ""), format = "ascii")
 writeRaster(predict(env.stack.21k, Maha), paste(AOGCM, "_Maha_21k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 
	eMaha <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maha)
	idMaha <- which(eMaha@t == as.numeric(threshold(eMaha, "spec_sens")))
	eval.Maha.sp <- c(eMaha@t[idMaha], eMaha@auc, (eMaha@TPR[idMaha]+eMaha@TNR[idMaha]-1))
	eval.Maha <- rbind(eval.Maha, eval.Maha.sp)
	

## Maxent	
	Maxent <- maxent(train[, -1], train[, 1])	

 writeRaster(predict(env.stack.0k, Maxent), paste(AOGCM, "_Maxent_0k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 writeRaster(predict(env.stack.6k, Maxent), paste(AOGCM, "_Maxent_6k_", id.specie, r, ".asc", sep = ""), format = "ascii")
 writeRaster(predict(env.stack.21k, Maxent), paste(AOGCM, "_Maxent_21k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 
	eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
	idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
	eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent]+eMaxent@TNR[idMaxent]-1))
	eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)


## SVM	
	SVM <- ksvm(pb ~ bio01 + bio02 + bio03 + bio16 + bio17, data = train)	

 writeRaster(predict(env.stack.0k, SVM), paste(AOGCM, "_SVM_0k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 writeRaster(predict(env.stack.6k, SVM), paste(AOGCM, "_SVM_6k_", id.specie, r, ".asc", sep = ""), format = "ascii")
 writeRaster(predict(env.stack.21k, SVM), paste(AOGCM, "_SVM_21k_", id.specie, r, ".asc", sep = ""), format = "ascii") 
 
	eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
	idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
	eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM]+eSVM@TNR[idSVM]-1))
	eval.SVM <- rbind(eval.SVM, eval.SVM.sp)


	eval.names <- c(eval.names, paste(id.specie, r, sep = ""))		
} # ends for"r"

dimnames(eval.Bioclim) <- list(eval.names, c("thrs", "AUC", "TSS"))
dimnames(eval.Gower) <- list(eval.names, c("thrs", "AUC", "TSS"))
dimnames(eval.Maha) <- list(eval.names, c("thrs", "AUC", "TSS"))
dimnames(eval.Maxent) <- list(eval.names, c("thrs", "AUC", "TSS"))
dimnames(eval.SVM) <- list(eval.names, c("thrs", "AUC", "TSS"))


write.table(eval.Bioclim, paste("zEval_", AOGCM, "_Bioclim_", id.specie, ".txt", sep = ""))
write.table(eval.Gower, paste("zEval_", AOGCM, "_Gower_", id.specie, ".txt", sep = ""))
write.table(eval.Maha, paste("zEval_", AOGCM, "_Maha_", id.specie, ".txt", sep = ""))
write.table(eval.Maxent, paste("zEval_", AOGCM, "_Maxent_", id.specie, ".txt", sep = ""))
write.table(eval.SVM, paste("zEval_", AOGCM, "_SVM_", id.specie, ".txt", sep = ""))

} # ends for"i"

###-----------------------------------------------------------------------------------------###


# plotando as predicoes

# divide o plot em 1 linha e 5 colunas
par(mfrow = c(2, 3))

# bioclim
m1 <- raster ("CCSM_Bioclim_0k_B.balansae1.asc")
plot (m1, main = "bioclim")

# gower
m2 <- raster ("CCSM_Gower_0k_B.balansae1.asc")
plot(m2, main = "gower")

# mahanalobis
m3 <- raster ("CCSM_Maha_0k_B.balansae1.asc")
plot(m3, main = "mahanalobis")

# maxent
m4 <- raster("CCSM_Maxent_0k_B.balansae1.asc")
plot(m4, main = "maxent")

# svm
m5 <-raster ("CCSM_SVM_0k_B.balansae1.asc")
plot(m5, main = "svm")

