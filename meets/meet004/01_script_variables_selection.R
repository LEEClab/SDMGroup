### script preparacao e selecao das variaveis ambientais ### 

# Thadeu Sobral de Souza - thadeusobral@gmail.com 
# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###-----------------------------------------------------------------------------------------###

# 1. clear the memory and load the packages
# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
memory.limit(size = 17500000000000) 

# install packages
# install.packages(c("raster", "rgdal", "corrplot", "RStoolbox", "vegan", "psych"), dep = T)

# load packages
library(raster) # manejo de arquivos sig 
library(rgdal) # manejo de arquivos sig
library(corrplot) # graficos de correlacao
library(RStoolbox) # pca de arquivos raster
library(vegan) # diversas analises multivariadas
library(psych) # analise fatorial

# check loaded packets
search()

###-----------------------------------------------------------------------------------------###

# 2. import data
# directory
setwd("D:/github/SDMGroup/meets/meet004/data")
getwd()

# listar o nome dos arquivos no diretorio com um padrao
asc <- list.files(patt = ".asc")
asc

# selecionar o nome dos arquivos especificos
pres <- grep("0k", asc, value = T)
pres

hol <- grep("6k", asc, value = T)
hol

lgm <- grep("21k", asc, value = T)
lgm

# carregar os arquivos .asc em uma variavel rasterstack e renomea-los
pres.s <- stack(pres)
pres.s

names(pres.s)
names(pres.s) <- c(paste0("pres_", "bio0", 1:9), paste0("pres_", "bio", 10:19))
names(pres.s)

plot(pres.s)
plot(pres.s[[1]])

# bioclim - descricao
# http://www.worldclim.org/bioclim

hol.s <- stack(hol)
hol.s
names(hol.s) <- c(paste0("hol_", "bio0", 1:9), paste0("hol_", "bio", 10:19))
plot(hol.s)

lgm.s <- stack(lgm)
lgm.s
names(lgm.s) <- c(paste0("lgm_", "bio0", 1:9), paste0("lgm_", "bio", 10:19))
plot(lgm.s)


###-----------------------------------------------------------------------------------------###

# 3. extrair os valores das celulas apenas para o presente
pres.s.v <- values(pres.s)
head(pres.s.v)
head(pres.s.v, 10)
head(pres.s.v, 50)

# contando o numero de linhas
nrow(pres.s.v)

# dimensao
dim(pres.s.v)

# omitir os NAs
pres.s.v.na <- na.omit(pres.s.v)
head(pres.s.v.na, 50)
dim(pres.s.v.na)

# perguntar se ha NAs
any(is.na(pres.s.v.na))

###-----------------------------------------------------------------------------------------###

# 4.correlacao

# criar pasta e definir diretorio para analise exploratoria - correlacao
setwd("..") # voltar uma pasta no diretorio

getwd() # conferir o diretorio

dir.create("analise_selecao_variaveis")  # criar uma pasta no diretorio

setwd("./analise_selecao_variaveis")  # mudar o diretorio para a pasta criada

dir.create("correlacao")  # criar pasta no diretorio da pasta criada

setwd("./correlacao") # mudar o diretorio para a pasta criada, da pasta criada

getwd() 

# tabela da correlacao
corr <- cor(pres.s.v.na)
corr
round(corr, 2) # arredondamento dos valores para dois valores decimais
abs(round(corr, 2)) # arredondamento e valor absoluto
ifelse(corr >= 0.7, "Sim", "Não") # sim ou nao
ifelse(corr >= 0.7, 1, 0) # 1 ou 0

# exportar tabela com a correlacao
write.table(abs(round(corr, 2)), "cor_pres.xls", row.names = T, sep = "\t")
write.table(ifelse(corr >= 0.7, "Sim", "Não"), "cor_pres_afirmacao.xls", row.names = T, 
		sep = "\t")

# plot da correlacao
corrplot(corr, type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
	   title = "Correlações entre variáveis Bioclimáticas")

# apenas azul
corrplot(abs(corr), type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
	   title = "Correlações entre variáveis Bioclimáticas")

# apenas vermelho
corrplot(-1 * (abs(corr)), type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
	   title = "Correlações entre variáveis Bioclimáticas")

# exportar figura na pasta do diretorio
tiff("cor_ma.tif", width = 18, height = 18, units = "cm", res = 300, compression = "lzw")

corrplot(corr, type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
	   title = "Correlações entre variáveis Bioclimáticas")

dev.off()

###-----------------------------------------------------------------------------------------###

# 5. pca

# criar pasta e definir diretorio para analise exploratoria - pca
setwd("..") # voltar uma pasta no diretorio
getwd() # conferir o diretorio
dir.create("pca") # criar pasta no diretorio
setwd("./pca") # mudar o diretorio para a pasta criada
getwd() # conferir o diretorio

# 5.1. pca para escolher variaveis
# pca do pacote "stats"
# pca com normalizacao interna
pca <- prcomp(pres.s.v.na, scale = T)
pca

# contribuicao de cada eixo (eigenvalues - autovalores)
summary(pca)

# grafico de barras com as contribuicoes
par(mar = c(3, 5, 5, 2))
screeplot(pca, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)

tiff("screeplot.tif", wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
par(mar = c(3, 5, 5, 2))
screeplot(pca, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)
dev.off()

# valores de cada eixo (eigenvectors - autovetores - escores)
pca$x
dim(pca$x)

# relacao das variaveis com cada eixo (loadings - cargas)
pca$rotation[, 1:5]
round(pca$rotation[, 1:5], 2)
abs(round(pca$rotation[, 1:5], 2))

# exportar tabela com a contribuicao
write.table(abs(round(pca$rotation[, 1:5], 2)), "contr_pca.xls", row.names = T, sep = "\t")

# plot
biplot(pca, var.axes = T, xlabs = rep("o", nrow(pca$x)), ylabs = paste0("bio", 1:19), cex = .8,
	 expand = 1.2, xlab = "PC1 (42.31%)", ylab = "PC2 (22.67%)", main = "PCA Bioclimáticas América do Sul", 
	 xlim = c(-.03, .04))



# 5.2. pca como novas variaveis
# pca dos raster
pca.r <- rasterPCA(pres.s, spca = T) 
pca.r

# contribuicao dos componentes
summary(pca.r$model)
summary(pca)

# grafico de barras com as contribuicoes
par(mar = c(3, 5, 5, 2))
screeplot(pca.r$model, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)

# comparacao
par(mfrow = c(1, 2))
screeplot(pca.r$model, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)

screeplot(pca, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)

tiff("screeplot_raster.tif", wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
par(mar = c(3, 5, 5, 2))
screeplot(pca.r$model, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)
dev.off()

# plot das pcs como novas variaveis
plot(pca.r$map)

plot(pca.r$map[[1:5]])


# exportar as novas variaveis
# exportar apenas uma variavel
writeRaster(pca.r$map[[1]], "pc1_e.asc", format = "ascii")
writeRaster(pca.r$map[[2]], "pc2_e.asc", format = "ascii")
writeRaster(pca.r$map[[3]], "pc3_e.asc", format = "ascii")
writeRaster(pca.r$map[[4]], "pc4_e.asc", format = "ascii")
writeRaster(pca.r$map[[5]], "pc5_e.asc", format = "ascii")

# comando for
print(1)
print(2)
print(3)
print(4)
print(5)

for(i in 1:5){
  print(i)}

for(i in 1:5000){
  print(i)}

# exportar as cinco variaveis
for(i in 1:5){
  writeRaster(pca.r$map[[i]], paste0("pc", i, "_e.asc"), format = "ascii", overwrite = T)}

###-----------------------------------------------------------------------------------------###

# 6. analise fatorial

# criar pasta e definir diretorio para analise exploratoria - fatorial
setwd("..") 
getwd() 
dir.create("fatorial") 
setwd("./fatorial") 
getwd() 

# analises preliminares de possibilidade de uso da analise fatorial
# kmo e bartlett
KMO(cor(pres.s.v.na)) # deve ser acima de 0.5
cortest.bartlett(cor(pres.s.v.na), n = nrow(pres.s.v.na)) # deve ser significativo (p < 0.05)

# numero de eixos - semelhante a pca
# screeplot
fa <- fa.parallel(pres.s.v.na, fa = "fa", fm = "ml") # sugere 5 eixos
fa

# exportar screeplot
tiff("screeplot_fatorial.tif", wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
fa.parallel(pres.s.v.na, fm = "ml", fa = "fa") 
dev.off()

# fatorial
fa.am <- fa(pres.s.v.na, nfactors = 5, rotate = "varimax", fm = "ml")
am.loadings <- loadings(fa.am)
am.loadings

abs(round(am.loadings, 2))

# exportar tabela dos resultados
write.table(abs(round(am.loadings, 2)), "as_loadings.xls", row.names = T, sep = "\t")

# bios escolhidas
# bio01, bio02, bio04, bio16, bio17

# significado das bios
# BIO1 = Temperatura media anual
# BIO2 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO3 = Isotermalidade (BIO2/BIO7) (* 100)
# BIO4 = Sazonalidade da temperatura (desvio padrao deviation *100)
# BIO5 = Temperatura maxima do mes mais quente
# BIO6 = Temperatura minima do mes mais frio
# BIO7 = Variacao da temperatura anual (BIO5-BIO6)
# BIO8 = Temperatura media do trimestre mais chuvoso
# BIO9 = Temperatura media do trimestre mais seco
# BIO10 = Temperatura media do trimestre mais quente
# BIO11 = Temperatura media do trimestre mais frio
# BIO12 = Precipitacao anual
# BIO13 = Precipitacao do mes mais chuvoso
# BIO14 = Precipitacao do mes mais seco
# BIO15 = Sazonalidade da precipitacao (coeficiente de variacao)
# BIO16 = Precipitacao do trimestre mais chuvoso
# BIO17 = Precipitacao do trimestre mais seco
# BIO18 = Precipitacao do trimestre mais quente
# BIO19 = Precipitacao do trimestre mais frio

###-----------------------------------------------------------------------------------------###

# 8. exportar as variaveis escolhidas
# bios escolhidas
# bio01, bio02, bio04, bio16, bio17

pres.s
names(pres.s)

lista <- c(01, 02, 04, 16, 17)

pres.s[[01]]

# diretorios de saida
setwd("..") 
getwd() 

setwd("..") 
getwd() 

dir.create("selection") 
setwd("./selection") 
getwd() 

# presente
for(i in lista){
  writeRaster(pres.s[[i]], ifelse(i < 10, paste0("CCSM_0k_am_bio0", i, ".asc"), 
		  paste0("CCSM_0k_am_bio", i, ".asc")), format = "ascii")}

# holoceno
for(i in lista){
  writeRaster(hol.s[[i]], ifelse(i < 10, paste0("CCSM_6k_am_bio0", i, ".asc"), 
		  paste0("CCSM_6k_am_bio", i, ".asc")), format = "ascii")}

# lgm
for(i in lista){
  writeRaster(lgm.s[[i]], ifelse(i < 10, paste0("CCSM_21k_am_bio0", i, ".asc"), 
		  paste0("CCSM_21k_am_bio", i, ".asc")), format = "ascii")}

###-----------------------------------------------------------------------------------------###


