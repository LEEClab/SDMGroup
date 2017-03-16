### script ensemble frequency import data sequence ###

# Thadeu Sobral de Souza - thadeusobral@gmail.com 
# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# Cleber Chaves - cleberchaves@gmail.com 

###-----------------------------------------------------------------------------------------###

# 1. limpar a memoria e carregar os pacotes 
# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
memory.limit(size = 17500000000000) 

# instalar e carregar pacotes
# install.packages(c("raster", "rgdal", "vegan"), dep = T)

# carregar pacotes
library(raster) # manejo de arquivos sig 
library(rgdal) # manejo de arquivos sig
library(vegan) # diversas analises multivariadas

# verificar pacotes carregados
search()


###-----------------------------------------------------------------------------------------###

# import data
# directory
setwd("D:/github/SDMGroup/meets/meet004/_saidas_enm")

# enms
# list files
asc <- list.files(pattern = ".asc")
asc

enm <- raster("CCSM_Bioclim_0k_Aralia_warmingiana1.asc")
enm
plot(enm)

# evaluate
txt <- list.files(pattern = ".txt")
txt

eva <- lapply(txt, read.table)
eva
names(eva) <- txt
eva

###-----------------------------------------------------------------------------------------###

## frequency ensemble 
# lists
# species
sp <- list("Aralia_warmingiana", "Cavanillesia_umbellata")
sp

# gcms
gc <- list("CCSM")
gc

# periods
pe <- list("0k", "6k", "21k")
pe

# algorithms
al <- list("Bioclim", "Gower", "Maha", "Maxent", "SVM")
al

# replicates
re <- list(1:5)
re

# ensembles
ens.re <- enm
ens.re[] <- 0
names(ens.re) <- "ens.re"
ens.re

ens.al <- enm
ens.al[] <- 0
names(ens.al) <- "ens.al"
ens.al

# for
for(i in sp){		
  asc.sp <- grep(i, asc, value = T)
  eva.sp <- eva[grep(i, names(eva))]
  
    for(j in gc){		
      asc.gc <- grep(j, asc.sp, value = T)
      eva.gc <- eva.sp[grep(j, names(eva.sp))]
       
	  for(k in pe){		
          asc.pe <- grep(k, asc.gc, value = T)

            for(l in al){		
	        asc.al <- grep(l, asc.pe, value = T)
                eva.al <- eva.gc[grep(l, names(eva.gc))]
			           
	          for(m in re){		
                    enm.al <- stack(asc.al)
	            ens.re <- sum(ens.re, enm.al[[m]] >= eva.al[[1]][m, 1])}
		
		writeRaster(ens.re, paste0("ensemble_freq_", i, "_", j, "_", k, "_", l, ".asc"), 
				format = "ascii")

	     ens.al <- sum(ens.al, ens.re)
		  	
	     ens.re[] <- 0}

	   writeRaster(ens.al, paste0("ensemble_freq_", i, "_", j, "_", k, ".asc"), format = "ascii")
	   writeRaster(ens.al / (length(al) * max(re[[1]])), paste0("ensemble_freq_", i, "_", j, "_", k, "_bin.asc"), format = "ascii")
		
	   ens.al[] <- 0}}}

###-----------------------------------------------------------------------------------------###
