### script baixar srtm v004 ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 29/05/2017

###------------------------------------------------------------------------------###

# limpar e aumentar a memoria
rm(list = ls())
memory.limit(size = 1.75e13)

# pacotes
# install.packages("downloader", dep = T)

library(downloader)

# diretorio
setwd("D:/environmental_data/srtm/v004")

###------------------------------------------------------------------------------###

# url
url <- "http://srtm.csi.cgiar.org/SRT-ZIP/SRTM_V41/SRTM_Data_GeoTiff/srtm_"
url

lat <- c(paste0("0", 1:9), 10:24)
lat

long <- c(paste0("0", 1:9), 10:72)
long

cells <- levels(interaction(long, lat, sep = "_"))
cells

# download
for(i in cells){
  tryCatch({
    download(paste0(url, i, ".zip"), paste0("srtm_", i, ".zip"), mode = "wb")
    unzip(paste0("srtm_", i, ".zip"))
    unlink(paste0("srtm_", i, ".zip")) 
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

###------------------------------------------------------------------------------###

