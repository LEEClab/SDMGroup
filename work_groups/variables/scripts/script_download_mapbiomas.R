### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 28/04/2017

###-----------------------------------------------------------------------------###
###                             mapbiomas v02 ###
###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
memory.limit(size = 1.75e13) 

# install and load packages
# install.packages("downloader", dep = T)

library(downloader)

###-----------------------------------------------------------------------------###
# directory
setwd("D:/environmental_data/mapbiomas/colecao_02")

# url
url <- "http://maps.lapig.iesa.ufg.br/mapbiomas/"

# download
an <- 2000:2016
an

bi <-  c("AMAZONIA", "PANTANAL", "CAATINGA", "MATAATLANTICA", 
         "CERRADO", "PAMPA")
bi

for(i in an){
  url.an <- paste0(url, i, "/")
  
    for(j in bi){
      url.bi <- paste0(url.an, j, ".tif")
      download(url.bi, paste0(j, "_", i, ".tif"), mode = "wb")}}


###-----------------------------------------------------------------------###
