### script download of data bases for enm ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 08/02/2017

###-------------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
memory.limit(size = 17500000000000) 

# install and require packages
# install.packages(c("downloader", "xml2", "rvest"), dep = T)

library(downloader)
library(xml2)
library(rvest)

###-------------------------------------------------------------------------------###
### chelsa
###-------------------------------------------------------------------------------###

# directory
setwd("D:/environmental_data/climate/chelsa")

# list of url
url <- "https://cloud.s3it.uzh.ch:8080/v1/AUTH_5218a3a69ebf4a059c5a95889c5ee56e/CHELSA/"
url

pg <- read_html(url)
pg

list <- as.list(html_attr(html_nodes(pg, "a"), "href"))
list

# download
for(i in list){
  download(paste0("https://cloud.s3it.uzh.ch:8080/v1/AUTH_5218a3a69ebf4a059c5a95889c5ee56e/CHELSA/", i), 
  paste0(i), mode = "wb")
  unzip(i)
  unlink(i)}


# check download and download errors
li <- sub(".tif", "", list.files())
li

ch <- sub(".zip", "", list)
ch

for(j in ch){
  if(j %in% li){}
      else{print(j)}}

for(j in ch){
  if(j %in% li){}
    else{
      #download(paste0("https://cloud.s3it.uzh.ch:8080/v1/AUTH_5218a3a69ebf4a059c5a95889c5ee56e/CHELSA/", j, ".zip"), 
      #paste0(j, ".zip"), mode = "wb")
      unzip(paste0(j, ".zip"))
      unlink(paste0(j, ".zip"))}}


###-------------------------------------------------------------------------------###