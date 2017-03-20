### script download of data bases for enm ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 17/03/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
memory.limit(size = 17500000000000) 

# install and require packages
# install.packages(c("downloader", "xml2", "rvest"), dep = T)

library(downloader)
library(xml2)
library(rvest)

###-----------------------------------------------------------------------------###
### topodata
###-----------------------------------------------------------------------------###
# directory
setwd("D:/environmental_data/relief/topodata") # define directory to store the zips

# list of url
url <- "http://www.dsr.inpe.br/topodata/data/geotiff/"

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

topo <- grep(".zip", link, value = T)
topo

###-----------------------------------------------------------------------------###

# topodata
setwd("..")
dir.create("topodata")
setwd("./topodata")
getwd()

for(i in topo){
  download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", i), paste0(i), 
  mode = "wb")
  unzip(i)
  unlink(i)}

# check download and download errors
li <- sub(".tif", "", list.files())
li

to <- sub(".zip", "", topo)
to

for(j in to){
  if(j %in% li){}
      else{print(j)}}

for(j in to){
  if(j %in% li){}
    else{
      download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", j, ".zip"), 
      paste0(j, ".zip"), mode = "wb")
      unzip(j)

###-----------------------------------------------------------------------------###
      unlink(j)}}

























