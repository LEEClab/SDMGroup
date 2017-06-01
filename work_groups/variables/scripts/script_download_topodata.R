### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 17/03/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
memory.limit(size = 1.75e13) 

# install and require packages
# install.packages(c("downloader", "xml2", "rvest"), dep = T)

library(downloader)
library(xml2)
library(rvest)

###-----------------------------------------------------------------------------###
###                               topodata
###-----------------------------------------------------------------------------###

## all topodata
# directory
setwd("D:/environmental_data")
dir.create("topodata")
setwd("topodata")
getwd()

# list of url
url <- "http://www.dsr.inpe.br/topodata/data/geotiff/"

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

topo <- grep(".zip", link, value = T)
topo

# download
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

# check
for(j in to){
  if(j %in% li){}
      else{print(j)}}

# download
for(j in to){
  if(j %in% li){}
    else{
      download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", j, ".zip"), 
      paste0(j, ".zip"), mode = "wb")
      unzip(j)
      unlink(j)}}


###-----------------------------------------------------------------------------###

## altitude
# directory
setwd("..")
dir.create("altitude")
setwd("altitude")
getwd()

# download
alt <- grep("ZN", link, value = T)
alt


for(i in alt){
  download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", i), paste0(i), 
  mode = "wb")
  unzip(i)
  unlink(i)}

# check download and download errors
li <- sub(".tif", "", list.files())
li

al <- sub(".zip", "", alt)
al

for(j in al){
  if(j %in% li){}
      else{print(j)}}

for(j in al){
  if(j %in% li){}
    else{
      download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", j, ".zip"), 
      paste0(j, ".zip"), mode = "wb")
      unzip(j)
      unlink(j)}}
  

###-----------------------------------------------------------------------------###


## declividade
# directory
setwd("..")
dir.create("declividade")
setwd("declividade")
getwd()

# download
dec <- link[grep("SN", link)]
dec

for(i in dec){
  download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", i), paste0(i), 
  mode = "wb")
  unzip(i)
  unlink(i)}

# check download and download errors
li <- sub(".tif", "", list.files())
li

de <- sub(".zip", "", dec)
de

for(j in de){
  if(j %in% li){}
      else{print(j)}}

for(j in de){
  if(j %in% li){}
    else{
      download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", j, ".zip"), 
      paste0(j, ".zip"), mode = "wb")
      unzip(paste0(j, ".zip"))
      unlink(paste0(j, ".zip"))}}

###-----------------------------------------------------------------------------###

## aspecto

# directory
setwd("..")
dir.create("aspecto")
setwd("aspecto")
getwd()

# download
asp <- link[grep("ON", link)]
asp

for(i in asp){
  download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", i), paste0(i), 
  mode = "wb")
  unzip(i)
  unlink(i)}

# check download and download errors
li <- sub(".tif", "", list.files())
li

as <- sub(".zip", "", asp)
as

for(j in as){
  if(j %in% li){}
      else{print(j)}}

for(j in as){
  if(j %in% li){}
    else{
      download(paste0("http://www.dsr.inpe.br/topodata/data/geotiff/", j, ".zip"), 
      paste0(j, ".zip"), mode = "wb")
      unzip(paste0(j, ".zip"))
      unlink(paste0(j, ".zip"))}}

###-----------------------------------------------------------------------------###

