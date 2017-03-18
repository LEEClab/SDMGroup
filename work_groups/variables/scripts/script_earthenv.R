### script download of data bases for enm ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 08/02/2017

###------------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
memory.limit(size = 17500000000000) 

# install and require packages
# install.packages(c("downloader", "xml2", "rvest"), dep = T)

library(downloader)
library(xml2)
library(rvest)

###------------------------------------------------------------------------------###
### earthenv
###------------------------------------------------------------------------------###

# 1. Global Habitat Heterogeneity

# directory
setwd("D:/environmental_data/earthenv")

# list of url
url <- "http://www.earthenv.org/texture"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- link[grep(".tif", link)]
tif

names <- as.character(tif)
names

names.s <- sub("http://data.earthenv.org/habitat_heterogeneity/", "", names)
names.s

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
      download(paste0("https://cloud.s3it.uzh.ch:8080/v1/AUTH_5218a3a69ebf4a059c5a95889c5ee56e/CHELSA/", j, ".zip"), 
      paste0(j, ".zip"), mode = "wb")
      unzip(j)
      unlink(j)}}


###------------------------------------------------------------------------------###


# 2. Global 1-km Consensus Land Cover

# directory
setwd("")

# list of url
url <- "http://www.earthenv.org/landcover"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- link[grep(".tif", link)]
tif

names <- as.character(tif)
names

names.s <- sub("http://data.earthenv.org/habitat_heterogeneity/", "", names)
names.s

# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

###------------------------------------------------------------------------------###


# 3. Global 1-km Cloud Cover

# directory
setwd("")

# list of url
url <- "http://www.earthenv.org/cloud"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- link[grep(".tif", link)]
tif

names <- as.character(tif)
names

names.s <- sub("http://data.earthenv.org/cloud/", "", names)
names.s

# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "cloud")}


###------------------------------------------------------------------------------###

# 4. Near-global environmental information for freshwater ecosystems in 1km resolution

# directory
setwd("")

# list of url
url <- "http://www.earthenv.org/streams"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

nc <- link[grep("//data", link)]
nc

names <- as.character(tif)
names

names.s <- sub("http://data.earthenv.org/habitat_heterogeneity/", "", names)
names.s

# download
for(i in 1:length(zip)){
  download(zip[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "present")}


###------------------------------------------------------------------------------###
