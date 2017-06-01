### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 08/02/2017

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
###                                 chelsa
###-----------------------------------------------------------------------------###

## v 1.0
# directory
setwd("D:/environmental_data/chelsa")
dir.create("v1_0")
setwd("v1_0")
getwd()

# list of url
url <- "http://www.wsl.ch/lud/chelsa/archive/version1.0/"
url

pg <- read_html(url)
pg

list <- grep("CHELSA", as.list(html_attr(html_nodes(pg, "a"), "href")), value = T)
list

# download
for(i in list){
  download(paste0("http://www.wsl.ch/lud/chelsa/archive/version1.0/", i), 
           paste0(i), mode = "wb")}


###------------------------------------------------------------------------------###


## v 1.1
# directory
setwd("..")
dir.create("v1_1")
setwd("v1_1")
getwd()

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


###------------------------------------------------------------------------------###

### v1.2
# directory
setwd("D:/environmental_data/chelsa/v1_2")

### bioclim float
# list of url
url <- "http://www.wsl.ch/lud/chelsa/data/bioclim/float/"
url

pg <- read_html(url)
pg

list <- grep("bio_", as.list(html_attr(html_nodes(pg, "a"), "href")), value = T)
list

# diretorio
dir.create("bioclim")
setwd("bioclim")
dir.create("float")
setwd("float")
getwd()

# download
for(i in list){
  download(paste0(url, i), paste0(i), mode = "wb")
  unzip(i)
  unlink(i)}


### bioclim integer
# list of url
url <- "http://www.wsl.ch/lud/chelsa/data/bioclim/integer/"
url

pg <- read_html(url)
pg

list <- grep("bio_", as.list(html_attr(html_nodes(pg, "a"), "href")), value = T)
list

# diretorio
setwd("..")
dir.create("integer")
setwd("integer")
getwd()

# download
for(i in list){
  download(paste0(url, i), paste0(i), mode = "wb")
  unzip(i)
  unlink(i)}


### precipitation
# list of url
url <- "http://www.wsl.ch/lud/chelsa/data/climatologies/prec/"
url

pg <- read_html(url)
pg

list <- as.list(html_attr(html_nodes(pg, "a"), "href"))
list

# diretorio
setwd("..")
setwd("..")
dir.create("prec")
setwd("prec")
getwd()

# download
for(i in list){
  download(paste0(url, i), paste0(i), mode = "wb")
  unzip(i)
  unlink(i)}

###-----------------------------------------------------------------------------###

