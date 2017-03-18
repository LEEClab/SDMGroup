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
### bioclim
###------------------------------------------------------------------------------###


### 1. current ###

# directory
setwd("D:/environmental_data/bioclim") # define directory to store the zips

# list of url
url <- "http://www.worldclim.org/current"

pg <- read_html(url)
pg

link <- html_attr(html_nodes(pg, "a"), "href")
link

zip <- link[grep("bil.zip", link)]
zip

names <- as.character(zip)
names

names.s <- sub("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/", "", names)
names.s

# directory
dir.create("current")
setwd("./current") # define directory to store the zips

# download
for(i in names.s){
  dir.create(sub(".zip", "", i))
  setwd(paste0("./", sub(".zip", "", i)))

  download(paste0("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/", i), 
  paste0(i), mode = "wb")
  unzip(i)
  unlink(i)

  setwd("..")}


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
      download(paste0("https:/cloud.s3it.uzh.ch:8080/v1/AUTH_5218a3a69ebf4a059c5a95889c5ee56e/CHELSA/", j, ".zip"), 
      paste0(j, ".zip"), mode = "wb")
      unzip(paste0(j, ".zip"))
      unlink(paste0(j, ".zip"))}}


###------------------------------------------------------------------------------###

### 2. past ###

# list of url
url <- "http://www.worldclim.org/paleo-climate1"

pg <- read_html(url)
pg

link <- html_attr(html_nodes(pg, "a"), "href")
link

zip <- link[grep(".zip", link)]
zip


# 2.1 mid
# directory
setwd("")

mid <- link[grep("mid", link)]
mid

names <- as.character(mid)
names

names.s <- sub("http://biogeo.ucdavis.edu/data/climate/cmip5/mid/", "", names)
names.s

# download
for(i in 1:length(mid)){
  download(mid[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "past_mid")}

###------------------------------------------------------------------------------###

# 2.2 lgm
# directory
setwd("") 

lgm <- link[grep("lgm", link)]
lgm

names <- as.character(lgm)
names

names.s <- sub("http:/biogeo.ucdavis.edu/data/climate/cmip5/lgm/", "", names)
names.s

# download
for(i in 1:length(lgm)){
  download(lgm[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "past_lgm")}

###------------------------------------------------------------------------------###

# 2.3 lig
# directory
setwd("")

lig <- link[grep("lig", link)]
lig

names <- as.character(lig)
names

names.s <- sub("http:/biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/pst/lig/", "", names)
names.s

# download
for(i in 1:length(lig)){
  download(lig[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = ".zip")
list

for(i in list){
  unzip(i, exdir = "past_lig")}


###------------------------------------------------------------------------------###


### 3. future ###

# 3.1 10 m

# list of url
url <- "http:/www.worldclim.org/cmip5_10m"

pg <- read_html(url)
pg

link <- html_attr(html_nodes(pg, "a"), "href")
link

zip <- link[grep(".zip", link)]
zip


# 3.1.1 rcp26
# directory
setwd("")

rcp26 <- link[grep("26", link)]
rcp26

names <- as.character(rcp26)
names

names.s <- sub("http:/biogeo.ucdavis.edu/data/climate/cmip5/10m/", "", names)
names.s

# download
for(i in 1:length(rcp26)){
  download(rcp26[i], names.s[i], mode = "wb")} 

# unzip the archives
list <- list.files(patt = "50.zip")
list

for(i in list){
  unzip(i, exdir = "10m_2050_rcp26")}

list <- list.files(patt = "70.zip")
list

for(i in list){
  unzip(i, exdir = "10m_2070_rcp26")}

###------------------------------------------------------------------------------###
