### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 20/03/2017

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
###                               earthenv
###-----------------------------------------------------------------------------###

# 1. Global Habitat Heterogeneity

# directory
setwd("D:/environmental_data/earthenv")
dir.create("heterogeneity")
setwd("heterogeneity")

# list of url
url <- "http://www.earthenv.org/texture"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- grep(".tif", link, value = T)
tif

# download
es <- c("/1km/", "/5km/", "/25km/")
es

di <- c("01km", "05km", "25km")
di

for(i in 1:length(es)){
  li <- grep(es[i], tif, value = T)
  na <- sub(paste0("http://data.earthenv.org/habitat_heterogeneity", es[i]), "", li)
 
  dir.create(di[i])
  setwd(di[i])

	  for(j in 1:length(li)){
  	  download(li[j], na[j], mode = "wb")}

  setwd("..")}

###-----------------------------------------------------------------------------###


# 2. Global 1-km Consensus Land Cover

# directory
setwd("..")
dir.create("land_cover")
setwd("land_cover")

# list of url
url <- "http://www.earthenv.org/landcover"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- grep(".tif", link, value = T)
tif

# download
di <- c("with_DISCover", "without_DISCover")
di

for(i in 1:length(di)){
  li <- grep(di[i], tif, value = T)
  na <- sub(paste0("http://data.earthenv.org/consensus_landcover/", di[i], "/"), "", li)

  dir.create(di[i])
  setwd(di[i])

	  for(j in 1:length(li)){
  	  download(li[j], na[j], mode = "wb")}

  setwd("..")}


###-----------------------------------------------------------------------------###


# 3. Global 1-km Cloud Cover

# directory
setwd("..")
dir.create("cloud_cover")
setwd("cloud_cover")

# list of url
url <- "http://www.earthenv.org/cloud"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

tif <- grep(".tif", link, value = T)
tif

na <- sub("http://data.earthenv.org/cloud/", "", tif)
na

# download
for(i in 1:length(tif)){
  download(tif[i], na[i], mode = "wb")}


###-----------------------------------------------------------------------------###

# 4. Near-global environmental information for freshwater ecosystems in 1km resolution

# directory
setwd("..")
dir.create("streams")
setwd("streams")

# list of url
url <- "http://www.earthenv.org/streams"
url

pg <- read_html(url)
pg

link <- as.list(html_attr(html_nodes(pg, "a"), "href"))
link

nc <- grep(".nc", link, value = T)
nc

na <- sub("http://data.earthenv.org/streams/", "", nc)
na

# download
for(i in 1:length(nc)){
  download(nc[i], na[i], mode = "wb")}

###-----------------------------------------------------------------------------###

# 5. EarthEnv-DEM90 digital elevation model

# directory
setwd("..")
dir.create("mde")
setwd("mde")
getwd()

# list of url
url <- "http://mirrors.iplantcollaborative.org/earthenv_dem_data/EarthEnv-DEM90/EarthEnv-DEM90_"
url

lat <- c(paste0("N", c("00", "05", seq(10, 80, 5))), 
         paste0("S", c("05", seq(10, 55, 5))))
lat

long <- c(paste0("W", c("005", paste0("0", seq(10, 95, 5)), seq(100, 180, 5))), 
          paste0("E", c("000", "005", paste0("0", seq(10, 95, 5)), seq(100, 175, 5))))
long

cells <- levels(interaction(lat, long, sep = ""))
cells

# download
for(i in cells){
  download(paste0(url, i, ".tar.gz"), paste0(i, ".tar.gz"), mode = "wb")
  untar(paste0(i, ".tar.gz"))
  unlink(paste0(i, ".tar.gz"))}


# check download and download errors
do <- sub(".prj", "", sub("EarthEnv-DEM90_", "", list.files(pattern = ".prj")))
do
length(do)

ch <- paste0(cells)
ch
length(ch)

# check
for(j in ch){
  if(j %in% do){}
  else{print(j)}}

# download
for(j in ch){
  if(j %in% do){}
  else{
    download(paste0(url, j, ".tar.gz"), paste0(j, ".tar.gz"), mode = "wb")
    untar(paste0(j, ".tar.gz"))
    unlink(paste0(j, ".tar.gz"))}}

###----------------------------------------------------------------------------###