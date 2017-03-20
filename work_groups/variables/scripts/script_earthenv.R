### script download of data bases for enm ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 20/03/2017

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
dir.create("heterogeneity")
setwd("./heterogeneity")

# list of url
url <- "http://www.earthenv.org/texture"
url

pg <- read_html(url)
pg

li <- as.list(html_attr(html_nodes(pg, "a"), "href"))
li

tif <- grep(".tif", li, value = T)
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
  setwd(paste0("./", di[i]))

	for(j in 1:length(li)){
  	  download(li[j], na[j], mode = "wb")}

  setwd("..")}

###------------------------------------------------------------------------------###


# 2. Global 1-km Consensus Land Cover

# directory
setwd("..")
dir.create("land_cover")
setwd("./land_cover")

# list of url
url <- "http://www.earthenv.org/landcover"
url

pg <- read_html(url)
pg

li <- as.list(html_attr(html_nodes(pg, "a"), "href"))
li

tif <- grep(".tif", li, value = T)
tif

# download
di <- c("with_DISCover", "without_DISCover")
di

for(i in 1:length(di)){
  li <- grep(di[i], tif, value = T)
  na <- sub(paste0("http://data.earthenv.org/consensus_landcover/", di[i], "/"), "", li)

  dir.create(di[i])
  setwd(paste0("./", di[i]))

	for(j in 1:length(li)){
  	  download(li[j], na[j], mode = "wb")}

  setwd("..")}


###------------------------------------------------------------------------------###


# 3. Global 1-km Cloud Cover

# directory
setwd("..")
dir.create("cloud")
setwd("./cloud")

# list of url
url <- "http://www.earthenv.org/cloud"
url

pg <- read_html(url)
pg

li <- as.list(html_attr(html_nodes(pg, "a"), "href"))
li

tif <- grep(".tif", li, value = T)
tif

na <- sub("http://data.earthenv.org/cloud/", "", tif)
na

# download
for(i in 1:length(tif)){
  download(tif[i], na[i], mode = "wb")}


###------------------------------------------------------------------------------###

# 4. Near-global environmental information for freshwater ecosystems in 1km resolution

# directory
setwd("..")
dir.create("streams")
setwd("./streams")

# list of url
url <- "http://www.earthenv.org/streams"
url

pg <- read_html(url)
pg

li <- as.list(html_attr(html_nodes(pg, "a"), "href"))
li

nc <- grep(".nc", li, value = T)
nc

na <- sub("http://data.earthenv.org/streams/", "", nc)
na

# download
for(i in 1:length(nc)){
  download(nc[i], na[i], mode = "wb")}

###------------------------------------------------------------------------------###
