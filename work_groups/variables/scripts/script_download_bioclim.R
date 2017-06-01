### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 21/03/2017

###------------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
memory.limit(size = 1.75e13)

# install and require packages
# install.packages(c("downloader", "xml2", "rvest"), dep = T)

library(downloader)
library(xml2)
library(rvest)

###------------------------------------------------------------------------------###
###                                 bioclim
###------------------------------------------------------------------------------###

### bioclim v01

### 1. current ###

# directory
setwd("D:/environmental_data/bioclim")
 
dir.create("bioclim_v01")
setwd("bioclim_v01")

dir.create("current")
setwd("current") 

# list of url
url <- "http://www.worldclim.org/current"

pg <- read_html(url)
pg

li <- html_attr(html_nodes(pg, "a"), "href")
li

zip <- grep(".zip", li, value = T)
zip

# download
fo <- c("bil", "esri")
fo

re <- paste0("_", c("10m", "5m", "2-5m", "30s"), "_")
re

di <- c("10m", "5m", "2_5m", "30s")
di

for(i in fo){
  dir.create(i)
  setwd(i)
  zip.fo <- grep(i, zip, value = T)

	for(j in 1:length(re)){
  	  dir.create(di[j])
  	  setwd(di[j])

	    li <- grep(re[j], zip.fo, value = T)
  	  na <- sub("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/", "", li)
	    zip.re <- grep(re[j], zip.fo, value = T)

    	  for(k in 1:length(na)){
     	    dir.create(sub(".zip", "", na[k]))
  		    setwd(sub(".zip", "", na[k]))

  		    download(zip.re[k], na[k], mode = "wb")
  		    unzip(na[k])
  		    unlink(na[k])
		
		      setwd("..")}
  
  	 setwd("..")}
  
  setwd("..")}


###------------------------------------------------------------------------------###

### 2. past ###

# directory
setwd("..")
dir.create("past")
setwd("past")
getwd()

# list of url
url <- "http://www.worldclim.org/paleo-climate1"

pg <- read_html(url)
pg

link <- html_attr(html_nodes(pg, "a"), "href")
link

zip <- grep(".zip", link, value = T)
zip


# 2.1 mid
# diretorio
dir.create("mid")
setwd("mid")

# download
zip.mid <- grep("mid", zip, value = T)
zip.mid

re <- paste0("_", c("10m", "5m", "2-5m", "30s"))
re

di <- c("10m", "5m", "2_5m", "30s")
di

for(i in 1:length(re)){
  dir.create(di[i])
  setwd(di[i])

  zip.re <- grep(re[i], zip.mid, value = T)
  na <- sub("http://biogeo.ucdavis.edu/data/climate/cmip5/mid/", "", zip.re)

  gcm <- unique(substr(na, 1, 5))
 
    for(j in gcm){
	    dir.create(j)
  	  setwd(j)
	    zip.gcm <- grep(j, zip.re, value = T)
	    na.gcm <- grep(j, na, value = T)

	      for(k in 1:length(na.gcm)){
	        dir.create(sub(".zip", "", na.gcm[k]))
	        setwd(sub(".zip", "", na.gcm[k]))

	        download(zip.gcm[k], na.gcm[k], mode = "wb")
	        unzip(na.gcm[k])
	        unlink(na.gcm[k])
	    
	        setwd("..")}
  
	    setwd("..")}
  
  setwd("..")}


# 2.2 lgm
# diretorio
setwd("..")
dir.create("lgm")
setwd("lgm")

# download
zip.lgm <- grep("lgm", zip, value = T)
zip.lgm

re <- paste0("_", c("10m", "5m", "2-5m"))
re

di <- c("10m", "5m", "2_5m")
di

for(i in 1:length(re)){
  dir.create(di[i])
  setwd(di[i])

  zip.re <- grep(re[i], zip.lgm, value = T)
  na <- sub("http://biogeo.ucdavis.edu/data/climate/cmip5/lgm/", "", zip.re)

  gcm <- unique(substr(na, 1, 5))
    
    for(j in gcm){
	    dir.create(j)
  	  setwd(j)
	    zip.gcm <- grep(j, zip.re, value = T)
	    na.gcm <- grep(j, na, value = T)

	      for(k in 1:length(na.gcm)){
	        dir.create(sub(".zip", "", na.gcm[k]))
	        setwd(sub(".zip", "", na.gcm[k]))

	        download(zip.gcm[k], na.gcm[k], mode = "wb")
	        unzip(na.gcm[k])
	        unlink(na.gcm[k])
	    
	        setwd("..")}
  
	    setwd("..")}
  
  setwd("..")}



# 2.3 lig
# directory
setwd("..")
dir.create("lig")
setwd("lig")
getwd()

# download
lig <- grep("lig", zip, value = T)
lig

na <- sub("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/pst/lig/lig_30s_", "", lig)
na

for(i in 1:length(na)){
  dir.create(sub(".zip", "", na[i]))
  setwd(sub(".zip", "", na[i]))

  download(lig[i], na[i], mode = "wb")
  unzip(na[i])
  unlink(na[i])

  setwd("..")}

###------------------------------------------------------------------------------###


### 3. future ###

# directory
setwd("..") 
setwd("..") 
dir.create("future")
setwd("future") 
getwd()

# list of url
url.f <- "http://www.worldclim.org/cmip5_"
url.f

# download
re <- c("10m", "5m", "2.5m", "30s")
di <- c("10m", "5m", "2_5m", "30s")

an <- c("2050", "2070")
an.s <- c("50", "70")

em <- c("rcp26", "rcp45", "rcp60", "rcp85")
em.s <- c("26", "45", "60", "85")

for(i in 1:length(re)){
  url <- paste0(url.f, re[i])
  pg <- read_html(url)
  li <- html_attr(html_nodes(pg, "a"), "href")
    
  dir.create(di[i])
  setwd(di[i])
  zip <- grep(".zip", li, value = T)
 
    for(j in 1:length(an)){
      dir.create(an[j])
      setwd(an[j])
	    zip.an <- grep(an.s[j], zip, value = T)

	    for(k in 1:length(em)){
  	  	  dir.create(em[k])
  	  	  setwd(em[k])
		      zip.em <- grep(em.s[k], zip.an, value = T)
		      na <- substr(sub(paste0("http://biogeo.ucdavis.edu/data/climate/cmip5/", re[i], "/"), 
				               "", zip.em), 1, 8)
		
		      gcm <- unique(substr(na, 1, 2))
	  
		        for(l in gcm){
  	  	      dir.create(l)
  	  	      setwd(l)
		          zip.gcm <- grep(paste0("/", l), zip.em, value = T)
		          na.gcm <- grep(l, na, value = T)

			          for(m in 1:length(na.gcm)){
	   		          dir.create(na.gcm[m])
	    		        setwd(na.gcm[m])

			            download(zip.gcm[m], paste0(na.gcm[m], ".tif"), mode = "wb")
			            unzip(paste0(na.gcm[m], ".tif"))
			            unlink(paste0(na.gcm[m], ".tif"))

			            setwd("..")}

		         setwd("..")}

		     setwd("..")}

	    setwd("..")}

  setwd("..")}

###----------------------------------------------------------------------------###

### bioclim v02

### 1. current ###

# directory
setwd("..")
setwd("..")
dir.create("bioclim_v02")
setwd("bioclim_v02")
getwd()

# list of url
url <- "http://www.worldclim.org/version2"

pg <- read_html(url)
pg

li <- html_attr(html_nodes(pg, "a"), "href")
li

zip <- grep(".zip", li, value = T)
zip

# download
re <- paste0("_", c("10m", "5m", "2.5m", "30s"), "_")
re

di <- c("10m", "5m", "2_5m", "30s")
di

for(i in 1:length(re)){
  dir.create(di[i])
  setwd(di[i])

  do <- grep(re[i], zip, value = T)
  na <- sub("http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_", "", do)
  
    for(j in 1:length(na)){
      dir.create(sub(".zip", "", na[j]))
  	  setwd(sub(".zip", "", na[j]))

  	  download(do[j], na[j], mode = "wb")
  	  unzip(na[j])
  	  unlink(na[j])

  	  setwd("..")}
  
  setwd("..")}


###----------------------------------------------------------------------------###


