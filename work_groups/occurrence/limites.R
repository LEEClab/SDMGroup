### script limits and points of countries, states and municipalities ###

# fontes
# http://www.gis-blog.com/r-raster-data-acquisition/
# https://www.students.ncl.ac.uk/keith.newman/r/maps-in-r-using-gadm
# https://mvzgis.wordpress.com/gis-basics/

# instalar pacotes
# install.packages(c("maps", "maptools", "raster", "rgdal"), dep = T)

# carregar pacotes
library(maps)
library(maptools)
library(raster)
library(rgdal)

# baixar dados de país, estados, municipios e distritos
br.0 <- getData("GADM", country = "BRA", level = 0) # pais
plot(br.0, col = "gray80", axes = T)
br.0@data

br.1 <- getData("GADM", country = "BRA", level = 1) # estados
plot(br.1, col = "gray80", axes = T)
br.1@data

br.2 <- getData("GADM", country = "BRA", level = 2) # municipios/cidades
plot(br.2, col = "gray80", axes = T)
br.2@data

br.3 <- getData("GADM", country = "BRA", level = 3) # distritos ou 
plot(br.3, col = "gray80", axes = T)
br.3@data

# exportar shapefiles
writeOGR(br.0, "shapes", "limite_brasil_gcs_wgs84.shp", driver="ESRI Shapefile")

