### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 01/06/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
memory.limit(size = 1.75e13) 

# install and require packages
# install.packages(c(maps", "maptools", "raster", "rgdal"), dep = T)

library(downloader)
library(maptools)
library(maps)
library(raster)
library(rgdal)

###-----------------------------------------------------------------------------###
###                                 gadm
###-----------------------------------------------------------------------------###

# diretorio
setwd("D:/environmental_data/vector/base_gadm")

# url 
url <- "http://biogeo.ucdavis.edu/data/gadm2.8/rds/"

# code
co <- c("ABW", "AFG", "AGO", "AIA", "ALA", "ALB", "AND", "ANT", "ARE", "ARG", 
        "ARM", "ASM", "ATA", "ATF", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", 
        "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", "BIH", "BLM", "BLR", "BLZ", 
        "BMU", "BOL", "BRA", "BRB", "BRN", "BTN", "BVT", "BWA", "CAF", "CAN", 
        "CCK", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COK", "COL", 
        "COM", "CPV", "CRI", "CUB", "CXR", "CYM", "CYP", "CZE", "DEU", "DJI", 
        "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", "ESP", "EST", 
        "ETH", "FIN", "FJI", "FLK", "FRA", "FRO", "FSM", "GAB", "GBR", "GEO", 
        "GGY", "GHA", "GIB", "GIN", "GLP", "GMB", "GNB", "GNQ", "GRC", "GRD", 
        "GRL", "GTM", "GUF", "GUM", "GUY", "HKG", "HMD", "HND", "HRV", "HTI", 
        "HUN", "IDN", "IMN", "IND", "IOT", "IRL", "IRN", "IRQ", "ISL", "ISR", 
        "ITA", "JAM", "JEY", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR", 
        "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA", "LIE", "LKA", 
        "LSO", "LTU", "LUX", "LVA", "MAC", "MAF", "MAR", "MCO", "MDA", "MDG", 
        "MDV", "MEX", "MHL", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MNP", 
        "MOZ", "MRT", "MSR", "MTQ", "MUS", "MWI", "MYS", "MYT", "NAM", "NCL", 
        "NER", "NFK", "NGA", "NIC", "NIU", "NLD", "NOR", "NPL", "NRU", "NZL", 
        "OMN", "PAK", "PAN", "PCN", "PER", "PHL", "PLW", "PNG", "POL", "PRI", 
        "PRK", "PRT", "PRY", "PSE", "PYF", "QAT", "REU", "ROU", "RUS", "RWA", 
        "SAU", "SDN", "SEN", "SGP", "SGS", "SHN", "SJM", "SLB", "SLE", "SLV", 
        "SMR", "SOM", "SPM", "SRB", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", 
        "SYC", "SYR", "TCA", "TCD", "TGO", "THA", "TJK", "TKL", "TKM", "TLS", 
        "TON", "TTO", "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "UKR", "UMI", 
        "URY", "USA", "UZB", "VAT", "VCT", "VEN", "VGB", "VIR", "VNM", "VUT", 
        "WLF", "WSM", "YEM", "ZAF", "ZMB", "ZWE")
co

le <- paste0("adm", 0:3)
le

int <- interaction(co, le, sep = "_")
int

do <- levels(int)
do

# download
for(i in 1:length(do)){
  tryCatch({
    download(paste0(url, do[i], ".rds"), paste0(do[i], ".rds"), mode = "wb")
    sh <- readRDS(paste0(do[i], ".rds"))
    writeOGR(sh, "shapefile", do[i], driver="ESRI Shapefile")
      }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
}



###-----------------------------------------------------------------------------###
