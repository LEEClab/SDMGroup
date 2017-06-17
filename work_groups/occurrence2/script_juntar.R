
library('taxize')
library('spocc')
library('flora')
library('divagis')
library("ggmap")
library('mapr')
library('sp')
library('rgdal')
library('maptools')

library('dplyr') ## para manipular BD
library('rgbif')

# ajustar conforme fonte do pacote flora
load('D:\\R\\flora\\flora-master\\R\\sysdata.rda')

# colunas
colunas.splink <- c("scientificname","longitude","latitude", "collectioncode","catalognumber",
                    "identifiedby","country","stateprovince","county","locality","daycollected",
                    "monthcollected","yearcollected","collector","collectornumber","notes")
colunas.splink_mun <- c("scientificname","longitude_mun","latitude_mun", "collectioncode",
                        "catalognumber","identifiedby","country","stateprovince","county",
                        "locality","daycollected","monthcollected","yearcollected","collector",
                        "collectornumber","notes")
colunas.jabot <- c("taxoncompleto","longitude","latitude", "siglacolecao","numtombo","determinador",
                   "pais","estado_prov","cidade","descrlocal","diacoleta","mescoleta","anocoleta",
                   "coletor","numcoleta","notas")
colunas.gbif <- c("scientificName","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber",
                  "identifiedBy","country","stateProvince","county","locality","day","month","year",
                  "recordedBy","recordNumber","occurrenceRemarks")

###################################################################################
colunas.splink <- c("scientificname","longitude","latitude", "collectioncode","catalognumber","identifiedby","country","stateprovince","locality","daycollected","monthcollected","yearcollected","collector","collectornumber","notes")
colunas.splink_mun <- c("scientificname","longitude_mun","latitude_mun", "collectioncode","catalognumber","identifiedby","country","stateprovince","locality","daycollected","monthcollected","yearcollected","collector","collectornumber","notes")
colunas.jabot <- c("taxoncompleto","longitude","latitude", "siglacolecao","numtombo","determinador","pais","estado_prov","descrlocal","diacoleta","mescoleta","anocoleta","coletor","numcoleta","notas")
colunas.gbif <- c("scientificName","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber","identifiedBy","country","stateProvince","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks")
###################################################################################


### Shapes
####################################################################################
###################################################################################
path.gis.uf <-  "D:/R/spoc/gis/uf/"
AC.pol <- readShapePoly(fn=paste0(path.gis.uf,"AC.shp"))
AL.pol <- readShapePoly(fn=paste0(path.gis.uf,"AL.shp"))
AM.pol <- readShapePoly(fn=paste0(path.gis.uf,"AM.shp"))
AP.pol <- readShapePoly(fn=paste0(path.gis.uf,"AP.shp"))
BA.pol <- readShapePoly(fn=paste0(path.gis.uf,"BA.shp"))
CE.pol <- readShapePoly(fn=paste0(path.gis.uf,"CE.shp"))
DF.pol <- readShapePoly(fn=paste0(path.gis.uf,"DF.shp"))
ES.pol <- readShapePoly(fn=paste0(path.gis.uf,"ES.shp"))
GO.pol <- readShapePoly(fn=paste0(path.gis.uf,"GO.shp"))
MA.pol <- readShapePoly(fn=paste0(path.gis.uf,"MA.shp"))
MG.pol <- readShapePoly(fn=paste0(path.gis.uf,"MG.shp"))
MS.pol <- readShapePoly(fn=paste0(path.gis.uf,"MS.shp"))
MT.pol <- readShapePoly(fn=paste0(path.gis.uf,"MT.shp"))
PA.pol <- readShapePoly(fn=paste0(path.gis.uf,"PA.shp"))
PB.pol <- readShapePoly(fn=paste0(path.gis.uf,"PB.shp"))
PE.pol <- readShapePoly(fn=paste0(path.gis.uf,"PE.shp"))
PI.pol <- readShapePoly(fn=paste0(path.gis.uf,"PI.shp"))
PR.pol <- readShapePoly(fn=paste0(path.gis.uf,"PR.shp"))
RJ.pol <- readShapePoly(fn=paste0(path.gis.uf,"RJ.shp"))
RN.pol <- readShapePoly(fn=paste0(path.gis.uf,"RN.shp"))
RO.pol <- readShapePoly(fn=paste0(path.gis.uf,"RO.shp"))
RR.pol <- readShapePoly(fn=paste0(path.gis.uf,"RR.shp"))
RS.pol <- readShapePoly(fn=paste0(path.gis.uf,"RS.shp"))
SC.pol <- readShapePoly(fn=paste0(path.gis.uf,"SC.shp"))
SE.pol <- readShapePoly(fn=paste0(path.gis.uf,"SE.shp"))
SP.pol <- readShapePoly(fn=paste0(path.gis.uf,"SP.shp"))
TO.pol <- readShapePoly(fn=paste0(path.gis.uf,"TO.shp"))

path.gis.biomas <-  "D:/R/spoc/gis/biomas/"
amazonia.pol <- readShapePoly(fn=paste0(path.gis.biomas,"amazonia.shp"))
caatinga.pol <- readShapePoly(fn=paste0(path.gis.biomas,"caatinga.shp"))
cerrado.pol <- readShapePoly(fn=paste0(path.gis.biomas,"cerrado.shp"))
mataatlantica.pol <- readShapePoly(fn=paste0(path.gis.biomas,"mataatlantica.shp"))
pampa.pol <- readShapePoly(fn=paste0(path.gis.biomas,"pampa.shp"))
pantanal.pol <- readShapePoly(fn=paste0(path.gis.biomas,"pantanal.shp"))

path.gis.bacias <-  "D:/R/spoc/gis/bacias/"
amazonas.pol <- readShapePoly(fn=paste0(path.gis.bacias,"amazonas.shp"))
atlanticoleste.pol <- readShapePoly(fn=paste0(path.gis.bacias,"atlanticoleste.shp"))
atlanticonortenordeste.pol <- readShapePoly(fn=paste0(path.gis.bacias,"atlanticonortenordeste.shp"))
atlanticosudeste.pol <- readShapePoly(fn=paste0(path.gis.bacias,"atlanticosudeste.shp"))
parana.pol <- readShapePoly(fn=paste0(path.gis.bacias,"parana.shp"))
saofrancisco.pol <- readShapePoly(fn=paste0(path.gis.bacias,"saofrancisco.shp"))
tocantins.pol <- readShapePoly(fn=paste0(path.gis.bacias,"tocantins.shp"))

BR.pol <- readShapePoly(fn="D:\\BaseGIS\\Diva\\BR.shp")
World.pol <- readShapePoly(fn="D:\\BaseGIS\\WMCROv3\\worldmap\\worldmap_WGS1984.shp")
SA.pol <- readShapePoly(fn="D:\\BaseGIS\\WMCROv3\\world\\Continent-countries\\SouthAmericat.shp")
NEO.TROP.pol <- readShapePoly(fn="D:\\BaseGIS\\Diva\\neotropical.shp")

Karst_Brazil.wmcro3.pol <- readShapePoly(fn="D:\\BaseGIS\\WMCROv3\\southamerica_originalshp\\OriginalShapefile\\Karst_Brazil2.shp")

Karst_icmbio.cecav.pol <- readShapePoly(fn="D:\\BaseGIS\\Diva\\karstcal.shp")

###################################################################################

### Busca Ocorrências na base gbif
####################################################################################=c("issues","scientificName","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber","identifiedBy","country","stateProvince","county","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks")
#################################################################################### i=2
busca_ocorrencia_gbif <- function(sp_search, name_fields=c("issues","scientificName","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber","identifiedBy","country","stateProvince","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks"), remove.badissues=TRUE, limite=0){
  all_occ <- {}
  for (i in 1:NROW(sp_search)){
    sp.name <- as.character(sp_search[1,i])
    speciesKey <- name_backbone(name=sp.name)$speciesKey
    if (limite==0){
      if (length(speciesKey)>0){n.occ <- occ_count(speciesKey,basisOfRecord = 'PRESERVED_SPECIMEN')}
      else{n.occ <- 10}}
    else{n.occ <-limite}
    
    cat(' -> ',sp.name,' [key gbif ',speciesKey,'(',n.occ,') ]')
    
    ### busca via spocc
    # c("name","longitude","latitude", "collectionCode","catalogNumber","identifiedBy","country","stateProvince","county","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks","issues")
    # dat.spocc <- occ(query = sp.name, from = source.data,limit = n.occ)
    #    if(NROW(dat.spocc$gbif$data[[1]])==0){cat(' (Sem Registros)')}
    #    else{
    #      dat <- as.data.frame(dat.spocc$gbif$data[[1]])
    ###
    
    # busca registros
    dat.full <- occ_search( scientificName = sp.name, limit = n.occ, basisOfRecord = 'PRESERVED_SPECIMEN')
    #dat.full <- dat.full[!duplicated(dat.full),]
    cat(' - (',NROW(dat.full$data),') encontrados ')
    
    # retira issues restritivos
    if (remove.badissues & NROW(dat.full$data) > 0){
      dat.full.in <- trata_issues(dat.full)
      cat(' - (',NROW(dat.full.in$data),') sem issues ')}
    else{dat.full.in <-dat.full}
    
    dat <- {}
    if(NROW(dat.full.in$data)==0){cat(' (Sem Registros)')}
    else{
      dat <- as.data.frame(dat.full.in$data)
      colretorno <- name_fields[name_fields %in% colnames(dat)]
      if(length(colretorno)==length(name_fields)){
        dat = as.data.frame(dat[,name_fields])
        colnames(dat) = name_fields
        all_occ <- rbind2(all_occ,dat)
        cat(' - (',NROW(dat),') baixadas ')}
      else{
        if(NROW(dat)>1){cat(' # ',sp.name,'( Colunas Diferentes: ',name_fields[!name_fields %in% colnames(dat)],')')}}}}
  return(all_occ)}
###################################################################################

###################################################################################
### Busca subespécies, variedades e sinônimos coforme flora BR
### taxon_rank_search = c('species','subspecies','variety')
### return_type  = 'names','names_synonyms','synonyms'
###################################################################################
nomes_sinonimos_florabr <- function(sp_name_search, taxon_rank_search=c('species','subspecies','variety'), return_type='names'){
  
  
  
  file.csv <- paste0("D:/GitHub/SDMGroupLocal/DataSim/","taxon",".txt")
  taxon.floraBR = read.csv(file.csv, dec='.',sep='\t',encoding="UTF-8", header = TRUE)
  
  file.csv <- paste0("D:/GitHub/SDMGroupLocal/DataSim/","resourcerelationship",".txt")
  resourcerelationship.floraBR =read.csv(file.csv, dec='.',sep='\t',encoding="UTF-8", header = TRUE)
  
  file.csv <- paste0("D:/GitHub/SDMGroupLocal/DataSim/","taxon",".txt")
  speciesprofile.floraBR =read.csv(file.csv, dec='.',sep='\t',encoding="UTF-8", header = TRUE)
  
  file.csv <- paste0("D:/GitHub/SDMGroupLocal/DataSim/","distribution",".txt")
  distribution.floraBR = read.csv(file.csv, dec='.',sep='\t',encoding="UTF-8", header = TRUE)
  
  taxon_rank_search = c("ESPECIE","VARIEDADE","SUB_ESPECIE")
  taxonomicStatus_search = c("NOME_ACEITO", "SINONIMO")
  genus_search = 'Cavanillesia'
  specific.epiteth_search = 'arborea'
  
  genus_search = 'Ceiba'
  specific.epiteth_search = 'rubriflora'
  
  infraspecific.epiteth_search = ''
  
  if(infraspecific.epiteth_search == ''){
    names_florabr <- taxon.floraBR[which(
      taxon.floraBR$id == taxon.floraBR[which(
        taxon.floraBR$genus== genus_search &
          taxon.floraBR$specificEpithet == specific.epiteth_search),c("acceptedNameUsageID")]),]}
  
  #taxon.floraBR$taxonomicStatus %in% taxonomicStatus_search &
  #  taxon.floraBR$taxonRank %in% taxon_rank_search)
  
  if(infraspecific.epiteth_search != ''){
    names_florabr <- taxon.floraBR[which(
      taxon.floraBR$genus== genus_search &
        taxon.floraBR$specificEpithet == specific.epiteth_search &
        taxon.floraBR$infraspecificEpithet == infraspecific.epiteth_search &
        taxon.floraBR$taxonomicStatus %in% taxonomicStatus_search &
        taxon.floraBR$taxonRank %in% taxon_rank_search),]}
  
  taxon.floraBR$str_acept = ifelse(taxon.floraBR$taxonomicStatus=="VARIEDADE"){paste0(taxon.floraBR$genus," ",taxon.floraBR$specificEpithet,"var. ",taxon.floraBR$infraspecificEpithet; )}
  
  
  ###
  names_florabr <- standardize_names(c(sp_name_search))
  
  ### lista as subespécies e variedades de uma espécie
  #spname <-  get.taxa(sp_name_search)$search.str
  #genus_search <- substr(spname,1, gregexpr(' ', spname)[[1]][1]-1)
  #specific.epiteth_search <- substr(spname, gregexpr(' ', spname)[[1]][1]+1, nchar(spname))
  #names_florabr <- all.taxa[which(all.taxa$genus== genus_search & all.taxa$specific.epiteth == specific.epiteth_search & all.taxa$taxon.rank %in% taxon_rank_search),]
  ###
  
  
  
  
  synonyms_florabr <- all.taxa$search.str[all.taxa$id %in% relationships$related.id[relationships$id %in% names_florabr$id]]
  #names <- distinct(as.data.frame(names_florabr,stringsAsFactors = FALSE),search.str)
  synanyms <- distinct(as.data.frame(synonyms_florabr,stringsAsFactors = FALSE))
  colnames(names) <- colnames(synanyms)<- c('search.str')
  
  if (return_type == 'names'){return(names)}
  if (return_type == 'synonyms'){return(synanyms)}
  if (return_type == 'names_synonyms'){return(rbind(names,synanyms))}
}
##################################################################################
nomes_sinonimos_tropicos <- function(sp.name,return_type='names_synonyms'){
  #require('taxize')
  x <- synonyms(c(sp.name),db="tropicos")[[1]]
  synanyms <- as.data.frame(distinct(as.data.frame(x$scientificname)),stringsAsFactors = FALSE)
  names <- as.data.frame(tp_search(name = sp.name)$scientificname,stringsAsFactors = FALSE)
  colnames(names) <- c('search.str')
  colnames(synanyms) <- c('search.str')
  if (return_type == 'names'){return(names)}
  if (return_type == 'synonyms'){return(synanyms)}
  if (return_type == 'names_synonyms'){return(rbind(names,synanyms))}
}
##################################################################################
##################################################################################
# Trata issues
##################################################################################
trata_issues <- function(dat.full){
  dat.full <- dat.full %>% occ_issues(mutate = "expand")
  
  # ver detalhes em http://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
  dat.full.in <- dat.full %>% occ_issues(-ZERO_COORDINATE,
                                         -COORDINATE_OUT_OF_RANGE,
                                         -COORDINATE_INVALID,
                                         -GEODETIC_DATUM_INVALID,
                                         -COORDINATE_REPROJECTION_FAILED,
                                         -COORDINATE_ACCURACY_INVALID,
                                         -COORDINATE_PRECISION_INVALID,
                                         -COORDINATE_UNCERTAINTY_METERS_INVALID,
                                         -COUNTRY_INVALID,
                                         -CONTINENT_INVALID,
                                         -PRESUMED_SWAPPED_COORDINATE
                                         -RECORDED_DATE_INVALID,
                                         -PRESUMED_NEGATED_LONGITUDE,
                                         -PRESUMED_NEGATED_LATITUDE,
                                         -BASIS_OF_RECORD_INVALID)
  return(dat.full.in)}

##################################################################################
#Salva dados conforme processo
##################################################################################
# project = projeto
#data_source = gbif, splink, jabot, all, mydataset
##################################################################################
save_occurrence_records <- function(dat, file.name, project='',data_source='',sep=' '){
  ## Grava resultados totais da busca por registros
  wd.root <- getwd()
  if( ! dir.exists(paste0(getwd(),"/occurrencerecords/")))
  {dir.create("OccurrenceRecords")}
  setwd(paste0(getwd(),"/occurrencerecords"))
  if(! dir.exists(paste0(getwd(),"/",project,"/")))
  {dir.create(project)}
  setwd(paste0(getwd(),"/",project))
  if( ! dir.exists(paste0(getwd(),"/",data_source,"/")))
  {dir.create(data_source)}
  setwd(paste0(getwd(),"/",data_source))
  file_name_txt <- paste0(file.name,".txt")
  write.table(dat,file_name_txt,append = FALSE, quote = TRUE, sep = sep, eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double")
  setwd(wd.root)
}
##################################################################################

##################################################################################
#carrega dados conforme processo
##################################################################################
##################################################################################
read_occurrence_records <- function(file.name,project='',data_source='',sep='\t',encoding="UTF-8",name.fields=''){
  file.csv <- paste0(getwd(),"/occurrencerecords/",project,'/',data_source,'/',file.name,".txt")
  cat(' <- lendo: ',file.csv)
  if(file.exists(file.csv)){
    occ.csv.full = read.csv(file.csv, dec='.',sep=sep, header = TRUE,encoding= encoding)
    if(NROW(occ.csv.full)>0){
      if(data_source=='gbif')  {name.fields=colunas.gbif}
      if(data_source=='splink'){name.fields=colunas.splink}
      if(data_source=='jabot') {name.fields=colunas.jabot} #& length(name.fields)==0
      if(!data_source %in% c('gbif','splink','jabot')){
        name.fields=c(colunas.splink,'source')
        occ.csv <- {}
        occ.csv = occ.csv.full[,name.fields]
        cat(' (',NROW(occ.csv),' registros) ')
        colnames(occ.csv) <- name.fields
        return(as.data.frame(occ.csv, stringsAsFactors = FALSE))
      }
      occ.csv <- {}
      occ.csv = occ.csv.full[,name.fields]
      occ.csv <- cbind(occ.csv,c(rep(data_source,NROW(occ.csv))))
      colnames(occ.csv) = c(colunas.splink,'source')
      cat(' (',NROW(occ.csv),' registros) ')
      
      if(data_source=='splink'){
        occ.csv_mun = {}
        occ.csv_mun = occ.csv.full[,colunas.splink_mun]
        occ.csv_mun <- occ.csv_mun[!occ.csv_mun$latitude_mun %in% NA & !occ.csv_mun$longitude_mun %in% NA,]
        occ.csv_mun <- cbind(occ.csv_mun,c(rep('splink.mun',NROW(occ.csv_mun))))
        colnames(occ.csv_mun) <- c(colunas.splink,'source')
        occ.csv <- rbind(occ.csv,occ.csv_mun)
        cat('(+',NROW(occ.csv_mun),' registros coordenadas município)') }
      return(as.data.frame(occ.csv,stringsAsFactors = FALSE))}
    else{cat(' # ',' arquivo vazio!# ')}
    
  }
  else{cat(' # ',' não encontrado!# ')}
}


##################################################################################
# Alinhamento de nomes conforme Flora do Brasil
##################################################################################
# returntype= 'name','all'
##################################################################################
standardize_names <- function(names_list,returntype='name'){
  
  names_list_sel <- as.matrix( names_list)
  names_list_sel1 <- {}
  for (i in 1:nrow(names_list_sel))
  { names_list_sel[i] <- remove.authors(standardize.names(names_list_sel[i]))}
  
  names_list_acept <- get.taxa(names_list_sel,
                               replace.synonyms = TRUE,
                               drop = c(""),
                               states = FALSE,
                               suggest.names = FALSE,
                               suggestion.distance = 0.9,
                               life.form = FALSE,
                               habitat = FALSE,
                               vernacular = FALSE,
                               establishment = FALSE)
  
  return(if(returntype=='name'){names_list_acept$search.str}
         else{names_list_acept})
}

###################################################################################
#
###################################################################################

distribution_map <- function(name.specie, occ.points, name.map, background.pol, save.map=TRUE, zoom=FALSE,project='',data_source=''){
  
  occ.points.all <- occ.points.splink <- occ.points.splink.mun <- occ.points.gbif <- occ.points.jabot <- {}
  
  if (!NROW(occ.points)>0){return(NULL)}
  
  occ.points.all <- SpatialPointsDataFrame(cbind(occ.points$longitude,occ.points$latitude),occ.points)
  
  name.map.txt = paste0(name.specie,' - ',name.map)
  file.jpg = paste0(path.occ(project,data_source),name.map.txt,'.jpg')
  
  if(save.map==TRUE){jpeg(file.jpg)}
  
  if(zoom==TRUE)
  {
    plot(occ.points.all, col="black", axes=TRUE, lty=1, lwd=1, pch=19, cex=1,main=name.map.txt)
    plot(background.pol, add=TRUE, border="gray40", lty=1, lwd=1)
  }
  else
  {
    plot(background.pol, axes=TRUE, border="gray40", lty=1, lwd=1, main=name.map.txt)
    plot(occ.points.all, add=TRUE, col="black", cex=1,lty=1, lwd=1, pch=19)
  }
  
  if (NROW(occ.points[occ.points$source %in% c('splink.mun'),])>0)
  {
    occ.points.splink.mun <- SpatialPointsDataFrame(cbind(occ.points[occ.points$source %in% c('splink.mun'),]$longitude,occ.points[occ.points$source %in% c('splink.mun'),]$latitude),occ.points[occ.points$source %in% c('splink.mun'),])
    
    plot(occ.points.splink.mun, add=TRUE, col="red", cex=0.5,lty=1, lwd=1, pch=19)
  }
  
  if (NROW(occ.points[occ.points$source %in% c('gbif'),])>0)
  {
    occ.points.gbif <- SpatialPointsDataFrame(cbind(occ.points[occ.points$source %in% c('gbif'),]$longitude,occ.points[occ.points$source %in% c('gbif'),]$latitude),occ.points[occ.points$source %in% c('gbif'),])
    
    plot(occ.points.gbif, add=TRUE, col="green", cex=0.5,lty=1, lwd=1, pch=19)
  }
  
  if (NROW(occ.points[occ.points$source %in% c('splink'),])>0)
  {
    occ.points.splink <- SpatialPointsDataFrame(cbind(occ.points[occ.points$source %in% c('splink'),]$longitude,occ.points[occ.points$source %in% c('splink'),]$latitude),occ.points[occ.points$source %in% c('splink'),])
    
    plot(occ.points.splink, add=TRUE, col="blue", cex=0.5,lty=1, lwd=1, pch=19)
  }
  
  if (NROW(occ.points[occ.points$source %in% c('jabot'),])>0)
  {
    occ.points.jabot <- SpatialPointsDataFrame(cbind(occ.points[occ.points$source %in% c('jabot'),]$longitude,occ.points[occ.points$source %in% c('jabot'),]$latitude),occ.points[occ.points$source %in% c('jabot'),])
    
    plot(occ.points.jabot, add=TRUE, col="gray60", cex=0.5,lty=1, lwd=1, pch=19)
  }
  
  if(save.map==TRUE){dev.off()}
}
###################################################################################

###################################################################################
#
###################################################################################
path.occ <- function(project='',data_source=''){
  wd.root <- getwd()
  if( ! dir.exists(paste0(getwd(),"/occurrencerecords/")))
  {dir.create("OccurrenceRecords")}
  setwd(paste0(getwd(),"/occurrencerecords"))
  if(! dir.exists(paste0(getwd(),"/",project,"/")))
  {dir.create(project)}
  setwd(paste0(getwd(),"/",project))
  if( ! dir.exists(paste0(getwd(),"/",data_source,"/")))
  {dir.create(data_source)}
  wd.occ <- paste0(getwd(),"/",data_source,'/')
  setwd(wd.root)
  return(wd.occ)}
###################################################################################


###################################################################################
#
###################################################################################

endemismo <- function(name.specie, occ.points, name.map, background.pol,limit.pol, save.map=TRUE, zoom=FALSE,project='',data_source=''){
  
  occ.tmp.points <- occ.tmp.points_in <- occ.tmp.points_out <- occ.tmp.points_add <- states.occurrences <- occurrence.state <- {}
  
  if (!NROW(occ.points)>0){return(NULL)}
  
  occ.tmp.points <- SpatialPointsDataFrame(cbind(occ.points$longitude,occ.points$latitude),occ.points)
  
  occ.tmp.points_in <- occ.tmp.points[!is.na(over(occ.tmp.points ,geometry(limit.pol))),] # dentro do poligono
  occ.tmp.points_out <- occ.tmp.points[is.na(over(occ.tmp.points ,geometry(limit.pol))),] # fora do poligono
  occ.tmp.points_add <- occ.tmp.points[!is.na(over(occ.tmp.points ,geometry(BR.pol))),]
  
  name.map.txt = paste0(name.specie,' - ',name.map)
  file.jpg = paste0(path.occ(project,data_source),name.map.txt,'.jpg')
  
  if(save.map==TRUE){jpeg(file.jpg)}
  
  if(zoom==TRUE)
  {
    plot(occ.tmp.points, col="black", axes=TRUE, lty=1, lwd=1, pch=19, cex=1,main=name.map.txt)
    plot(background.pol, add=TRUE, border="gray80", lty=1, lwd=1)
    plot(limit.pol, add=TRUE, border="gray40", lty=1, lwd=1)
  }
  else
  {
    plot(background.pol, axes=TRUE, border="gray80", lty=1, lwd=1, main=name.map.txt)
    plot(limit.pol, add=TRUE, border="gray40", lty=1, lwd=1)
    plot(occ.tmp.points, add=TRUE, col="black", cex=1,lty=1, lwd=1, pch=19)
  }
  
  if (NROW(occ.tmp.points_out@data)>0)
  {
    plot(occ.tmp.points_out, add=TRUE, col="red", cex=0.5,lty=1, lwd=1, pch=19)
  }
  
  if (NROW(occ.tmp.points_in@data)>0)
  {
    plot(occ.tmp.points_in, add=TRUE, col="green", cex=0.5,lty=1, lwd=1, pch=19)
  }
  
  
  #  if (NROW(occ.points[occ.points$source %in% c('jabot'),])>0)
  #  {
  #    occ.points.jabot <- SpatialPointsDataFrame(cbind(occ.points[occ.points$source %in% c('jabot'),]$longitude,occ.points[occ.points$source %in% c('jabot'),]$latitude),occ.points[occ.points$source %in% c('jabot'),])
  #  }
  
  states.occurrences <- get.taxa(name.specie,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,c(''))$occurrence
  occurrence.state <- if(length(grep(states.occurrences,paste0('BR-',name.map)))>0){1}else{0}
  
  percetual_BR = round(NROW(occ.tmp.points_in@data)/NROW(occ.tmp.points_add@data)*100,2)
  if(percetual_BR>100){percetual_BR=NA}
  
  if(save.map==TRUE){dev.off()}
  
  return(data.frame(scientificname= name.specie,
                    ocorrencias_total = NROW(occ.tmp.points@data),
                    area_endemismo = name.map,
                    ocorrencias_dentro = NROW(occ.tmp.points_in@data),
                    percetual_dentro = round(NROW(occ.tmp.points_in@data)/NROW(occ.tmp.points@data)*100,2),
                    ocorrencias_fora = NROW(occ.tmp.points_out@data),
                    percetual_fora = round(NROW(occ.tmp.points_out@data)/NROW(occ.tmp.points@data)*100,2),
                    ocorrencias_BR = NROW(occ.tmp.points_add@data),
                    percetual_BR = percetual_BR,
                    occurrence.state.floraBR = occurrence.state
  ))
}
###################################################################################