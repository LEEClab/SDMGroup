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
#load('sysdata.rda')
#setwd("~/R/win-library/3.3/flora/R")

### Busca Ocorrências na base gbif
####################################################################################=c("name","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber","country","stateProvince","county","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks")
#################################################################################### i=2
busca_ocorrencia_gbif <- function(sp_search, name_fields=c("issues","scientificName","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber","identifiedBy","country","stateProvince","county","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks"), remove.badissues=TRUE, limite=0){
  all_occ <- {}
  for (i in 1:NROW(sp_search)){
    sp.name <- as.character(sp_search$search.str[i])
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

  names_florabr <- all.taxa[which(all.taxa$search.str==sp_name_search & all.taxa$taxon.rank %in% taxon_rank_search),] 
  
  ### lista as subespécies e variedades de uma espécie
  #spname <-  get.taxa(sp_name_search)$search.str
  #genus_search <- substr(spname,1, gregexpr(' ', spname)[[1]][1]-1)
  #specific.epiteth_search <- substr(spname, gregexpr(' ', spname)[[1]][1]+1, nchar(spname))
  #names_florabr <- all.taxa[which(all.taxa$genus== genus_search & all.taxa$specific.epiteth == specific.epiteth_search & all.taxa$taxon.rank %in% taxon_rank_search),]
  ###
  
  synonyms_florabr <- all.taxa$search.str[all.taxa$id %in% relationships$related.id[relationships$id %in% names_florabr$id]]
  names <- distinct(as.data.frame(names_florabr,stringsAsFactors = FALSE),search.str)
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
#base = gbif, splink, jabot, literatura
#dir = allrecords, clean, 
##################################################################################
save_occurrence_records <- function(dat,file.name,base='gbif',dir='allrecords'){
  ## Grava resultados totais da busca por registros
  wd.root <- getwd()
  if( ! dir.exists(paste0(getwd(),"/OccurrenceRecords/")))
  {dir.create("OccurrenceRecords")}
  setwd(paste0(getwd(),"/OccurrenceRecords"))
  if(! dir.exists(paste0(getwd(),"/",base,"/")))
  {dir.create(base)}
  setwd(paste0(getwd(),"/",base))
  if( ! dir.exists(paste0(getwd(),"/",dir,"/")))
  {dir.create(dir)}
  setwd(paste0(getwd(),"/",dir))
  file_name_txt <- paste0(file.name,".txt")
  write.table(dat,file_name_txt,append = FALSE, quote = TRUE, sep = " ",eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double", fileEncoding = "UTF-8")
  setwd(wd.root)
}
##################################################################################
#base = gbif, splink, jabot, literatura
#sep = '\t', ';'
#encoding = "UTF-8", Latin-1
##################################################################################
read_occurrence_records
##################################################################################
read_occurrence_records <- function(file.name,base='splink',dir='allrecords',sep='\t',encoding="UTF-8",name.fields=''){

  colunas.splink <- c("scientificname","longitude","latitude", "collectioncode","catalognumber","identifiedby","country","stateprovince","county","locality","daycollected","monthcollected","yearcollected","collector","collectornumber","notes")	
  colunas.splink_mun <- c("scientificname","longitude_mun","latitude_mun", "collectioncode","catalognumber","identifiedby","country","stateprovince","county","locality","daycollected","monthcollected","yearcollected","collector","collectornumber","notes")	
  colunas.jabot <- c("taxoncompleto","longitude","latitude", "siglacolecao","numtombo","determinador","pais","estado_prov","cidade","descrlocal","diacoleta","mescoleta","anocoleta","coletor","numcoleta","notas")	
  colunas.gbif <- c("scientificName","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber","identifiedBy","country","stateProvince","county","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks")	

file.csv <- paste0(getwd(),"/OccurrenceRecords/",base,'/',dir,'/',file.name,".txt")
cat('lendo: ',file.csv)  

if(file.exists(file.csv)){
  occ.csv.full = read.table(file.csv, dec='.',sep=sep, header = TRUE,encoding= encoding )
  if(NROW(occ.csv.full)>0){
    if(base=='gbif')  {name.fields=colunas.gbif}
    if(base=='splink'){name.fields=colunas.splink}
    if(base=='jabot') {name.fields=colunas.jabot} #& length(name.fields)==0
    if(base=='all'){
      name.fields=c(colunas.splink,'source')
      occ.csv <- {}
      occ.csv = occ.csv.full[,name.fields]
      cat(' (',base,': ',NROW(occ.csv),' carregados)')  
      colnames(occ.csv) <- name.fields
      return(as.data.frame(occ.csv, stringsAsFactors = FALSE))
    }
    occ.csv <- {}
    occ.csv = occ.csv.full[,name.fields]
    occ.csv <- cbind(occ.csv,c(rep(base,NROW(occ.csv))))
    colnames(occ.csv) = c(colunas.splink,'source')
    cat(' (',base,': ',NROW(occ.csv),' carregados)')  

    if(base=='splink'){
      occ.csv_mun = {}
      occ.csv_mun = occ.csv.full[,colunas.splink_mun]
      occ.csv_mun <- occ.csv_mun[!occ.csv_mun$latitude_mun %in% NA & !occ.csv_mun$longitude_mun %in% NA,]  
      occ.csv_mun <- cbind(occ.csv_mun,c(rep('splink.mun',NROW(occ.csv_mun))))
      colnames(occ.csv_mun) <- c(colunas.splink,'source')
      occ.csv <- rbind(occ.csv,occ.csv_mun)
      cat('( splinl.cm: ',NROW(occ.csv_mun),' carregados)')  
      }
    }
    return(as.data.frame(occ.csv,stringsAsFactors = FALSE))  
}
else{cat(file.csv,' não encontrado')}
}


###################################################################################
# Lista de campos por base de busca, colocar na mesma orderm os campos correspondentes
###################################################################################
colunas.splink <- c("scientificname","longitude","latitude", "collectioncode","catalognumber","identifiedby","country","stateprovince","county","locality","daycollected","monthcollected","yearcollected","collector","collectornumber","notes")	
colunas.splink_mun <- c("scientificname","longitude_mun","latitude_mun", "collectioncode","catalognumber","identifiedby","country","stateprovince","county","locality","daycollected","monthcollected","yearcollected","collector","collectornumber","notes")	
colunas.jabot <- c("taxoncompleto","longitude","latitude", "siglacolecao","numtombo","determinador","pais","estado_prov","cidade","descrlocal","diacoleta","mescoleta","anocoleta","coletor","numcoleta","notas")	
colunas.gbif <- c("scientificName","decimalLongitude","decimalLatitude", "collectionCode","catalogNumber","identifiedBy","country","stateProvince","county","locality","day","month","year","recordedBy","recordNumber","occurrenceRemarks")	
###################################################################################




##################################################################################
#Salva dados conforme processo
##################################################################################
# project = projeto 
#data_source = gbif, splink, jabot, all, mydataset
##################################################################################
# save_occurrence_records <- function(dat, file.name, project='',data_source='',sep=' '){
#   ## Grava resultados totais da busca por registros
#   wd.root <- getwd()
#   if( ! dir.exists(paste0(getwd(),"/OccurrenceRecords/")))
#   {dir.create("OccurrenceRecords")}
#   setwd(paste0(getwd(),"/OccurrenceRecords"))
#   if(! dir.exists(paste0(getwd(),"/",project,"/")))
#   {dir.create(project)}
#   setwd(paste0(getwd(),"/",project))
#   if( ! dir.exists(paste0(getwd(),"/",data_source,"/")))
#   {dir.create(data_source)}
#   setwd(paste0(getwd(),"/",data_source))
#   file_name_txt <- paste0(file.name,".txt")
#   write.table(dat,file_name_txt,append = FALSE, quote = TRUE, sep = sep, eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double")
#   setwd(wd.root)
# }
##################################################################################

##################################################################################
#carrega dados conforme processo
##################################################################################
##################################################################################
# read_occurrence_records <- function(file.name,project='',data_source='',sep='\t',encoding="UTF-8",name.fields=''){
#   file.csv <- paste0(getwd(),"/OccurrenceRecords/",project,'/',data_source,'/',file.name,".txt")
#   cat(' <- lendo: ',file.csv)  
#   if(file.exists(file.csv)){
#     occ.csv.full = read.csv(file.csv, dec='.',sep=sep, header = TRUE,encoding= encoding)
#     if(NROW(occ.csv.full)>0){
#       if(data_source=='gbif')  {name.fields=colunas.gbif}
#       if(data_source=='splink'){name.fields=colunas.splink}
#       if(data_source=='jabot') {name.fields=colunas.jabot} #& length(name.fields)==0
#       if(!data_source %in% c('gbif','splink','jabot')){
#         name.fields=c(colunas.splink,'source')
#         occ.csv <- {}
#         occ.csv = occ.csv.full[,name.fields]
#         cat(' (',NROW(occ.csv),' registros) ')  
#         colnames(occ.csv) <- name.fields
#         return(as.data.frame(occ.csv, stringsAsFactors = FALSE))
#       }
#       occ.csv <- {}
#       occ.csv = occ.csv.full[,name.fields]
#       occ.csv <- cbind(occ.csv,c(rep(data_source,NROW(occ.csv))))
#       colnames(occ.csv) = c(colunas.splink,'source')
#       cat(' (',NROW(occ.csv),' registros) ')  
#       
#       if(data_source=='splink'){
#         occ.csv_mun = {}
#         occ.csv_mun = occ.csv.full[,colunas.splink_mun]
#         occ.csv_mun <- occ.csv_mun[!occ.csv_mun$latitude_mun %in% NA & !occ.csv_mun$longitude_mun %in% NA,]  
#         occ.csv_mun <- cbind(occ.csv_mun,c(rep('splink.mun',NROW(occ.csv_mun))))
#         colnames(occ.csv_mun) <- c(colunas.splink,'source')
#         occ.csv <- rbind(occ.csv,occ.csv_mun)
#         cat('(+',NROW(occ.csv_mun),' registros coordenadas município)') }
#       return(as.data.frame(occ.csv,stringsAsFactors = FALSE))}
#     else{cat(' # ',' arquivo vazio!# ')}
#     
#   }
#   else{cat(' # ',' não encontrado!# ')}
# }


##### functions  cleaning data with geographic information


info_geografica<-function(ruta_info_geo){
  
  require(maps)
  require(maptools)
  require(raster)
  require(rgdal)
  
  
  wdroot<-getwd()
  setwd(ruta_info_geo) 
  world<<-map("world")
  paises<<-readShapePoly("gadm28.shp")
  getData("GADM", level = 0, country="") # pais
  plot(br.0, col = "gray80", axes = T)
  br.0@data
  
  BR.pol <- readShapePoly("BR.shp")
  NT.pol <- readShapePoly("neotropical.shp")
  paises<<-readShapePoly("PAISES_SA_WGS84.shp")
  estado<<-readShapePoly("BRA_limite_estado.shp")
  nombres.est<<-as.character(estado@data$NAME_1)
  Encoding(nombres.est)<<-"latin1"
  estado@data$NAME_1<<-nombres.est
  mpios<<-readShapePoly("BRA_limite_mun3.shp")
  nombres.mun<<-as.character(mpios@data$NAME_2)
  Encoding(nombres.mun)<<-"latin1"
  mpios@data$NAME_2<<-nombres.mun
  sedes<<-readShapePoints("sedes_municipais.shp")
  setwd(wdroot)
}



### Revisar "overlay" de los registros con el "Shape" de departamento
corroboracion_pais=function(datos,mun){
  
  coordinates(datos)=~longitude+latitude
  #ovm=overlay(datos,mun)
  ovm=over(datos,mun)
  #cntrm=as.character(mun@data$COUNTRY[ovm$PAIS]) #esta 
  cntrm=as.character(ovm$PAIS)
  if(length(which(!is.na(datos@data$state)))>0){
    im=1:nrow(datos@data) # linea
    tb=as.character(datos@data$country)# fala
    
    
    CompareMun=cbind(im,cntrm,tb)
    
    
    ma1=NULL 
    for  ( i in 1:nrow(CompareMun)){
      tmp<-agrep(CompareMun[i,2],CompareMun[i,3], max=4,value=F,ignore.case=T)
      if (length(tmp)==0) tmp=0
      ma<-c(CompareMun[i,1],tmp)
      ma1=rbind(ma1,ma)
    }
    
    km=as.integer(ma1[which(is.na(ma1[,2])),1]) # pais diferente
    lm=as.integer(ma1[which(ma1[,2]==1),1]) # pais igual 
    
    
  }else{
    mm=rep(0,nrow(datos))
    op=rep(NA,nrow(datos))
  }
  
  X=list()
  X[[1]]=lm # pais correcto
  X[[2]]=km #pais errado
  X[[3]]=cntrm # sugerencia
  return(X)
}

corroboracion_dep=function(datos,estado){
  
  coordinates(datos)=~longitude+latitude
  #ovm=overlay(datos,estado)
  ovm=over(datos,estado)
  #cntrm=as.character(estado@data$HASC_1[ovm])
  cntrm1=as.character(ovm$HASC_1)
  cntrm2=as.character(ovm$NAME_1)
  Encoding(cntrm2)="latin1"
  
  if(length(which(!is.na(datos@data$stateprovince)))>0){
    im=jm=NULL
    for (j in 1:length(cntrm1)){
      print(j)
      name.c=cntrm1[j]
      name.c2=cntrm2[j]
      
      name.dat=as.character(datos@data$stateprovince)[j]
      Encoding(name.dat)="latin1"
      if (nchar(name.dat)==2){ comp=name.dat==name.c} else{
        comp=agrep(name.dat,name.c2,max.distance=3, ignore.case=T)
      }
      if (length(comp)==0){im=c(im,j) } else {## datos con diferente estado
        jm=c(jm,j)}## datos con igual estado 
    }
    
    diferente=as.data.frame(cbind(cntrm1,as.character(datos@data$stateprovince)))[im,]
    
    #CompareDpto=cbind(im,diferente)
    #MunCorrecto=cbind(cntrm,as.character(datos@data$state))[jm,]
    
    
  } else{
    im=rep(0,nrow(datos))
    jm=rep(NA,nrow(datos))
  }
  
  
  X=list()
  X[[1]]=jm # estado igual 
  X[[2]]=im# estado diferente
  X[[3]]=cntrm1 #nombre estado correcto
  return(X)
}

corroboracion=function(datos,mun){
  coordinates(datos)=~longitude+latitude
  #ovm=overlay(datos,mun)
  ovm=over(datos,mun)
  #cntrm=as.character(mun@data$NAME_2[ovm])
  cntrm=as.character(ovm$NAME_2)
  Encoding(cntrm)="latin1"
  
  if(length(which(!is.na(datos@data$county)))>0){
    mm=op=NULL
    for (j in 1:length(cntrm)){
      print(j)
      name.c=cntrm[j]
      name.dat=as.character(datos@data$county)[j]
      Encoding(name.dat)="latin1"
      # if(name.dat==""){name.dat=NA}else{name.dat=name.dat}
      if (!is.na(name.dat)& name.dat!=""){comp=agrep(name.dat,name.c,max.distance=4, ignore.case=T)}
      if (length(comp)!=0){mm=c(mm,j) } else {##iqual municipality 
        op=c(op,j)} ## different municipality
    }
    
  }else{
    mm=rep(0,nrow(datos))
    op=rep(NA,nrow(datos))
  }
  
  X=list()
  X[[1]]=mm
  X[[2]]=op
  X[[3]]=cntrm
  return(X)
}




