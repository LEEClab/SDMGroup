

library('maptools')
library(sp)
library(maps)
library("svDialogs")
library("rgeos")
library("rJava")

setwd("~/GitHub/SDMGroup/work_groups/occurrence")
source("funcoes.R")
project = 'pabloplants'


### Limpeza 
ruta_info_geo="C:\Users\User\Documents\GitHub\SDMGroup\work_groups\occurrence\GISBase"
BR.pol <- readShapePoly(fn=paste0(getwd(),"/GISBase/BR.shp"))
NT.pol <- readShapePoly(fn=paste0(getwd(),"/GISBase/neotropical.shp"))
info_geografica(ruta_info_geo)
### especificar polígono limite
background.pol <- pol.limite <- BR.pol

### lista de espécies
file.csv <- 'pablo_plants.txt'
spp.csv <- read.table(file.csv, header=TRUE, sep="\t",dec = ".") 
spp <- as.matrix(spp.csv[ ,1])

###

for (i in 1:NROW(spp)){
  
  sp <- spp[i]
  
  ### regarrega dados depois da junção
  occ.tmp <- occ.tmp.out <- occ.tmp.out.temp <- {}
  occ.tmp <- occ.full <- as.data.frame(read_occurrence_records(sp,project =project,data_source = 'all', sep=';'),stringsAsFactors = FALSE)
  
  if (!NROW(occ.tmp)>0){next()}
  
  # Retira registros com lat/long = 0 e NA
  
  remove_latlongNA0=TRUE # testar com lastInterpreted !!!
  if(remove_latlongNA0==TRUE)
  {
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$latitude),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$latitude),]
    if (NROW(occ.tmp.out.temp)>0)
    {
      remove.type = 'lat. NA'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)
    }
    
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$longitude),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$longitude),]
    if (NROW(occ.tmp.out.temp)>0)
    {
      remove.type = 'long. NA'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)
    }
    
    occ.tmp.out.temp <- occ.tmp[occ.tmp$latitude %in% 0,]
    occ.tmp <- occ.tmp[!occ.tmp$latitude %in% 0,]
    if (NROW(occ.tmp.out.temp)>0)
    {
      remove.type = 'lat. 0'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)
    }
    
    occ.tmp.out.temp <- occ.tmp[occ.tmp$longitude %in% 0,]
    occ.tmp <- occ.tmp[!occ.tmp$longitude == 0,]
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'long. 0'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
  }
  
  remove_anoNA=FALSE
  if(remove_anoNA==TRUE)
  {
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$yearcollected),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$yearcollected),] # retira anos NA 
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'Ano NA'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
  }
  
  remove_regdup=FALSE
  if(remove_regdup==TRUE)
  {
    occ.tmp.out.temp <- occ.tmp[duplicated(occ.tmp[1:length(occ.full)-1]),]
    occ.tmp <- occ.tmp[!duplicated(occ.tmp[]),] #Retira duplicados, fora source
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'reg.duplicado'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
  }
  
  # SE TRUE remove todos, precisa de ajuste
  remove_duplicata=FALSE # testar com lastInterpreted !!!
  if(remove_duplicata==TRUE){
    occ.tmp.out.temp <- occ.tmp[duplicated(occ.tmp$collector,occ.tmp$collectornumber,occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ),]
    occ.tmp <- occ.tmp[!duplicated(occ.tmp$collector,occ.tmp$collectornumber,occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ),] # retita coletor repetido
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'duplicata'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
    
    occ.tmp.out.temp <- occ.tmp[duplicated(occ.tmp$collectioncode,occ.tmp$catalognumber,occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ),]
    occ.tmp <- occ.tmp[!duplicated(occ.tmp$collectioncode,occ.tmp$catalognumber,occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ),] # retira colecao e tombo repetido
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'duplicado coleção'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
  }
  
  remove_anocorte=0 # esta dando erro
  if(remove_anocorte>0)
  {
    occ.tmp.out.temp <- occ.tmp[occ.tmp$yearcollected <= remove_anocorte,]
    occ.tmp <- occ.tmp[occ.tmp$yearcollected >= remove_anocorte,] # retira anos NA 
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = paste0('Ano < ',remove_anocorte)
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
  }
  
  standardizenames = FALSE
  if(standardizenames==TRUE)
  {
    occ.tmp$scientificname <- standardize_names(occ.tmp$scientificname)
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$scientificname),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$scientificname),]
    
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'nome inválido'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
  } 
  
  standardizenameshard = FALSE #TESTAR
  if(standardizenameshard==TRUE){occ.tmp$scientificname <- rep(c(sp),NROW(occ.tmp))} 
  
  remove_limiteespacial=TRUE # testar com lastInterpreted !!!
  if(remove_limiteespacial==TRUE & NROW(occ.tmp)>0)
  {
    occ.tmp.points <- SpatialPointsDataFrame(cbind(occ.tmp$longitude,occ.tmp$latitude),occ.tmp)
    occ.tmp.points_in <- occ.tmp.points[!is.na(over(occ.tmp.points ,geometry(background.pol))),] # dentro do poligono
    occ.tmp.points_out <- occ.tmp.points[is.na(over(occ.tmp.points ,geometry(background.pol))),] # fora do poligono
    
    occ.tmp.out.temp <- occ.tmp.points_out@data
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'fora polígono limite'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
  }
  

  set4=occ.tmp.points_in@data
  set4$ID=1:nrow(set4)
  # ### 1. Check country
  check.country=TRUE
  if(check.country==T){
    PAIS=corroboracion_pais(set4,paises)
    set4$ok_country=rep(NA,nrow(set4)) ;set4$ok_country[PAIS[[1]]]=1
    set4$suggest_country=PAIS[[3]]
    msgBox("Country concordance checked ")}
  
  
  #### 2. check state # just working for Brasil
  check.state=TRUE
  if(check.state==T){
    set5<-set4[which(set4$suggest_country=="Brasil"),] #2055
    DEP=corroboracion_dep(set5,estado)
    ok_state=rep(NA,nrow(set5)) ;ok_state[DEP[[1]]]=1
    set5=cbind(set5,ok_state)
    set5$suggest_state=DEP[[3]]
    msgBox("State concordance checked ")
  }
  
  
  ### 3. check municipality
  check.municipality=T
  if (check.municipality==T){
    A=corroboracion(set5,mpios)
    set5$ok_mun=NA ;set5$ok_mun[A[[1]]]=1
    set5$suggest_mun=A[[3]]
    msgBox("Municipality concordance checked ")}
  
  ##### 4. check that the coordinate is equal to the coordinate of the city municipality 
  check.citycoor=T
  if(check.citycoor==T){
    sedes_lat=which(set5$latitude %in% sedes@data$LATITUDE)
    set5$coord_mun=NA
    for ( i in sedes_lat){
      filas=which(sedes@data$LATITUDE %in% set5$latitude[i]) #elige latitudes iguales a las latitudes de municipalidades
      respuesta=NULL
      for(j in filas){# revisa  si para esas latitides iguales tambien las longitudes son igules
        resp=set5$longitude[i]==sedes@data$LONGITUDE[j]
        respuesta=c(respuesta,resp)
      }
      if (TRUE %in%respuesta){# si lat y longitud son igual a la de la municipalidad pone 1 de lo contrario 0
        set5$coord_mun[i]=1
      }else{set5$coord_mun[i]=0}
    }
    
    mun.ref=set5$ID[which(set5$coord_mun==1)] # identifica cuales  refID tienen la misma cordenada que la municipalidad
    # 
    if (length(mun.ref)!=0){
      msgBox(paste0("IDs ",mun.ref, " have the coordenate of the municipality, Be careful"))} else
      {msgBox(paste0("All the coordenate are different from the municipality"))}
  }
  ##### 7. check that the name coordinate is equal to the centroid of the  municipality 
  check.centroid=T
  if(check.centroid==T){
    dat=set5
    coordinates(dat)<-~longitude+latitude
    ovm=over(dat,mpios)
    mun_cent=ovm[,c("Longmuncen", "Latmuncent")]
    
    set5=cbind(set5,mun_cent)
    
    dif.lon=abs(set5$longitude-set5$Longmuncen)
    dif.lat=abs(set5$latitude-set5$Latmuncent)
    dif=cbind(dif.lon,dif.lat)
    
    iqual=which(dif[,1]<0.001 & dif[,2]<0.01)
    set5$coord_mun_cent=NA
    if (length(iqual)!=0){
      set5$coord_mun_cent[iqual]=1
      msgBox("IDs", set5$ID[iqual], "has the coordinate of the centroid of the municipality")
    }else{
      msgBox("All the coordenate are different from the municipality")
    }
  }
  
  
  
  # salva dados e limpeza
  set.fin=merge(set4,set5[,c("ID" ,"ok_state","suggest_state",
                             "ok_mun","suggest_mun","coord_mun","Longmuncen","Latmuncent",
                             "coord_mun_cent")] ,by="ID",all.x=T)
  geo.out=set.fin[which(is.na(set.fin$ok_country)|is.na(set.fin$ok_state)|is.na(set.fin$ok_mun)|set.fin$coord_mun==1|set.fin$coord_mun_cent==1),]
  geo.ok=set.fin[which(set.fin$ok_country==1 & set.fin$ok_state==1 & set.fin$ok_mun==1 & is.na(set.fin$coord_mun==1) & is.na(set.fin$coord_mun_cent)),]
  
  if(NROW(occ.tmp.out)>0){
    occ.tmp.out=cbind(ID=NA,occ.tmp.out[,-ncol(occ.tmp.out)],ok_country=NA,suggest_country=NA,ok_state=NA,
                    suggest_state=NA,ok_mun=NA,suggest_mun=NA,coord_mun=NA,Longmuncen=NA,
                    Latmuncent=NA,coord_mun_cent=NA)
    occ.tmp.out=rbind(occ.tmp.out, geo.out)} else{
      occ.tmp.out=geo.out
    }
  save_occurrence_records(occ.tmp.out,sp,project,'clean_out',sep='\t')
  
    
  save_occurrence_records(geo.ok,sp,project,'clean_in',sep='\t')
  }

