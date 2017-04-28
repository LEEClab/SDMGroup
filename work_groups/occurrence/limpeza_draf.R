ruta_info_geo="~/UNESP/TREECO/INFO_GEO/USANDO"


info_geografica<-function(ruta_info_geo){
  setwd(ruta_info_geo) 
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
    
    diferente=as.data.frame(cbind(cntrm,as.character(datos@data$stateprovince)))[im,]
    
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



info_geografica(ruta_info_geo)






# ### 4. Check country

set4=occ.sel
set4$ID=1:nrow(set4)
PAIS=corroboracion_pais(set4,paises)
set4$bien_pais=rep(NA,nrow(set4)) ;set4$bien_pais[PAIS[[1]]]=1

set4$sugerencia_pais=PAIS[[3]]

msgBox("Country concordance checked ")

#### 5. check state
set5<-set4[which(set4$sugerencia_pais=="Brasil"),] #2055
DEP=corroboracion_dep(set5,estado)
bien_depto=rep(NA,nrow(set5)) ;bien_depto[DEP[[1]]]=1

set5=cbind(set5,bien_depto)
set5$sugerencia_depto=DEP[[3]]

msgBox("State concordance checked ")



### 6. check municipality
A=corroboracion(set5,mpios)

set5$bien_mun=NA ;set5$bien_mun[A[[1]]]=1
set5$sugerencia_mun=A[[3]]

msgBox("Municipality concordance checked ")


##### 7. check that the name coordinate is equal to the coordinate of the city municipality 

sedes_lat=which(set5$latitude %in% sedes@data$LATITUDE)
set5$cord_mun=NA
for ( i in sedes_lat){
  filas=which(sedes@data$LATITUDE %in% set5$latitude[i]) #elige latitudes iguales a las latitudes de municipalidades
  respuesta=NULL
  for(j in filas){# revisa  si para esas latitides iguales tambien las longitudes son igules
    resp=set5$longitude[i]==sedes@data$LONGITUDE[j]
    respuesta=c(respuesta,resp)
  }
  if (TRUE %in%respuesta){# si lat y longitud son igual a la de la municipalidad pone 1 de lo contrario 0
    set5$cord_mun[i]=1
  }else{set5$cord_mun[i]=0}
}

mun.ref=set5$ID[which(set5$cord_mun==1)] # identifica cuales  refID tienen la misma cordenada que la municipalidad
# 
if (length(mun.ref)!=0){
msgBox(paste0("IDs ",mun.ref, " have the coordenate of the municipality, Be careful"))} else
{msgBox(paste0("All the coordenate are different from the municipality"))}


##### 7. check that the name coordinate is equal to the coordinate of the city municipality 

dat=set5
coordinates(dat)<-~longitude+latitude
ovm=over(dat,mpios)
mun.cent=ovm[,c("Longmuncen", "Latmuncent")]

set5=cbind(set5,mun.cent)

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



  

