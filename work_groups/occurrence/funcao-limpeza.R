
limpeza<-function(occ.tmp=data.frame(na),
                  background.pol,
                  remove_latlongNA0=TRUE, 
                  remove_anoNA=TRUE,
                  remove_limiteespacial=TRUE,
                  check.country=TRUE,
                  check.state=TRUE,
                  check.municipality=TRUE,
                  check.citycoor=TRUE,
                  check.centroid=TRUE){
  
  ### atribuindo ID 
  
  ID=1:nrow(occ.tmp) 
  occ.tmp=cbind(ID,occ.tmp)
  occ.tmp.out <- occ.tmp.out.temp <- {}

  #remove_latlongNA0=TRUE # testar com lastInterpreted !!!
  if(remove_latlongNA0==TRUE){
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$latitude),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$latitude),]
    if (NROW(occ.tmp.out.temp)>0){
      remove.type = 'lat. NA'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$longitude),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$longitude),]
    if (NROW(occ.tmp.out.temp)>0){
      remove.type = 'long. NA'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
    occ.tmp.out.temp <- occ.tmp[occ.tmp$latitude %in% 0,]
    occ.tmp <- occ.tmp[!occ.tmp$latitude %in% 0,]
    if (NROW(occ.tmp.out.temp)>0){
      remove.type = 'lat. 0'
      occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
      occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
    
    occ.tmp.out.temp <- occ.tmp[occ.tmp$longitude %in% 0,]
    occ.tmp <- occ.tmp[!occ.tmp$longitude == 0,]
    if (NROW(occ.tmp.out.temp)>0){
      remove.type = 'long. 0'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp,reason=rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
  }
  
  #remove_anoNA=TRUE
  if(remove_anoNA==TRUE){
    occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$yearcollected),]
    occ.tmp <- occ.tmp[!is.na(occ.tmp$yearcollected),] # retira anos NA 
    if (NROW(occ.tmp.out.temp)>0)
    {remove.type = 'Ano NA'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
  }
  
  
  #remove_limiteespacial=TRUE # testar com lastInterpreted !!!
  if(remove_limiteespacial==TRUE & NROW(occ.tmp)>0)
  {
    occ.tmp.points <- SpatialPointsDataFrame(cbind(occ.tmp$longitude,occ.tmp$latitude),occ.tmp)
   
    CRS.new=background.pol@proj4string
    proj4string(occ.tmp.points) <- CRS.new 
    
    occ.tmp.points_in <- occ.tmp.points[!is.na(over(occ.tmp.points ,geometry(background.pol))),] # dentro do poligono
    occ.tmp.points_out <- occ.tmp.points[is.na(over(occ.tmp.points ,geometry(background.pol))),] # fora do poligono
    
    occ.tmp.out.temp <- occ.tmp.points_out@data
    if (NROW(occ.tmp.out.temp)>0)
    {
    remove.type = 'Out of boundary polygon'
    occ.tmp.out.temp <- cbind(occ.tmp.out.temp, reason=rep(remove.type,NROW(occ.tmp.out.temp)))
    occ.tmp.out <- rbind(occ.tmp.out,occ.tmp.out.temp)}
  }
  
  
  set4=occ.tmp.points_in@data
  set4$reason=NA
  #set4$ID=1:nrow(set4) # subir
  
  # ### 1. Check country
  #check.country=TRUE
  if(check.country==T){
    PAIS=corroboracion_pais(set4,paises)
    set4$ok_country=rep(NA,nrow(set4)) ;set4$ok_country[PAIS[[1]]]=1
    set4$suggest_country=PAIS[[3]]
    cat("Country concordance checked ")} # deixar msn como prints e não como janelas
  
  
  #### 2. check state # just working for Brasil
  #check.state=TRUE
  if(check.state==T){
    #set5<-set4[which(set4$suggest_country=="Brasil"),] #2055
    DEP=corroboracion_dep(set4,estado)
    ok_state=rep(NA,nrow(set4)) ;ok_state[DEP[[1]]]=1
    set4=cbind(set4,ok_state)
    set4$suggest_state=DEP[[3]]
    cat("State concordance checked ")
  }
  
  
  ### 3. check municipality
  #check.municipality=T
  if (check.municipality==T){
    A=corroboracion(set4,mpios)
    set4$ok_mun=NA ;set4$ok_mun[A[[1]]]=1
    set4$suggest_mun=A[[3]]
    cat("Municipality concordance checked ","!")}
  
  ##### 4. check that the coordinate is equal to the coordinate of the city municipality 
  #check.citycoor=T
  if(check.citycoor==T){
    sedes_lat=which(set4$latitude %in% sedes$lat)
    set4$coord_mun=NA
    for ( i in sedes_lat){
      filas=which(sedes$lat %in% set4$latitude[i]) #elige latitudes iguales a las latitudes de municipalidades
      respuesta=NULL
      for(j in filas){# revisa  si para esas latitides iguales tambien las longitudes son igules
        resp=set4$longitude[i]==sedes$long[j]
        respuesta=c(respuesta,resp)
      }
      if (TRUE %in%respuesta){# si lat y longitud son igual a la de la municipalidad pone 1 de lo contrario 0
        set4$coord_mun[i]=1
      }else{set4$coord_mun[i]=0}
    }
    
    mun.ref=set4$ID[which(set4$coord_mun==1)] # identifica cuales  refID tienen la misma cordenada que la municipalidad
    # 
    if (length(mun.ref)!=0){
      cat(paste0("IDs ",mun.ref, " have the coordenate of the municipality, Be careful","!"))} else
      {cat(paste0("All the coordenate are different from the municipality","!"))}
  }
  
  ##### 7. check that the name coordinate is equal to the centroid of the  municipality 
  ##### 
  #check.centroid=T
  if(check.centroid==T){
    dat=set4
    CRS.new=centroid@proj4string
    
    coordinates(dat)=~longitude+latitude
    proj4string(dat) <- CRS.new
    
    ovm=over(dat,centroid)
    mun_cent=ovm[,c("Longmuncen", "Latmuncent")]
    
    set4=cbind(set4,mun_cent)
    
    dif.lon=abs(set4$longitude-set4$Longmuncen)
    dif.lat=abs(set4$latitude-set4$Latmuncent)
    dif=cbind(dif.lon,dif.lat)
    
    iqual=which(dif[,1]<0.001 & dif[,2]<0.01)
    set4$coord_mun_cent=NA
    if (length(iqual)!=0){
      set4$coord_mun_cent[iqual]=1
      cat("IDs", set4$ID[iqual], "have the coordinate of the centroid of the municipality")
    }else{
      cat("All the coordenate are different from the municipality")
    }
  }
  
    
  # salva dados e limpeza

#   geo.out=set.fin[which(is.na(set.fin$ok_country)|is.na(set.fin$ok_state)|is.na(set.fin$ok_mun)|set.fin$coord_mun==1|set.fin$coord_mun_cent==1),]
#     geo.ok=set.fin[which(set.fin$ok_country==1 & set.fin$ok_state==1 & set.fin$ok_mun==1 & is.na(set.fin$coord_mun==1) & is.na(set.fin$coord_mun_cent)),]

   project="clean.results"
  if(NROW(occ.tmp.out)>0){
      occ.tmp.out=cbind(occ.tmp.out,ok_country=NA,suggest_country=NA,ok_state=NA, suggest_state=NA,ok_mun=NA,suggest_mun=NA,coord_mun=NA,Longmuncen=NA,Latmuncent=NA,coord_mun_cent=NA)
      occ.tmp.out.all=rbind(occ.tmp.out, set4)
      save_occurrence_records(occ.tmp.out.all,'limpeza-out',project,'clean_results')
      return(occ.tmp.out.all)
      } else{
        save_occurrence_records(set4,'limpeza-out',project,'clean_results')
        return(set4)
      }
    
    #save_occurrence_records(occ.tmp.out,'limpeza-out',project,'clean_out',sep='\t')
    
    
    
    #save_occurrence_records(geo.ok,'limpeza-ok',project,'clean_in',sep='\t')
  }




