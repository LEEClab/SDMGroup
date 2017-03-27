

library('maptools')


setwd("D:/GitHub/SDMGroup/work_groups/occurrence")
project = 'pabloplants'


### Limpeza 
BR.pol <- readShapePoly(fn=paste0(getwd(),"/GISBase/BR.shp"))
NT.pol <- readShapePoly(fn=paste0(getwd(),"/GISBase/neotropical.shp"))

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
  
  # salva dados e limpeza
  
  if(NROW(occ.tmp.out)>0){
    colnames(occ.tmp.out) <-  c(colunas.splink,'source','remove.type')
    save_occurrence_records(occ.tmp.out,sp,project,'clean_out',sep='\t')
  } 
  
  if(NROW(occ.tmp.points_in@data)>0){
    occ.sel <- occ.tmp.points_in@data
    save_occurrence_records(occ.tmp.points_in@data,sp,project,'clean_in',sep='\t')
  }
}
