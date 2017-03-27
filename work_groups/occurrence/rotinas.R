
##################################################################################

# pasta do projeto em cada fonde de dados
setwd("D:\\GitHub\\SDMGroup\\work_groups\\occurrence")
diretorio <-"plantascalcario" 

### tipos de entrada
# arquivo com lista de espécies
spp.csv <- read.table('spp.txt', header=TRUE, sep="\t",dec = ".") 
spp <- as.matrix(spp.csv[ ,1])

#ou espécie por espécie
sp <- spp <- c('Cavanillesia umbellata')
sp <- spp <- c('Aralia warmingiana')

# loop baixar gbig
for (i in 1:NROW(spp)){
  sp <- spp[i]
  cat(' (',i,'-',sp)
  if(!file.exists(paste0(getwd(),'/OccurrenceRecords/gbif/',diretorio,'/',sp,'.txt'))){
    spp.search <- nomes_sinonimos_florabr(sp,return_type='names')#names_synonyms
    if (NROW(spp.search)>0){
      x<-busca_ocorrencia_gbif(spp.search)
      if (NROW(x)>0)
      {save_occurrence_records(x,sp,base = 'gbif',dir = diretorio)}
      else{cat('=','sem registros!')}
    }else{cat('=',' não encontrada em floraBR!)')}
  }else{cat('=','já baixado!)')}
}

#junção de dados de arquivos de mesmo nome de diferentes bases (por nome de especie por exemplo)

for (i in 1:NROW(spp)){
  sp <- spp[i]
  cat(' (',i,'-',sp)
  x <- xg <- xs <- {}
  xg <- read_occurrence_records(sp,base='gbif',sep=' ',dir=diretorio,encoding= 'Latin-1')
  xs <- read_occurrence_records(sp,base='splink',dir=diretorio)
  xj <- read_occurrence_records(sp,base='jabot',sep=',',dir=diretorio,encoding= 'Latin-1')
  x <- rbind(xg,xs,xj)
  save_occurrence_records(x,sp,base = 'all',dir=diretorio)
}

### Limpeza 
BR.pol <- readShapePoly(fn=paste0(getwd(),"/GISBase/BR.shp"))
NT.pol <- readShapePoly(fn=paste0(getwd(),"/GISBase/neotropical.shp"))

### especificar polígono limite
pol.limite <- NT.pol
###

### regarrega dados depois da junção
occ.tmp <- occ.full <- read_occurrence_records(spp,base='all',sep=' ',dir=diretorio,encoding= 'Latin-1')
cat(' (total. ',NROW(occ.full),')')

# Retira registros com lat/long = 0 e NA
remove_latlongNA0=TRUE # testar com lastInterpreted !!!
if(remove_latlongNA0){
  occ.tmp <- occ.tmp[!occ.tmp$latitude %in% 0 & !occ.tmp$longitude %in% 0 & !occ.tmp$latitude %in% NA & !occ.tmp$longitude %in% NA,]
  cat(' (Lat.Long NA e 0:',NROW(occ.full)-NROW(occ.tmp),')')}

remove_regdup=FALSE
if(remove_regdup){
  occ.tmp.dup <- occ.tmp[duplicated(occ.tmp[1:length(occ.full)-1]),]
  occ.tmp <- occ.tmp[!duplicated(occ.tmp[]),] #Retira duplicados, fora source
  cat(' (reg.dup.:',NROW(occ.tmp.dup),')')}

remove_anoNA=TRUE
if(remove_anoNA){
  occ.tmp.anoNA <- occ.tmp[occ.tmp$yearcollected %in% NA,]
  occ.tmp <- occ.tmp[!occ.tmp$yearcollected %in% NA,] # retira anos NA 
  cat(' (ano NA:',NROW(occ.tmp.anoNA),')')}  

remove_anocorte=1967
if(remove_anocorte>0){
  occ.tmp.anocorte <- occ.tmp[occ.tmp$yearcollected >= remove_anocorte,]
  occ.tmp <- occ.tmp[occ.tmp$yearcollected >= remove_anocorte,] # retira ano < 
  cat(' (acima ano corte ',remove_anocorte,' ',NROW(occ.tmp.anocorte),')')}  


remove_duplicata=FALSE # testar com lastInterpreted !!!
if(remove_duplicata){
  occ.tmp.dupcoletor <- occ.tmp[duplicated(occ.tmp$collector,occ.tmp$collectornumber,occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ),]
  occ.tmp <- occ.tmp[!duplicated(occ.tmp$collector,occ.tmp$collectornumber,occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ),] # retita coletor repetido
  cat(' (dup.coletor ',NROW(occ.tmp.dupcoletor),')')
  
  occ.tmp.dupcolecao <- occ.tmp[duplicated(occ.tmp$collectioncode,occ.tmp$catalognumber,occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ),]
  occ.tmp <- occ.tmp[!duplicated(occ.tmp$collectioncode,occ.tmp$catalognumber,occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ),] # retira colecao e tombo repetido
  cat(' (dup.coleção ',NROW(occ.tmp.dupcolecao),')')}

remove_limiteespacial=TRUE # testar com lastInterpreted !!!
if(remove_limiteespacial){
  occ.tmp.points <- SpatialPointsDataFrame(cbind(occ.tmp$longitude,occ.tmp$latitude),occ.tmp)
  occ.tmp.points_in <- occ.tmp.points[!is.na(over(occ.tmp.points ,geometry(pol.limite))),] # dentro do poligono
  occ.tmp.points_out <- occ.tmp.points[is.na(over(occ.tmp.points ,geometry(pol.limite))),] # fora do poligono
  cat(' (dentro ',NROW(occ.tmp.points_in),') (fora',NROW(occ.tmp.points_out),')')
}

# mapa exemplo
plot(pol.limite)
plot(occ.tmp.points_in, add=TRUE, col="green", pch=19, cex=0.5)
plot(occ.tmp.points_in, add=TRUE, col="black", pch=19, cex=0.2)
plot(occ.tmp.points_out, add=TRUE, col="red", pch=19, cex=0.2)

# salva dados da limpeza
occ.sel <- edit(occ.tmp.points_in@data)
save_occurrence_records(occ.sel,paste0('Limpeza-',sp),base = 'all',dir=diretorio)

