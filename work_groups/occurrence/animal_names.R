###################################################################################
### Busca subespécies, variedades e sinônimos coforme flora BR
### taxon_rank_search = c('species','subspecies','variety')
### return_type  = 'names','names_synonyms','synonyms'
### classification= TRUE (return the taxonomic  classification)
###################################################################################
root.dir="~/UNESP/RICARDO"
setwd(root.dir)
mammals=read.csv("spnames.csv")
sp.name=unique(as.character(mammals$sp.name))
nomes_sinonimos_animals <- function(sp.name,return_type='names_synonyms', 
                                    taxon_rank_search = 'species',
                                    classification=TRUE){
  require('taxize')
  require("plyr")
  require("dplyr")
  names.col <-ldply(col_search(sp.name))
  
  
  names.acc=which(names.col$status=="accepted name")
  syn=which(names.col$status=="synonym")
  names.col$acc_name[names.acc]=names.col$name
  
  gen=sapply(strsplit(names.col$acc_name," ") , "[[", 1)
  epi=sapply(strsplit(names.col$acc_name," ") , "[[", 2)
  names.col$sp=paste0(gen," ",epi)
  
  if (return_type=="names"){
    if (taxon_rank_search=="species"){
      names=(names.col[names.acc,c(".id","sp")])}
    else{names=names.col[names.acc,c(".id","acc_name")]}}
  
  if (return_type=="synonyms"){
    names=names.col[syn,c(".id","name")]}
  
  if(return_type=='names_synonyms'){
    if (taxon_rank_search=="species"){
      names=(names.col[,c(".id","sp")])}
    else{names=names.col[,c(".id","acc_name")]}}
  
  #names=as.data.frame(cbind(as.character(sp.name),names))
  colnames(names) <- c('original','search.str')
  
  if(classification==T){
    animal.classification=ldply(classification(names[,2],db="col",rows=1), .fun=t)
    animal.classification2=animal.classification[-which(duplicated(animal.classification$.id)),]
    
    # joint animal calsification data to dataset
    animal.classification3= merge(names, animal.classification2, by.x="search.str", by.y=".id")
    animal.classification3=cbind(id=1:nrow(animal.classification3),animal.classification3)
    wrong=animal.classification3[which(!is.na(animal.classification3[,11])),]
    good=animal.classification3[-which(!is.na(animal.classification3[,11])),]
    wrong=wrong[, -8]
    names(wrong)=names(good)=c("id","original","search.str", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus")

    animal.classification4=rbind(wrong[,1:9], good[,1:9])
    animal.classification4=animal.classification4[order(animal.classification4$id),]
    #animal.classification4=cbind(sp.name,animal.classification4)
  }
  if (classification==T){return(animal.classification4)}else{return(names)}

}



animals.names=nomes_sinonimos_animals(sp.name, classification=T)

animals.names2=nomes_sinonimos_animals(sp.name, taxon_rank_search = 'subspecies', classification=F)


spp=as.data.frame(unique(animals.names$search.str), stringsAsFactors = F)
names(spp)='search.str'


# loop baixar gbig
for (i in 1:NROW(spp)){
  sp.search <- as.data.frame(spp[i,],stringsAsFactors = F)
  names(sp.search)='search.str'
  sp=as.character(sp.search)
  cat(' (',i,'-',as.character(sp.search),')')
   if (NROW(spp.search)>0){
      x<-busca_ocorrencia_gbif(sp.search)
      if (NROW(x)>0)
      {
        setwd(paste0(getwd(),"/","records_gbif"))
        file_name_txt <- paste0(sp,".txt")
        write.table(dat,file_name_txt,append = FALSE, quote = TRUE, sep = ",",eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double", fileEncoding = "UTF-8")
        setwd(root.dir)}
        
        #save_occurrence_records(x,sp,base = 'gbif',dir = "records_gbif" )}
      else{cat('=','sem registros!')}
   }
  else{cat('=',' não encontrada')}
  }#else{cat('=','já baixado!)')}


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

