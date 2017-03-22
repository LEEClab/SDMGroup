###################################################################################
### Busca subespécies, variedades e sinônimos coforme flora BR
### taxon_rank_search = c('species','subspecies','variety')
### return_type  = 'names','names_synonyms','synonyms'
### classification= TRUE (return the taxonomic  classification)
###################################################################################
setwd("~/GitHub/SDMGroup/work_groups/occurrence/OccurrenceRecords/animals")
mammals=read.csv("list mammals of brasil.csv")
sp.name=mammals$Sp.name[1:30]
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



animals.names=nomes_sinonimos_animals(sp.name, classification=F, return_type='synonyms')

animals.names2=nomes_sinonimos_animals(sp.name, taxon_rank_search = 'subspecies', classification=F)


