###################################################################################
### Busca subespécies, variedades e sinônimos coforme flora BR
### taxon_rank_search = c('species','subspecies','variety')
### return_type  = 'names','names_synonyms','synonyms'
### classification= TRUE (return the taxonomic  classification)
###################################################################################
root.dir="~/UNESP/RICARDO"
root.funtions="~/GitHub/SDMGroup/work_groups/occurrence"

###### 0.load functions

setwd(root.funtions)
source("animal_names.R")
source("funcoes.R")
source("funcao-limpeza.R")
###### 1. load data
setwd(root.dir)
mammals=read.csv("spnames.csv")


###### 2.Taxonomic homogenization
sp.name=unique(as.character(mammals$sp.name))

animals.names=nomes_sinonimos_animals(sp.name, classification=T) #returns the original search and teh acepted name (str.search) in COL
# clasification T, returns thetaxonomic clasification 



###### 3.Get Gbif records
spp=as.data.frame(unique(animals.names$search.str), stringsAsFactors = F)
names(spp)='search.str'


for (i in 1:NROW(spp)){
  sp.search <- as.data.frame(spp[i,],stringsAsFactors = F)
  names(sp.search)='search.str'
  sp=as.character(sp.search)
  cat(' (',i,'-',as.character(sp.search),')')
   if (NROW(sp.search)>0){
      x<-busca_ocorrencia_gbif(sp.search)
      if (NROW(x)>0)
      {
        setwd(paste0(root.out,"/","Gbif"))
        file_name_txt <- paste0(sp,".txt")
        write.table(dat,file_name_txt,append = FALSE, quote = TRUE, sep = ",",eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = "double", fileEncoding = "UTF-8")
        setwd(root.dir)}
        
        #save_occurrence_records(x,sp,base = 'gbif',dir = "records_gbif" )}
      else{cat('=','sem registros!')}
   }
  else{cat('=',' não encontrada')}
  }

###### 4.Merge with splink data
setwd(root.dir)

diretorio="Ricardo"
for (i in 1:NROW(spp)){
  sp <- spp[i,1]
  cat(' (',i,'-',sp,")")
  x <- xg <- xs <- {}
  xg <- read_occurrence_records(sp,dir="Ricardo",base='gbif',sep=',',encoding= 'Latin-1')
  xs <- read_occurrence_records(sp,base='splink',sep=',',dir="Ricardo")
  #xj <- read_occurrence_records(sp,base='jabot',sep=',',dir=diretorio,encoding= 'Latin-1')
  x <- rbind(xg,xs)#,xj)
  save_occurrence_records(x,sp,base = 'all',dir=diretorio)
}

##### 5. Put all the species together
 
setwd(paste0(root.dir,"/OccurrenceRecords","/all/Ricardo"))

files=dir()
all.records=NULL

for (i in files){
  print(i)
  tryCatch({
    dat=read.csv(i,sep=" ",stringsAsFactors = F)}, error=function(e){cat(i,"is an empty file", conditionMessage(e), "\n")})
    all.records=rbind(all.records,dat)
}
all.records$longitude=as.numeric(all.records$longitude)
all.records$latitude=as.numeric(all.records$latitude)

##### 6.Clean data

setwd(root.dir)
#load geographic data
info_geografica(all.records)
#Clean the data
clean_data=limpeza(all.records, background.pol=background.pol)



