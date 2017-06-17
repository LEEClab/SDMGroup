### script rotina shyne limpeza pontos ###

# Pablo
# Caro
# Mauricio

###-----------------------------------------------------------------------------###
###     rotina     ###
###-----------------------------------------------------------------------------###

###-----------------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

search()

###-----------------------------------------------------------------------------###

# packages
install.packages(c("taxize", "flora", "sp", "rgdal", "maptools", "dplyr", "rgbif"), dep = T)

library(taxize)
library(spocc)
library(flora)
library(sp)
library(rgdal)
library(maptools)
library(dplyr)
library(rgbif)

# funcoes
source("funcoes_baixar_limpar_occ.R")


###-----------------------------------------------------------------------------###

### 1. lista de especies

# diretorio
setwd("D:/limpeza")

# nome do projeto
project <- "plantascalcario" 

# arquivo com lista de espécies
spp.csv <- read.table(paste0(project, ".txt"), h = T, sep = "\t", dec = ".")
spp <- as.matrix(spp.csv[, 1])

###-----------------------------------------------------------------------------###

### 2. preparar lista de especies
spp <- standardize_names(spp) # remove autores, retorno nome valido conforme Flora do Brasil
spp

# ou so remove autores
for (i in 1:NROW(spp)){spp[i] <- remove.authors(spp[i])}

###-----------------------------------------------------------------------------###


### 3. baixar gbif

data_source <- "gbif"

for (i in 1:NROW(spp)){
 sp <- spp[i]
 cat(" (", i, "-", sp)
 if(!file.exists(paste0(getwd(), "/OccurrenceRecords/", project, "/" , data_source, "/", sp, ".txt"))){
 
 ## pode-se passar só espécie
 spp.search <- as.data.frame(sp)
 
 # ou solicitar lista de sinônomos
 # spp.search <- nomes_sinonimos_florabr(sp)
 
 
 if (NROW(spp.search)>0){
 x <- busca_ocorrencia_gbif(spp.search)
 if (NROW(x)>0)
 {save_occurrence_records(x, sp, project, "gbif")}
 else{cat(" = ", "SEM REGISTROS!")}
 }else{cat(" = ", " informe uma espécie!)")}
 }else{cat(" = ", "já baixada!)")}
}

# 3.Dividir especies em arquivos separados

####
colunas.jabot <- c("taxoncompleto", "longitude", "latitude", "siglacolecao", "numtombo", "determinador", "pais", "estado_prov", "cidade", "descrlocal", "diacoleta", "mescoleta", "anocoleta", "coletor", "numcoleta", "notas")


path <- "D:/GitHub/gbif_occ_sinvar/OccurrenceRecords/pasf/jabot/"
file.csv <- "spp.txt"

xj <- read.csv(paste0(path, file.csv), dec = ".", sep = ", ", encoding = "Latin-1", header = T )

xj.sn <- standardize_names(xj$scientificname)

xj <- cbind(xj, data.frame(scientificname_sd = xj.sn))
spp.names <- distinct(as.data.frame(xj$scientificname_sd))

for(i in 1:NROW(spp.names))
{
 sp <- as.character(spp.names[i, 1])
 xj.sel <- xj[xj$scientificname_sd %in% sp, ]
 save_occurrence_records(xj.sel, sp, project, "jabot", sep = ", ")
}


####

#junção de dados de arquivos de mesmo nome de diferentes bases (por nome de especie por exemplo)

xg <- read_occurrence_records("Begonia exigua", project = project, data_source = "gbif", sep = " ")


for (i in 1:NROW(spp))
{
 sp <- spp[i]
 cat(" (", i, "-", sp)
 x <- xg <- xs <- xm <- {}
 xg <- read_occurrence_records(sp, project = project, data_source = "gbif", sep = " ", encoding = "Latin-1")
 xs <- read_occurrence_records(sp, project = project, data_source = "splink", sep = "\t", encoding = "UTF-8")
 xj <- read_occurrence_records(sp, project = project, data_source = "jabot", sep = ", ", encoding = "Latin-1")
 xm <- read_occurrence_records(sp, project = project, data_source = "mydataset", sep = "\t" )
 
x <- rbind(as.data.frame(xs, stringsAsFactors = F), 
  as.data.frame(xg, stringsAsFactors = F), 
  as.data.frame(xj, stringsAsFactors = F), 
  as.data.frame(xm, stringsAsFactors = F))
 
 save_occurrence_records(x, sp, project, "all", sep = ";")
}




### Limpeza
BR.pol <- readShapePoly(fn = paste0(getwd(), "/GISBase/BR.shp"))
NT.pol <- readShapePoly(fn = paste0(getwd(), "/GISBase/neotropical.shp"))
BR.UF.pol <- readShapePoly(fn = paste0(getwd(), "/GISBase/br_ufs.shp"))

### especificar polígono limite
background.pol <- pol.limite <- BR.UF.pol

###

for (i in 1:NROW(spp)){
 
 sp <- spp[i]
 
 ### regarrega dados depois da junção
 occ.tmp <- occ.tmp.out <- occ.tmp.out.temp <- {}
 occ.tmp <- occ.full <- as.data.frame(read_occurrence_records(sp, project = project, data_source = "all", sep = "\t"), stringsAsFactors = F)
 
 if (!NROW(occ.tmp)>0){next()}
 
 # Retira registros com lat/long = 0 e NA
 
 remove_latlongNA0 = T # testar com lastInterpreted !!!
 if(remove_latlongNA0 == T)
 {
 occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$latitude), ]
 occ.tmp <- occ.tmp[!is.na(occ.tmp$latitude), ]
 if (NROW(occ.tmp.out.temp)>0)
 {
 remove.type = "lat. NA"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)
 }
 
 occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$longitude), ]
 occ.tmp <- occ.tmp[!is.na(occ.tmp$longitude), ]
 if (NROW(occ.tmp.out.temp)>0)
 {
 remove.type = "long. NA"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)
 }
 
 occ.tmp.out.temp <- occ.tmp[occ.tmp$latitude %in% 0, ]
 occ.tmp <- occ.tmp[!occ.tmp$latitude %in% 0, ]
 if (NROW(occ.tmp.out.temp)>0)
 {
 remove.type = "lat. 0"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)
 }
 
 occ.tmp.out.temp <- occ.tmp[occ.tmp$longitude %in% 0, ]
 occ.tmp <- occ.tmp[!occ.tmp$longitude == 0, ]
 if (NROW(occ.tmp.out.temp)>0)
 {remove.type = "long. 0"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)}
 
 }
 
 remove_anoNA = F
 if(remove_anoNA == T)
 {
 occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$yearcollected), ]
 occ.tmp <- occ.tmp[!is.na(occ.tmp$yearcollected), ] # retira anos NA
 if (NROW(occ.tmp.out.temp)>0)
 {remove.type = "Ano NA"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)}
 
 }
 
 remove_regdup = F
 if(remove_regdup == T)
 {
 occ.tmp.out.temp <- occ.tmp[duplicated(occ.tmp[1:length(occ.full)-1]), ]
 occ.tmp <- occ.tmp[!duplicated(occ.tmp[]), ] #Retira duplicados, fora source
 if (NROW(occ.tmp.out.temp)>0)
 {remove.type = "reg.duplicado"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)}
 
 }
 
 # SE T remove todos, precisa de ajuste
 remove_duplicata = F # testar com lastInterpreted !!!
 if(remove_duplicata == T){
 occ.tmp.out.temp <- occ.tmp[duplicated(occ.tmp$collector, occ.tmp$collectornumber, occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ), ]
 occ.tmp <- occ.tmp[!duplicated(occ.tmp$collector, occ.tmp$collectornumber, occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ), ] # retita coletor repetido
 if (NROW(occ.tmp.out.temp)>0)
 {remove.type = "duplicata"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)}
 
 
 occ.tmp.out.temp <- occ.tmp[duplicated(occ.tmp$collectioncode, occ.tmp$catalognumber, occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ), ]
 occ.tmp <- occ.tmp[!duplicated(occ.tmp$collectioncode, occ.tmp$catalognumber, occ.tmp$yearcollected, occ.tmp$monthcollected, occ.tmp$daycollected ), ] # retira colecao e tombo repetido
 if (NROW(occ.tmp.out.temp)>0)
 {remove.type = "duplicado coleção"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)}
 }
 
 remove_anocorte = 0 # esta dando erro
 if(remove_anocorte>0)
 {
 occ.tmp.out.temp <- occ.tmp[occ.tmp$yearcollected < = remove_anocorte, ]
 occ.tmp <- occ.tmp[occ.tmp$yearcollected > = remove_anocorte, ] # retira anos NA
 if (NROW(occ.tmp.out.temp)>0)
 {remove.type = paste0("Ano < ", remove_anocorte)
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)}
 
 }
 
 standardizenames = F
 if(standardizenames == T)
 {
 occ.tmp$scientificname <- standardize_names(occ.tmp$scientificname)
 occ.tmp.out.temp <- occ.tmp[is.na(occ.tmp$scientificname), ]
 occ.tmp <- occ.tmp[!is.na(occ.tmp$scientificname), ]
 
 if (NROW(occ.tmp.out.temp)>0)
 {remove.type = "nome inválido"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)}
 
 }
 
 standardizenameshard = T #TESTAR
 if(standardizenameshard == T){occ.tmp$scientificname <- rep(c(sp), NROW(occ.tmp))}
 
 remove_limiteespacial = T # testar com lastInterpreted !!!
 if(remove_limiteespacial == T & NROW(occ.tmp)>0)
 {
 occ.tmp.points <- SpatialPointsDataFrame(cbind(occ.tmp$longitude, occ.tmp$latitude), occ.tmp)
 occ.tmp.points_in <- occ.tmp.points[!is.na(over(occ.tmp.points , geometry(background.pol))), ] # dentro do poligono
 occ.tmp.points_out <- occ.tmp.points[is.na(over(occ.tmp.points , geometry(background.pol))), ] # fora do poligono
 
 occ.tmp.out.temp <- occ.tmp.points_out@data
 if (NROW(occ.tmp.out.temp)>0)
 {remove.type = "fora polígono limite"
 occ.tmp.out.temp <- cbind(occ.tmp.out.temp, rep(remove.type, NROW(occ.tmp.out.temp)))
 occ.tmp.out <- rbind(occ.tmp.out, occ.tmp.out.temp)}
 }
 
 # salva dados e limpeza
 
 if(NROW(occ.tmp.out)>0){
 colnames(occ.tmp.out) <- c(colunas.splink, "source", "remove.type")
 save_occurrence_records(occ.tmp.out, sp, project, "clean_out", sep = "\t")
 }
 
 if(NROW(occ.tmp.points_in@data)>0){
 occ.sel <- occ.tmp.points_in@data
 save_occurrence_records(occ.tmp.points_in@data, sp, project, "clean_in", sep = "\t")
 }
}


for (i in 1:NROW(spp))
{
 sp <- spp[i]
 cat(" (", i, "-", sp)
 occ.all <- {}
 occ.all <- as.data.frame(read_occurrence_records(sp, project = project, data_source = "clean_in", sep = "\t", encoding = "UTF-8"), stringsAsFactors = F)
 
 distribution_map(sp, occ.all, "Detalhe", BR.UF.pol, T, T, project, data_source = "maps")
 
 distribution_map(sp, occ.all, "Neotrópico", BR.UF.pol, T, F, project, data_source = "maps")
}
dev.off()


### abrindo listagem separada
setwd("D:/Doutorado/Artigos em Preparacao/Samambaias e Licofitas em afloramentos calcarios no Brasil")

occ.all <- read.table("sl-calcario.txt", header = T, sep = "\t", dec = ".")
sp <- "spp"
project <- "slBR"
###

sp.resumo <- data.frame({})
NT.pol <- BR.UF.pol

for (i in 1:NROW(spp))
{
 sp <- spp[i]
 cat(" (", i, "-", sp)
 
 occ.all <- as.data.frame(read_occurrence_records(sp, project = project, data_source = "clean_in", sep = "\t", encoding = "UTF-8"), stringsAsFactors = F)
 
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "Brasil", NT.pol, NT.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "Brasil", NT.pol, BR.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "AC", NT.pol, AC.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "AL", NT.pol, AL.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "AM", NT.pol, AM.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "AP", NT.pol, AP.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "BA", NT.pol, BA.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "CE", NT.pol, CE.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "DF", NT.pol, DF.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "ES", NT.pol, ES.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "GO", NT.pol, GO.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "MA", NT.pol, MA.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "MG", NT.pol, MG.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "MS", NT.pol, MS.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "MT", NT.pol, MT.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "PA", NT.pol, PA.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "PB", NT.pol, PB.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "PE", NT.pol, PE.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "PI", NT.pol, PI.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "PR", NT.pol, PR.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "RJ", NT.pol, RJ.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "RN", NT.pol, RN.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "RO", NT.pol, RO.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "RR", NT.pol, RR.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "RS", NT.pol, RS.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "SC", NT.pol, SC.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "SE", NT.pol, SE.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "SP", NT.pol, SP.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "TO", NT.pol, TO.pol, T, F, project, data_source = "endemismo"))
 
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "Amazonia", NT.pol, amazonia.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "Caatinga", NT.pol, caatinga.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "Cerrado", NT.pol, cerrado.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "Mata Atlantica", NT.pol, mataatlantica.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "Pampa", NT.pol, pampa.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "Pantanal", NT.pol, pantanal.pol, T, F, project, data_source = "endemismo"))
 
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "BH Amazonas", NT.pol, amazonas.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "BH Atlantico Norte Nordeste", NT.pol, atlanticonortenordeste.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "BH Atlantico Sudeste", NT.pol, atlanticosudeste.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "BH Parana", NT.pol, parana.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "BH Sao Francisco", NT.pol, saofrancisco.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "BH Tocantins", NT.pol, tocantins.pol, T, F, project, data_source = "endemismo"))
 
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "WMCRO3 Karst Brazil ", NT.pol, Karst_Brazil.wmcro3.pol, T, F, project, data_source = "endemismo"))
 sp.resumo <- rbind(sp.resumo, endemismo(sp, occ.all, "ICMBio CECAV Karst Carbo.", NT.pol, Karst_icmbio.cecav.pol, T, F, project, data_source = "endemismo"))
 
}
c
save_occurrence_records(sp.resumo, "geographic_distribution_sumaryfim", project, "clean_in", sep = "\t")

getwd()




for (i in 1:NROW(spp))
{
 especie <- sp <- spp[i]
 cat(" (", i, "-", sp)
 
 x <- as.data.frame(read_occurrence_records(sp, project = project, data_source = "clean_in", sep = "\t", encoding = "UTF-8"), stringsAsFactors = F)
 
 xy <- data.frame( ocorrencia = paste0(x$collector, "-", x$collectornumber), 
   longitude = as.numeric(x$longitude), 
   latitude = as.numeric(x$latitude))
 
 sumary.dg <- dist.geografica(xy, zoom = F, save.map = T, BR.pol, nome = especie)
 save_occurrence_records(sumary.dg, "geographic_distribution_sumary", project, "geographic_distribution_sumary", sep = "\t")
 
}



#occ.all.source <- as.list(NULL)
#sources <- distinct(as.data.frame(occ.all$source))
#for(i in 1:NROW(sources)){
#name.source <- as.character(sources[i, 1])
#occ.all.source[[i]] <- occ.all[occ.all$source %in% name.source, ] }