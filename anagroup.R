library(DBI)
library(RPostgres)
library(getPass)
library(readr)
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster, quietly=TRUE)
library(dplyr)
library(cluster)
library(stargazer)
library(seqhandbook)
library(factoextra)
library(gtsummary)
library(tidyr)
library(labelled)
library(GGally)
library(nnet)
library(ggeffects)
library(stringr)
library(Hmisc)
library(purrr)


options(encoding = "UTF-8") # Je définis l'encodages des caractères en UTF-8

con <- dbConnect(RPostgres::Postgres(), dbname = 'db_zjfap5mxw4', host='localhost', port='10000', user='user_fsw4sbba8u', password=getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = TRUE))
options(scipen = 9999)


data_groupe <- dbGetQuery(con,"SELECT
    m5_typpro,
  	m4_typpro,
  	m3_typpro,
  	m2_typpro,
  	m1_typpro,
  	p1_typpro,
  	p2_typpro,
  	p3_typpro,
  	p4_typpro,
  	idpk,
  	area,
  	ccodep,
  	iddep22,
  	idcom22
    ,cat
    ,typpat
    ---,densite1km
    ,groupe
    ,groupe2
  FROM anaseq.filiations_totales
    WHERE groupe IS NOT NULL
  ")

names(data_groupe) <- c("a2012","a2013","a2014","a2015","a2016","a2017","a2018","a2019","a2020"
                    ,"idpk", "area","ccodep","iddep22","idcom22"
                    ,"cat","typpat"
                    #,"densite1km"
                    ,"groupe","groupe2"
)

data_groupe_tbl <- data_groupe %>%
  group_by(groupe,groupe2) %>%
  summarise(area = sum(area)/1000000)

data_groupe2_tbl <- data_groupe %>%
  group_by(groupe2) %>%
  summarise(area = sum(area)/1000000)



#Analyse par groupe

data_groupe$typpat <- case_when(
  data_groupe$typpat == "Sport"
  | data_groupe$typpat == "Santé"
  | data_groupe$typpat == "Sport et santé"
  | data_groupe$typpat == "Culture et loisirs"
  | data_groupe$typpat == "Science et enseignement"
  | data_groupe$typpat == "Religieux"
  | data_groupe$typpat == 'Transport'
  | data_groupe$typpat == 'Gestion des eaux'
  | data_groupe$typpat == "Administratif et militaire"
  | data_groupe$typpat == "Administratif ou militaire"
  ~ 'Equipements publics et patrimoniaux',
  data_groupe$typpat == "Terrain à bâtir"
  | data_groupe$typpat == "Disponible"
  ~ "Non-affecté",
  TRUE ~ data_groupe$typpat)

#On remplace le NC qui apparaît sur une année seulement (manquement dans les données)
data_groupe <- data_groupe %>%
  mutate_at(vars(1:9),
            list(~ case_when(. == "NC"
                             | . == "Pas de propriétaire"
                             ~ "NC / Pas de propriétaire",
                             TRUE ~ .)))

for (i in 2:8) {
  if (i != ncol(data_groupe)) {
    current_col <- data_groupe[, i]
    prev_col <- data_groupe[, i - 1]
    next_col <- data_groupe[, i + 1]
    
    idx_to_replace <- which(current_col == "NC" & prev_col != "NC" & next_col != "NC")
    
    data_groupe[idx_to_replace, i] <- next_col[idx_to_replace]
  }
}




### UNE NOUVELLE SECTION AVEC DE NOUVELLES CHOSES

#### COUCOU LES CMI !
