library(httr)
library(jsonlite)
library(data.table)
library(readxl)
library(dplyr)
library(readr)
library(sf)
library(leaflet)
library(dplyr)
library(sp)
library(rgdal)
library(rgeos)
library(mapview)
library(readxl)
library(viridis)

source("modules/kobo.R")
source("modules/shiny_utils.R")
source("modules/shortcuts.R")
source("modules/utils.R")

data_bulletin<-kobo_data_downloader("219686", "contrat_performance:contrat2018",api = "kobohr")
names(data_bulletin) = gsub("/", "_", names(data_bulletin))

data_bulletin %>%
  mutate(Date_visite=today) %>%
  mutate(etablissement=gsub(" ","_",identification_nom_etablissement)) -> data_bulletin


data_visite<-kobo_data_downloader("234083", "contrat_performance:contrat2018",api = "kobohr")
names(data_visite) = gsub("/", "_", names(data_visite))

data_visite %>%
  mutate(Date_visite=today)->data_visite

iepp<- fread("data/iepp_18.csv")

iepp2<-read_xlsx("data/iepp_18.xlsx")

indicateurs <- read_xlsx("data/indicateurs.xlsx")
sig <- read_xlsx("data/sig_primaire_bon.xlsx")
prefecture <- readOGR("data/Cartes/.","TGO_l05_2012",encoding = "utf8",stringsAsFactors = FALSE)
pref <- st_as_sf(prefecture)
pref$PREFECTURE <- gsub("S/P ","",pref$PREFECTURE)
pref$PREFECTURE <- gsub("-","_",pref$PREFECTURE)
pref$PREFECTURE <- gsub(" ","_",pref$PREFECTURE)

#prefecture@data<-left_join(prefecture@data,sig,by = "PREFECTURE")

ecole <- inner_join(pref,sig)



#library(scales)
#french_percent <- percent_format(decimal.mark = ",", suffix = " %")
















