library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(readxl)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)

commune_gps <- read_csv2("data/commune_gps_tidied.csv")
Promethee<-read_delim("data/liste_incendies_du_16_12_2021_located.csv",delim=";")
MidiPyr<-read_delim("data/feuxmidpyr3_located.csv",delim =";",locale=locale(decimal_mark = "."))
Aquitaine<-read_delim("data/feux_aquitaine_GIPATGRI.txt",delim = ";")
clean_names(Promethee)->Promethee
clean_names(MidiPyr)->MidiPyr
clean_names(Aquitaine)->Aquitaine
Promethee%>%
  rename(burned_area_km2=surface_parcourue_m2)%>%
  mutate(database_origin="liste_incendies_du_16_12_2021_located",
    burned_area_km2=burned_area_km2/1000,
    type_de_feu=as.character(type_de_feu))->Promethee
MidiPyr%>%
  rename(code_insee=commun_code_insee,
         burned_area_km2=surtot,
         date=datdeb,
         burned_area_forest_km2=surfor,
         burned_area_lan_km2=surlan)%>%
  mutate(database_origin="feuxmidpyr3_located",
    long=as.numeric(long),
         numero=as.numeric(numero),
         code_insee=as.character(code_insee),
    origin=as.character(origin))->MidiPyr

PromMidiPyr<-full_join(Promethee,MidiPyr)

Aquitaine%>%
  rename(code_insee=commune,
         long=longitude,
         lat=latitude,
         burned_area_km2=surface_totale_m2,
         date=date_de_premiere_alerte,
         origin=origine,
         burned_area_forest_km2=surface_foret,
         burned_area_lan_km2=surfaces_non_boisees_m2)%>%
  mutate(departement=as.character(departement),
         code_insee=as.character(code_insee),
         burned_area_km2=burned_area_km2/1000,
         burned_area_forest_km2=burned_area_forest_km2/1000,
         burned_area_lan_km2=burned_area_lan_km2/1000,
         date=as.character(date),
         database_origin="feux_aquitaine_GIPATGRI")->Aquitaine

Fire_database<-full_join(PromMidiPyr,Aquitaine)

Fire_database%>%
  filter(!is.na(long))->Fire_database

map<-st_as_sf(Fire_database,coords = c("long","lat"))
st_crs(map)<-4326
tmap_mode("view")


tm_shape(map)+
  tm_dots(col = "database_origin")->map_to_be_saved

  tm_shape(Commune_centroid)+
  tm_dots(shape="type_de_feu",col="blue")->map
tmap_save(map_to_be_saved, "Merged_Fire_Database.html")

Fire_database%>%
  rename(LONG=long,
         LAT=lat)%>%
  select(annee,code_insee,burned_area_km2,LONG,LAT,burned_area_forest_km2,burned_area_lan_km2,everything())->Fire_database
write.table(Fire_database,"Merged_Fire_Database.csv",sep =  ";",fileEncoding = "UTF-8",row.names = F)


#    datdeb=str_replace(datdeb,pattern = "/",replacement = "-"),
# datdeb=str_replace(datdeb,pattern = "/",replacement = "-"))%>%
#   unite("date",datdeb:heurec,sep = " ")%>%
#   mutate(date=as_datetime(date))