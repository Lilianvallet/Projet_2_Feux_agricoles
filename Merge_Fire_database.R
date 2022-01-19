library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(readxl)
library(xlsx)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)

# Diffrents type of database (Promethee, Aquitaine and MidiPyr) ---------------------------------
## Chargement et nettoyage de base de données ----------------------
commune_gps <- read_csv2("data/commune_gps_tidied.csv")
Promethee<-read_delim("data/liste_incendies_du_16_12_2021_located.csv",delim=";")
MidiPyr<-read_delim("data/feuxmidpyr3_located.csv",delim =";",locale=locale(decimal_mark = "."))
Aquitaine<-read_delim("data/feux_aquitaine_GIPATGRI.txt",delim = ";")
Atlas<-as_tibble(read.xlsx("data/Atlas/MissingAndPossibleProblems_17Dec.xlsx",sheetIndex = 3))%>%
  select(Year:Filename)
Atlas_area<-read_csv("data/Atlas/Atlas_Database.csv")
BDIFF<-read_delim("export_BDIFF_incendies_20220111_located.csv",,delim =";",locale=locale(decimal_mark = "."))
clean_names(Promethee)->Promethee
clean_names(MidiPyr)->MidiPyr
clean_names(Aquitaine)->Aquitaine
clean_names(Atlas)->Atlas
clean_names(Atlas_area)->Atlas_area
clean_names(BDIFF)->BDIFF
##Promethee ... ----------------------
Promethee%>%
  rename(burned_area_km2=surface_parcourue_m2)%>%
  mutate(database_origin="liste_incendies_du_16_12_2021_located",
         burned_area_km2=burned_area_km2/1000,
         type_de_feu=as.character(type_de_feu))%>%
  filter(long>0)->Promethee

#... Avec MidiPyr --------------------
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

## ... Avec Aquitaine --------------------
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

## Atlas ensemble ------------------
Atlas<-left_join(Atlas,Atlas_area,by="id")
Atlas%>%
  rename(burned_area_largefire_km2=area_ha)%>%
  mutate(burned_area_largefire_km2=burned_area_largefire_km2*10)->Atlas
## ... Avec Atlas -----------------
Fire_database<-left_join(Fire_database,Atlas,by=c("numero","code_du_carreau_dfci"))

Fire_database%>%
  filter(numero==3278&departement)%>%
  select(burned_area_km2)
Atlas%>%
  filter(code_du_carreau_dfci=="LE80F15")%>%
  select(burned_area_largefire_km2)



Fire_database%>%
  filter(!is.na(burned_area_largefire_km2))->Large_Fire_Database
Large_Fire_Database%>%
  mutate(diff=(burned_area_largefire_km2-burned_area_km2)/(burned_area_largefire_km2),
         x=1:nrow(Large_Fire_Database))->Large_Fire_Database
Large_Fire_Database%>%
  ggplot()+
  aes(x=x)+
  geom_point(aes(y=burned_area_km2))+
  geom_point(aes(y=burned_area_largefire_km2),col=2)+
  geom_linerange(aes(ymin=burned_area_km2,ymax=burned_area_largefire_km2))+
  theme_minimal()

Large_Fire_Database%>%
  filter(diff<(-20000))%>%
  select(diff,x)
Large_Fire_Database%>%
  filter(diff<(-1)| diff>1)%>%
  summarise(mean(diff))
sd(Large_Fire_Database$diff)
# Same type of database (Atlas) -------------------------------------------------------------------
seq_names<-c("AtlasCorse",
             "AtlasDept66",
             "Dept4Atlas31",
             "Dept4Atlas32",
             "Dept5Atlas",
             "Dept6Atlas",
             "Dept7Atlas",
             "Dept11Atlas",
             "Dept26Atlas",
             "Dept30Atlas",
             "Dept48Atlas",
             "Dept83Atlas31",
             "Dept83Atlas32",
             "Dept84Atlas")
seq_departement<-c("2A2B","66","4","4","5","6","7","11","26","30","48","83","83","84")
Atlas_Database<-list()
for(i in 1:14){
  name<-seq_names[i]
  data<-st_read(paste0("data/Atlas/",name,".shp"))
  Atlas_Database[[i]]<-st_transform(data,crs = 4326)
  Atlas_Database[[i]]["departement"]<-seq_departement[i]
}
Atlas_database<-bind_rows(Atlas_Database)
plot(Atlas_database$geometry)
tmap_options(check.and.fix = TRUE)
tmap_mode("view")
tm_shape(Atlas_database)+
  tm_polygons("AreaHa",lwd=0, palette=get_brewer_pal("OrRd", n = 15, contrast = c(0.48, 1)))->map
tmap_save(map,"output_data/Atlas_Database.html")
st_write(Atlas_database,"data/Atlas/Atlas_Database.shp")

tab<-as_tibble(Atlas_database)
write.xlsx2(tab,"Atlas_Database.xlsx",)
st_write(Atlas_database,"data/Atlas/Atlas_Database.csv")




data<-read_csv2("Merged_Fire_Database.csv")
data%>%count(numero)
plot(data$numero)

#Merging option 1 : Promethee + BDIFF -----------------------------
Promethee%>%
  rename(burned_area_km2=surface_parcourue_m2)%>%
  mutate(database_origin="Promethee",
         burned_area_km2=burned_area_km2/1000,
         type_de_feu=as.character(type_de_feu))%>%
  filter(long>0)->Promethee

Promethee%>%
  distinct(code_insee)->Promethee_listcommune

BDIFF%>%
  mutate(long=as.numeric(long),
         lat=as.numeric(lat),
         database_origin="BDIFF")%>%
  filter(long<20)%>%
  filter(!str_detect(Promethee_listcommune,code_insee))->BDIFF_reduced

Fire_database<-full_join(Promethee,BDIFF_reduced)

#Merging option 2 : Promethee + MidiPyr +Aquitaine +BDIFF -----
##Promethee ... ----------------------
Promethee%>%
  rename(burned_area_km2=surface_parcourue_m2)%>%
  mutate(database_origin="Promethee",
         burned_area_km2=burned_area_km2/1000,
         type_de_feu=as.character(type_de_feu))%>%
  filter(long>0)->Promethee

#... Avec MidiPyr --------------------
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

## ... Avec Aquitaine --------------------
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

## ... Avec BDIFF
Promethee%>%
  distinct(code_insee)->Promethee_listcommune
MidiPyr%>%
  distinct(code_insee)->MidiPyr_listcommune
Aquitaine%>%
  distinct(code_insee)->Aquitaine_listcommune
listcommune<-bind_rows(Promethee_listcommune,MidiPyr_listcommune,Aquitaine_listcommune)

BDIFF%>%
  mutate(long=as.numeric(long),
         lat=as.numeric(lat),
         database_origin="BDIFF")%>%
  select(!statut)%>%
  filter(long<20)%>%
  filter(!str_detect(listcommune,code_insee))->BDIFF_reduced

Fire_database<-full_join(Fire_database,BDIFF_reduced)

Fire_database%>%
  filter(!is.na(long))->Fire_database
# Map -------------------------------------------
map<-st_as_sf(Fire_database,coords = c("long","lat"))
st_crs(map)<-4326
tmap_mode("plot")
osm_fire_sf <- read_osm(map, ext = 1.1)
tm_shape(osm_fire_sf) +
  tm_rgb() +
tm_shape(map)+
  tm_dots(col = "database_origin",palette=c("#e41a1c","#377eb8","#4daf4a","#984ea3"))+
  tm_legend(legend.bg.color="white")->map_to_be_saved

tm_shape(Commune_centroid)+
  tm_dots(shape="type_de_feu",col="blue")->map
tmap_save(map_to_be_saved, "Merged_Fire_Database_version2.png")

# Output ------------------------------------
Fire_database%>%
  rename(LONG=long,
         LAT=lat)%>%
  select(annee,code_insee,burned_area_km2,burned_area_largefire_km2,LONG,LAT,burned_area_forest_km2,burned_area_lan_km2,everything())->Fire_database
write.table(Fire_database,"Merged_Fire_Database.csv",sep =  ";",fileEncoding = "UTF-8",row.names = F)

Fire_database%>%
  group_by(database_origin)%>%
  count()
dim(Fire_database)
