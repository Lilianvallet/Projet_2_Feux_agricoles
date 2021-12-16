library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(readxl)
library(lubridate)
library(xlsx)
library(tmap)
library(tmaptools)
commune_gps <- read_csv2("data/commune_gps_tidied.csv")
#exemple<-read_delim("data/liste_incendies_ du_16_12_2021.csv",delim=";",skip=2)
#exemple<-read_csv2("data/feuxmidpyr3.csv")
exemple<-read_delim("data/feux_aquitaine_GIPATGRI.txt",delim = ";")

  exemple<-clean_names(exemple)
commune_gps %>%
  separate(geo_point_2d, c("LAT", "LONG"), sep = ",") %>%
  mutate(
    LONG = as.numeric(LONG),
    LAT = as.numeric(LAT)
  ) -> commune_gps

for(i in 1:length(exemple$commun_code_insee))
{
  print(i)
  if(!is.na(exemple$commun_code_insee[i])){
    if(str_starts(exemple$commun_code_insee[i],pattern = "0")){
      exemple$commun_code_insee[i]<-str_sub(exemple$commun_code_insee[i],start=2)
    }
  }
}

exemple%>%
  mutate(commune=as.character(commune))->exemple

affected_commune <- left_join(exemple, commune_gps, by = c("commune" = "code_insee"))

affected_commune%>%
  ggplot(aes(x=longitude,y=latitude))+
  geom_point( )
affected_commune%>%
  filter(!is.na(latitude))->data
Fire_centroid<-st_as_sf(data,coords = c("longitude","latitude"))
st_crs(Fire_centroid)<-4326
Commune_centroid<-st_as_sf(data,coords = c("LONG","LAT"))
st_crs(Commune_centroid)<-4326


affected_commune%>%
mutate(difflong=longitude-LONG,
         difflat=latitude-LAT)->tab
plot(tab$difflat)
tab%>%
  filter(difflat>0.2|difflat<(-0.2)|difflong>0.2|difflong<(-0.2))%>%
  ggplot()+
  geom_point(aes(x=longitude,y=latitude))+
  geom_point(aes(x=LONG,y=LAT),col="red")
tmap_mode("view")
tm_shape(Fire_centroid)+
  tm_dots(shape = "type_de_feu",col = "red")+
  tm_shape(Commune_centroid)+
  tm_dots(shape="type_de_feu",col="blue")->map
tmap_save(map, "Aquitaine_coordinates_comparison.html")
#affected_commune%>%filter(is.na(LAT))%>%
#  distinct(commun_code_insee,.keep_all = T)

affected_commune%>%
  filter(is.na(LAT))

write.table(affected_commune,"feuxmidpyr3_located.csv",sep =  ";",fileEncoding = "UTF-8",row.names = F)


               
