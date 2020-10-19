#Data Visualisation and Bascic Analysis of Snakebite Data
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tmap)
library(RColorBrewer)

#All hospitals (including specialists)
all_hospital_admissions_df = read.csv("./Geotagged_Snake_Complete.csv")

#Hospitals per Year reporting to MSU
grouped_df =all_hospital_admissions_df%>%
  group_by(Year, Hospital)%>%
  summarise(Admissions = sum(Count))%>%
  mutate(Year = as.factor(as.character(Year)))
table(grouped_df$Year)

#Hospitals per year that could be used for spatial analysis
hospital_admissions_all_years_all_vars_noAdNa = hospital_admissions_all_years_all_vars%>%
  filter(!is.na(Admissions))
table(hospital_admissions_all_years_all_vars_noAdNa$Year)


#Hospital Admissions (with live discharge) per year
all_hospital_admissions_ld = all_hospital_admissions_df%>%
  filter(Outcome == "live.discharge")%>%
  mutate(Year = as.factor(as.character(Year)))%>%
  group_by(Hospital, Year)%>%
  summarise(Discharges = sum(Count))

ggplot(data = all_hospital_admissions_ld, aes(x=Year, y=Discharges, group = Year)) +
  geom_col(fill = "lightblue")+
  geom_label(data = hospital_per_year, aes(label = hospital_per_year, x=Year))+
  ylab("Number of Snakebite Life Discharges") +
  theme_bw()



all_hospital_admissions_deaths= all_hospital_admissions_df%>%
  filter(Outcome == "death")%>%
  group_by(Hospital, Year)%>%
  summarise(Count = sum(Count))
s
ggplot(data = all_hospital_admissions_ld, aes(x=Year, y=Count, group = Year)) +
  geom_col(fill = "lightblue")+
  geom_label(data = hospital_per_year, aes(label = hospital_per_year, x=Year))+
  ylab("Number of Snakebite Deaths") +
  theme_bw()


#Only Hospitals used in Spatial Analysis:
hospital_admissions_all_years_all_vars = read.csv("./hospital_admissions_all_vars.csv")


table(temp$Year)

#Plot Deaths per Year/Hospital

hospital_admissions_all_years_all_vars%>%
  group_by(Year)%>%
  summarise(Count = sum(Admissions, na.rm = TRUE))

####2013 Dataset used in Spatial Analysis#####
#Read in dataset (post deldir; with expected admissions, altitude, climate and agriculture, hospital level etc)
hospital_admissions_all_years_all_vars = read.csv("./hospital_admissions_all_vars.csv")

#2013 best covers the year of the snakebite survey the incidence was generated from
hospital_admissions_2013_all_vars = hospital_admissions_all_years_all_vars%>%
  filter(Year == 2013)

#Remove all the NAs from the Hospital Admissions Data to prevent errors occurring later on.
hospital_admissions_2013_noNA = hospital_admissions_2013_all_vars%>%
  filter(!is.na(Admissions))%>%
  filter(!is.na(Expected_Admissions))%>%
  filter(!is.na(Altitude))%>%
  filter(!is.na(Climatic_Zone))%>%
  filter(!is.na(Agry))%>%
  filter(!is.na(population_density))%>%
  mutate(Climatic_Zone = as.factor(Climatic_Zone))

AltVSAdmis = ggplot(hospital_admissions_2013_noNA, aes(y = Admissions, x = Altitude)) +
  geom_point() +
  theme_classic()+
  xlab("Mean Altitude of Hospital Catchment (m)") +
  ylab(" ")+
  ggtitle("A")

RiskVSAdmis = ggplot(hospital_admissions_2013_noNA, aes(y = Admissions, x = incidence_risk)) +
  geom_point(col = "red") +
  theme_classic()+
  xlab("Mean Snakebite Incidence of Hospital Catchment (m)") +
  ylab(" ")+
  ggtitle("B")

PopSizeVSAdmis = ggplot(hospital_admissions_2013_noNA, aes(y = Admissions, x = population_size)) +
  geom_point(col = "orange") +
  theme_classic()+
  xlab("Population Size of Hospital Catchment (m)") +
  ylab(" ")+
  scale_x_continuous(labels = scales::comma)+
  ggtitle("C")

PopDenseVSAdmis = ggplot(hospital_admissions_2013_noNA, aes(y = Admissions, x = population_density)) +
  geom_point(col = "blue") +
  theme_classic()+
  xlab("Population Density of Hospital Catchment per Square km") +
  ylab(" ")+
  scale_x_continuous(labels = scales::comma)+
  ggtitle("D")

ClimZoneVSAdmis = ggplot(hospital_admissions_2013_noNA, aes(y = Admissions, x = Climatic_Zone)) +
  geom_boxplot() +
  theme_classic()+
  xlab("Climatic Zone") +
  ylab("                               Number of Snakebite Admissions")+ 
  scale_x_discrete(labels = c('Dry','Intermediate','Wet'))+
  ggtitle("E")

AgryVSAdmis = ggplot(hospital_admissions_2013_noNA, aes(y = Admissions, x = Agry)) +
  geom_point(col = "darkgreen") +
  theme_classic()+
  xlab("Proportion of Agricultural Workers in Hospital Catchment") +
  ylab(" ")+
  ggtitle("F")

HosTypeVSAdmis = ggplot(hospital_admissions_2013_noNA, aes(y = Admissions, x = Hospital_Type)) +
  geom_boxplot() +
  theme_classic()+
  xlab("Hospital Type") +
  ylab("")+
  ggtitle("G")

CareLevVSAdmis = ggplot(hospital_admissions_2013_noNA, aes(y = Admissions, x = Care_Level)) +
geom_boxplot() +
  theme_classic()+
  xlab("Hospital Care Level") +
  ylab(" ")+
  ggtitle("H")

grid.arrange(AltVSAdmis,
             RiskVSAdmis,
             PopSizeVSAdmis,
             PopDenseVSAdmis,
             ClimZoneVSAdmis,
             AgryVSAdmis,
             HosTypeVSAdmis,
             CareLevVSAdmis,
             nrow = 4)


#Histogram of Hospital Admissions:
ggplot(hospital_admissions_2013_noNA, aes(x=Admissions))+
  geom_histogram(binwidth = 10)





#Locations of Hospitals and Hospital Territories:
tm_shape(hospital_admissions_2013_SpatPolyDF) +tm_fill("Care_Level") +tm_borders()+
  tm_shape(sri_lanka_border) +tm_borders()

snakebite_raster_map = tm_shape(snakebite_raster) +tm_raster()
sri_lanka_map = tm_shape(sri_lanka_border) +tm_borders()
hospital_territories_map = tm_shape(hospital_territories_spatpolydf) +tm_borders()

final_map = snakebite_raster_map + hospital_territories_map + sri_lanka_map
final_map


#Hospital Admissions Data Set
hospital_admissions_2013_CAA_Mapping = hospital_admissions_2013_CAA
coordinates(hospital_admissions_2013_CAA_Mapping) = ~x+y

#Agricultural Workers Map:
Agry_Map = tm_shape(hospital_admissions_2013_SpatPolyDF) +tm_fill(col = "Agry", palette = "Greens") + tm_borders(col = "black")+
  tm_shape(hospital_admissions_2013_CAA_Mapping) + tm_dots() +
    tm_shape(sri_lanka_border) + tm_borders(lwd = 2)
Agry_Map
#Altitude Map
Altitude_Map = tm_shape(hospital_admissions_2013_SpatPolyDF) +
  tm_fill(col = "Altitude", palette = "Greys", breaks = c(0, 600, 2000)) + 
  tm_borders(col = "black")+
  tm_shape(hospital_admissions_2013_CAA_Mapping) + tm_dots() +
  tm_shape(sri_lanka_border) + tm_borders(lwd = 2)
Altitude_Map
#Care Level Map
Care_Level_Map = tm_shape(hospital_admissions_2013_SpatPolyDF) +
  tm_fill(col = "Care_Level", palette = "Dark2") + tm_borders(col = "black")+
  tm_shape(hospital_admissions_2013_CAA_Mapping) + tm_dots() +
  tm_shape(sri_lanka_border) + tm_borders(lwd = 1, col = "white")
Care_Level_Map

#incidence risk map
Incidence_Risk_Map = tm_shape(hospital_admissions_2013_SpatPolyDF) +
  tm_fill(col = "bite_incidence", pallete = "Reds") + 
      tm_borders(col = "black") +
        tm_shape(sri_lanka_border) + tm_borders(lwd = 1, col = "black")

#population size map
Population_Size_Map = tm_shape(hospital_admissions_2013_SpatPolyDF) +
  tm_fill(col = "population_size", 
          pallete = "Blues", 
          breaks = c(0, 50000, 100000, 150000, 200000, 250000,
                     300000, 350000, 400000, 450000, 500000)) + 
  tm_borders(col = "black") +
  tm_shape(sri_lanka_border) + tm_borders(lwd = 1, col = "black")
Population_Size_Map

#population density map
hospital_admissions_2013_SpatPolyDF$
#Admissions map





