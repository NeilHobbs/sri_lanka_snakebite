#Map Data Visualisation and Map Making:
library(dplyr)
library(sf)
library(raster)
library(deldir)
library(tmap)
library(rgdal)
library(ggplot2)
#Visualise what's going on:


#Keep things visualised to 2013 for the time being:
hospital_admissions_all_years_all_vars = read.csv("./hospital_admissions_all_vars.csv")

#2013 best covers the year of the snakebite survey the incidence was generated from
hospital_admissions_2013_all_vars = hospital_admissions_all_years_all_vars%>%
  filter(Year == 2013)

#1. Histogram and/or Boxplot of Admissions stratified by hospital care_level
      #Histogram
ggplot(hospital_admissions_2013_all_vars, aes(x=Admissions))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~Care_Level)
  
    #Boxplot:



snakebite_raster_map = tm_shape(snakebite_raster) +tm_raster()
sri_lanka_map = tm_shape(sri_lanka_border) +tm_borders()
hospital_territories_map = tm_shape(hospital_territories_spatpolydf) +tm_borders()

final_map = snakebite_raster_map + hospital_territories_map + sri_lanka_map
final_map

plot_territories_map = function(hosp.adm.year.spatpolydf, year, TorF){
  
  territories.map = tm_shape(hosp.adm.year.spatpolydf) + tm_fill("Difference", 
                                                                 breaks = c(-1400, -1200, -1000, -800, -600,
                                                                            -400, -200, 0, 200,
                                                                            400, 600, 800), c) +
    tm_layout(title = year, legend.show = TorF, legend.outside = TorF) + tm_borders() +
    sri_lanka_map
  
  return(territories.map)
}

HA_2008_map = plot_territories_map(hospital_admissions_2008_SpatPolyDF, 2008, TRUE)
HA_2009_map = plot_territories_map(hospital_admissions_2009_SpatPolyDF, 2009, FALSE)
HA_2010_map = plot_territories_map(hospital_admissions_2010_SpatPolyDF, 2010, FALSE)
HA_2011_map = plot_territories_map(hospital_admissions_2011_SpatPolyDF, 2011, FALSE)
HA_2012_map = plot_territories_map(hospital_admissions_2012_SpatPolyDF, 2012, TRUE)
HA_2013_map = plot_territories_map(hospital_admissions_2013_SpatPolyDF, 2013, FALSE)
HA_2014_map = plot_territories_map(hospital_admissions_2014_SpatPolyDF, 2014, FALSE)
HA_2015_map = plot_territories_map(hospital_admissions_2015_SpatPolyDF, 2015, FALSE)
HA_2016_map = plot_territories_map(hospital_admissions_2016_SpatPolyDF, 2016, TRUE)
HA_2017_map = plot_territories_map(hospital_admissions_2017_SpatPolyDF, 2017, FALSE)
HA_2018_map = plot_territories_map(hospital_admissions_2018_SpatPolyDF, 2018, FALSE)

tmap_arrange(HA_2008_map, HA_2009_map,
             HA_2010_map, HA_2011_map, 
             ncol = 2, nrow=2)

tmap_arrange(HA_2012_map, HA_2013_map,
             HA_2014_map, HA_2015_map,
             ncol = 2, nrow =2)

tmap_arrange(HA_2016_map, HA_2017_map,
             HA_2018_map, ncol = 2,
             nrow =2)

hospital_admissions_deldir = hospital_admissions_deldir%>%
  mutate(Year = as.character(Year))


ggplot(hospital_admissions_deldir, aes(x=Admissions ,y=Expected_Admissions, colour = Year)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, colour = "red")+
  ylab("Expected Snakebite Hospital Admissions") +
  xlab("Actual Snakebite Hospital Admissions") +
  xlim(0, 1500)+
  ylim(0, 1500)+
  facet_wrap(~Year)+
  theme_classic()


plot_territories_proportion_map = function(hosp.adm.year.spatpolydf){
  
  territories.map = tm_shape(hosp.adm.year.spatpolydf) + tm_fill("Proportion_Turning_Up",
                                                                 breaks= c(0, 0.1, 0.2, 0.3, 0.4,
                                                                           0.5, 0.6, 0.7, 0.8, 0.9,
                                                                           1, 2, 10, 50))+tm_borders()+
    sri_lanka_map
  return(territories.map)                                                          
}

plot_territories_proportion_map(hospital_admissions_2008_SpatPolyDF)



plot_admissions_map = function(hosp.adm.year.spatpolydf, year, TorF){
  
  territories.map = tm_shape(hosp.adm.year.spatpolydf) + tm_fill("Admissions", 
                                                                 breaks = c(0, 10, 50, 100, 200,
                                                                            500, 1000, 2000)) +
    tm_layout(title = year, legend.show = TorF, legend.outside = TorF) + tm_borders() +
    sri_lanka_map
  
  return(territories.map)
}


admissions_2008_map = plot_admissions_map(hospital_admissions_2008_SpatPolyDF, 2008, TRUE)
admissions_2009_map = plot_admissions_map(hospital_admissions_2009_SpatPolyDF, 2009, FALSE)
admissions_2010_map = plot_admissions_map(hospital_admissions_2010_SpatPolyDF, 2010, FALSE)
admissions_2011_map = plot_admissions_map(hospital_admissions_2011_SpatPolyDF, 2011, FALSE)
admissions_2012_map = plot_admissions_map(hospital_admissions_2012_SpatPolyDF, 2012, FALSE)
admissions_2013_map = plot_admissions_map(hospital_admissions_2013_SpatPolyDF, 2013, TRUE)
admissions_2014_map = plot_admissions_map(hospital_admissions_2014_SpatPolyDF, 2014, FALSE)
admissions_2015_map = plot_admissions_map(hospital_admissions_2015_SpatPolyDF, 2015, FALSE)
admissions_2016_map = plot_admissions_map(hospital_admissions_2016_SpatPolyDF, 2016, FALSE)
admissions_2017_map = plot_admissions_map(hospital_admissions_2017_SpatPolyDF, 2017, FALSE)
admissions_2018_map = plot_admissions_map(hospital_admissions_2018_SpatPolyDF, 2018, FALSE)

Admissions_2013_Map = tm_shape(hospital_admissions_2013_SpatPolyDF)
+ tm_fill("Expected_Admissions", 
          breaks = c(0, 10, 50, 100, 200,
                     500, 1000, 2000)) +
  tm_borders() +
  sri_lanka_map
Expected_Admissions_2013_Map = tm_shape(hospital_admissions_2013_SpatPolyDF)
+ tm_fill("Expected_Admissions", 
          breaks = c(0, 10, 50, 100, 200,
                     500, 1000, 2000)) +
  tm_borders() +
  sri_lanka_map

library(RColorBrewer)

Difference_2013_Map = tm_shape(hospital_admissions_2013_SpatPolyDF)+
  tm_fill("Difference", palette = "RdYlGn",
          breaks = c(-1200,-600, -400, -200, -100,
                     0, 100, 200, 600)) +
  tm_borders() +
  sri_lanka_map


Admissions_2013_Map = tm_shape(hospital_admissions_2013_SpatPolyDF)+
  tm_fill("Admissions", palette = "RdYlGn",
          breaks = c(0, 10, 50,
                     100) +
            tm_borders() +
            sri_lanka_map
          
          range(hospital_admissions_2013_SpatPolyDF$Admissions, na.rm = TRUE)
          
          admissions_2013_map#Actual Admissions
          Difference_2013_Map #Difference
          EA_2013_map #Expected_Admissions
          
tmap_arrange(admissions_2013_map, HA_2013_map, EA_2013_map)
          