#Required packages
library(stringr)
library(dplyr)
library(readxl)
library(raster)
library(sf)
library(tmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dismo)

#more_hospitals = st_read("./Additional_Data/hotosm_lka_health_facilities_gpkg/hotosm_lka_health_facilities.gpkg")

#Hospital Coordinates supplied by Dileepa - check where they are from
#But have had names updated to match snakebite dataset. Original file still exists.
hospital_coordinates = read_excel("./hospital_coordinates.xlsx")

#make sure all hospital names lower case to enable matching between data sets
hospital_coordinates = hospital_coordinates%>%
  mutate(Hospital = str_to_lower(Hospital))

#Import intitial tidied datasets (see Data Tidying files)
snakes2008 = read.csv("snakes2008.csv")
snakes2009 = read.csv("snakes2009.csv")
snakes2010 = read.csv("snakes2010.csv")
snakes2011 = read.csv("snakes2011.csv")
snakes2012 = read.csv("snakes2012.csv")
snakes2013 = read.csv("snakes2013.csv")
snakes2014 = read.csv("snakes2014.csv")
snakes2015 = read.csv("snakes2015.csv")
snakes2016 = read.csv("snakes2016.csv")
snakes2017 = read.csv("snakes2017.csv")
snakes2018 = read.csv("snakes2018.csv")

#bind the rows of the tidied data sets
snakes_df = rbind(snakes2008, snakes2009, snakes2010,
                  snakes2011, snakes2012, snakes2013,
                  snakes2014, snakes2015, snakes2016,
                  snakes2017, snakes2018)

#Make sure all the hospital names are lower case (to enable matching)
snakes_df = snakes_df%>%
  mutate(Hospital = str_to_lower(Hospital))

geotagged_hospital_snakebite = left_join(snakes_df, hospital_coordinates, by = "Hospital")

#Get only the ones that have been successfully tagged
tagged = geotagged_hospital_snakebite%>%
  filter(!is.na(x))

#Get the ones that were not successfully tagged
untagged = geotagged_hospital_snakebite%>%
  filter(is.na(x))%>%
  dplyr::select(-"x", -"y")


#Coordinates obtained from data.humdata.org, OpenStreetMap Export
other_hospital_coordinates = read.csv("./Other_Hospital_Coordinates.csv")

other_hospital_coordinates = other_hospital_coordinates%>%
  rename(y = X.1)%>%
  rename(x = geom)%>%
  rename(Hospital = name)

#Select only the useful columns
other_hospital_coordinates = other_hospital_coordinates%>%
  dplyr::select("Hospital", "x", "y")%>%
  mutate(Hospital = str_to_lower(Hospital))



#get rid get rid of the c( and ) listlist bits and excess spaces
other_hospital_coordinates = other_hospital_coordinates%>%
  mutate(x = str_remove_all(x, "[c]"))%>%
  mutate(y = str_remove_all(y, "[)]"))%>%
  mutate(x = str_remove_all(x, "[(]"))%>%
  mutate(x = str_remove_all(x, "[listlist]"))%>%
  mutate(x = str_remove(x, "[ ]"))
              
#convert to numeric as they str_remove does not change from character to numeric
other_hospital_coordinates = other_hospital_coordinates%>%
  mutate(x = as.numeric(x))%>%
  mutate(y = as.numeric(y))

write.csv(other_hospital_coordinates, "./other_hospital_coords_update.csv")
#Tag those not tagged in first round, with alternate dataset
geotagged_2 = left_join(untagged, other_hospital_coordinates, by = "Hospital")

#Split into the ones tagged...
tagged_2 = geotagged_2%>%
  filter(!is.na(x))

#...and not tagged
untagged_2 = geotagged_2%>%
  filter(is.na(x))

#Import manually acquired coordinates from OSM and GoogleMaps:
google_coords = read_excel("./Manually Obtained Hospital Coordinates from GoogleMaps.xlsx")
OSM_coords = read_excel("./Manually Obtained Hospital Coordinates from OSM.xlsx")

#bind datasets, and make hospital names lower case
google_OSM_coords = rbind(google_coords, OSM_coords)
google_OSM_coords = google_OSM_coords%>%
  mutate(Hospital = str_to_lower(Hospital))


geotagged_3 = left_join(untagged_2, google_OSM_coords, by ="Hospital")

geotagged_3 = geotagged_3%>%
  rename(x = x.y)%>%
  rename(y = y.y)

tagged_3 = geotagged_3%>%
  filter(!is.na(x))

untagged_3 = geotagged_3%>%
  filter(is.na(x))

untagged_3 = untagged_3%>%
  dplyr::select("Hospital", "Count", "Age", "Quarter", "Sex", "Outcome", "Year")

#Add the New General Hospital, Matara coordinates supplied by Dileepa:
coords_dileepa = read_excel("./coordinates_from_dileepa.xlsx")
coords_dileepa = coords_dileepa%>%
  mutate(Hospital = str_to_lower(Hospital))

geotagged_4 = left_join(untagged_3, coords_dileepa)

tagged_4 = geotagged_4%>%
  filter(!is.na(x))

#Remove the added capital X column to prevent confusion over x coordinate column
tagged = tagged%>%
  dplyr::select(-"X")

tagged = tagged%>%
  mutate(x = as.numeric(x))%>%
  mutate(y = as.numeric(y))

tagged = tagged%>%
  dplyr::select("Hospital", "Count", "Age", "Quarter", "Sex", "Outcome", "Year", "x", "y")

tagged_2 = tagged_2%>%
  dplyr::select("Hospital", "Count", "Age", "Quarter", "Sex", "Outcome", "Year", "x", "y")

tagged_3 = tagged_3%>%
  dplyr::select("Hospital", "Count", "Age", "Quarter", "Sex", "Outcome", "Year", "x", "y")


#Complete data sets of geotagged snakebite hospital admissions
Geotagged_Snake_Complete = rbind(tagged, tagged_2, tagged_3, tagged_4)

Switched_Coords = Geotagged_Snake_Complete%>%
  filter(y > 10)

Unswitched_Coords = Geotagged_Snake_Complete%>%
  filter(y < 10)

Switched_Coords = Switched_Coords%>%
  rename(should_be_x = y)%>%
  rename(should_be_y = x)%>%
  rename(x = should_be_x)%>%
  rename(y = should_be_y)

Geotagged_Snake_Complete = rbind(Unswitched_Coords, Switched_Coords)
Geotagged_Snake_Complete_DF = Geotagged_Snake_Complete

all_hospital_coordinates = Geotagged_Snake_Complete%>%
  dplyr::select("Hospital", "x", "y")%>%
  distinct()
 
all_hospital_coordinates_df = all_hospital_coordinates
 
distinct_hospitals = all_hospital_coordinates_df%>%
  distinct(Hospital)

total_cases_per_hospital = Geotagged_Snake_Complete_DF%>%
  group_by(Hospital)%>%
  summarise(Cases = sum(Count))

#Exclude specialist and military/prison/police hospitals:
specialist_hospitals = all_hospital_coordinates_df[all_hospital_coordinates_df$Hospital %in%
                                                     c("cancer hospital maharagama",
                                                     "dental institute",
                                                     "kethumathi maternity hospital",
                                                   "national dental hospital of sri lanka",
                                                   "national eye hospital",
                                                   "national hospital for respiratory diseases",
                                                   "national institute of mental health",
                                                   "navy hospita - kankasanthurai",
                                                   "navy hospital - boossa",
                                                   "navy hospital - poonawa",
                                                   "ni for nephrology dialysis & transplantation",
                                                   "|pallekele prison",
                                                   "peradeniya dental hospital",
                                                   "welisara chest hospital",
                                                   "navy hospital - trincomalee",
                                                   "chest clinic",
                                                   "police hospital",
                                                   "castle street hospital for women",
                                                   "de soysa hospital for women",
                                                   "mahara prison",
                                                   "prison hospital",
                                                   "military"), ]

specialist_hospitals_2 = Geotagged_Snake_Complete_DF[Geotagged_Snake_Complete_DF$Hospital %in%
                                                     c("cancer hospital maharagama",
                                                       "dental institute",
                                                       "kethumathi maternity hospital",
                                                       "national dental hospital of sri lanka",
                                                       "national eye hospital",
                                                       "national hospital for respiratory diseases",
                                                       "national institute of mental health",
                                                       "navy hospita - kankasanthurai",
                                                       "navy hospital - boossa",
                                                       "navy hospital - poonawa",
                                                       "ni for nephrology dialysis & transplantation",
                                                       "|pallekele prison",
                                                       "peradeniya dental hospital",
                                                       "welisara chest hospital",
                                                       "navy hospital - trincomalee",
                                                       "chest clinic",
                                                       "police hospital",
                                                       "castle street hospital for women",
                                                       "de soysa hospital for women",
                                                       "mahara prison",
                                                       "prison hospital",
                                                       "military",
                                                      "anuradhapura prison",
                                                      "lady ridgeway hospital for children"), ]

hospitals_for_analysis = anti_join(all_hospital_coordinates_df, specialist_hospitals)
hospitals_for_analysis_df = hospitals_for_analysis

snakebites_for_analysis = anti_join(Geotagged_Snake_Complete_DF, specialist_hospitals_2)
snakebites_for_analysis_df = snakebites_for_analysis

write.csv(snakebites_for_analysis_df, ".//Geotagged_Snakebites_All_Info.csv")

Geotagged_Snakebite_Year_Outcome = snakebites_for_analysis_df%>%
  group_by(Hospital, Year, Outcome)%>%
  summarise(Cases = sum(Count))

#retag:
Geotagged_Snakebite_Year_Outcome_XY = left_join(Geotagged_Snakebite_Year_Outcome, all_hospital_coordinates_df)

write.csv(Geotagged_Snakebite_Year_Outcome_XY, ".//Geotagged_Snakebite_Year_Outcome_XY.csv")

coordinates_only = hospitals_for_analysis%>%
  dplyr::select(-"Hospital")


#Import snakebite incidence raster of Sri Lanka
snakebite_raster = raster("./Snakebite_map_raster.tif")

#turn geotagged hospitals in a spatial features coordinates for easier plotting
coordinates(Geotagged_Snake_Complete) = ~x+y
coordinates(hospitals_for_analysis) = ~x+y

#Plot the raster and hospital points over one another.
plot(snakebite_raster)
points(hospitals_for_analysis)

library(deldir)

#Make voronoi polygons of hospital territories:
z = deldir(hospitals_for_analysis$x, hospitals_for_analysis$y)

# then the tiles.
z_polygon = z$delsgs

w = tile.list(z)

#Make as a polygon for easier data handling
hospital_territories <- vector(mode="list", length=length(w))
for (i in seq(along=hospital_territories)) {
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  hospital_territories[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}

#make as spatial polygons and dataframes
hospital_territories_spatpoly = SpatialPolygons(hospital_territories)
hospital_territories_spatpolydf = SpatialPolygonsDataFrame(hospital_territories_spatpoly, data = hospitals_for_analysis_df)

cropped_territories = st_crop(hospital_territories_spatpoly, sri_lanka_border)

plot(snakebite_raster)
lines(hospital_territories_spatpoly)

sri_lanka_border = st_read("./Additional_Data/lka_adm_shp/lka_admbnda_adm0_slsd_20200305.shp")

incidence_risk = extract(snakebite_raster, hospital_territories_spatpolydf, mean, na.rm=TRUE)
hospital_territories_spatpolydf$bite_incidence = incidence_risk[, 1]

#Population density map seems to be acting very weirdly
population_density = raster("./popluation_density__raster_from_sex_distribution_wgs84.tif")

extracted_pop_density = extract(population_density, hospital_territories_spatpolydf, fun=sum, na.rm=TRUE)
hospital_territories_spatpolydf$population_size = extracted_pop_density[, 1]

pop_size = hospital_territories_spatpolydf$population_size
snake_incidence = hospital_territories_spatpolydf$bite_incidence
expected_snakebites = pop_size*snake_incidence
hospital_territories_spatpolydf$expected_cases = expected_snakebites
hospital_territories_df = data.frame(hospital_territories_spatpolydf)

actual_cases = snakebites_for_analysis_df%>%
  group_by(Hospital, Year)%>%
  summarise(Admissions = sum(Count))

expected_actual = left_join(hospital_territories_df, actual_cases)

expected_actual = expected_actual%>%
  filter(!is.na(bite_incidence))%>%
  ungroup()


library(ggplot2)

ggplot(expected_actual, aes(x=Admissions, y=expected_cases))+
  geom_point() +
  facet_wrap(~Year)


#Alternative Population density data from WorldPop:
population_density_2008 = raster("./Additional_Data/lka_ppp_2008.tif")
populaiton_density_2009 = raster("./Additional_Data/lka_ppp_2009.tif")
population_density_2010 = raster("./Additional_Data/lka_ppp_2010.tif")
population_density_2011 = raster("./Additional_Data/lka_ppp_2011.tif")
population_density_2012 = raster("./Additional_Data/lka_ppp_2012.tif")
population_density_2013 = raster("./Additional_Data/lka_ppp_2013.tif")
population_density_2014 = raster("./Additional_Data/lka_ppp_2014.tif")
population_density_2015 = raster("./Additional_Data/lka_ppp_2015.tif")
population_density_2016 = raster("./Additional_Data/lka_ppp_2016.tif")
population_density_2017 = raster("./Additional_Data/lka_ppp_2017.tif")
population_density_2018 = raster("./Additional_Data/lka_ppp_2018.tif")

#Extract population data for each hospital territory (needs to be the total, removing NAs)
population_per_territory_2008 = extract(population_density_2008, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2009 = extract(population_density_2009, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2010 = extract(population_density_2010, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2011 = extract(population_density_2011, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2012 = extract(population_density_2012, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2013 = extract(population_density_2013, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2014 = extract(population_density_2014, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2015 = extract(population_density_2015, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2016 = extract(population_density_2016, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2017 = extract(population_density_2017, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
population_per_territory_2018 = extract(population_density_2018, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)

hospital_territories_spatpolydf$population_2008 = population_per_territory_2008[ ,1]
hospital_territories_spatpolydf$population_2009 = population_per_territory_2009[ ,1]
hospital_territories_spatpolydf$population_2010 = population_per_territory_2010[ ,1]
hospital_territories_spatpolydf$population_2011 = population_per_territory_2011[ ,1]
hospital_territories_spatpolydf$population_2012 = population_per_territory_2012[ ,1]
hospital_territories_spatpolydf$population_2013 = population_per_territory_2013[ ,1]
hospital_territories_spatpolydf$population_2014 = population_per_territory_2014[ ,1]
hospital_territories_spatpolydf$population_2015 = population_per_territory_2015[ ,1]
hospital_territories_spatpolydf$population_2016 = population_per_territory_2016[ ,1]
hospital_territories_spatpolydf$population_2017 = population_per_territory_2017[ ,1]
hospital_territories_spatpolydf$population_2018 = population_per_territory_2018[ ,1]












