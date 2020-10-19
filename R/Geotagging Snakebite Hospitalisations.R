#Required packages
library(stringr)
library(dplyr)
library(readxl)

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

#Use the untagged_2 and tag with google/OSM coords
geotagged_3 = left_join(untagged_2, google_OSM_coords, by ="Hospital")

#Ensure columns correctly named
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


#Found some coordinates where x and y coordinates were switched around
Switched_Coords = Geotagged_Snake_Complete%>%
  filter(y > 10)

Unswitched_Coords = Geotagged_Snake_Complete%>%
  filter(y < 10)

#Correct the names of the switched coordinates columns
Switched_Coords = Switched_Coords%>%
  rename(should_be_x = y)%>%
  rename(should_be_y = x)%>%
  rename(x = should_be_x)%>%
  rename(y = should_be_y)

#And recombine to give the complete and correct dataset
Geotagged_Snake_Complete = rbind(Unswitched_Coords, Switched_Coords)

#Complete data sets of geotagged snakebite hospital admissions
Geotagged_Snake_Complete_DF = Geotagged_Snake_Complete

write.csv(Geotagged_Snake_Complete_DF, ".//Geotagged_Snake_Complete.csv")

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
                                                   "pallekele prison",
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
                                                   "panama base hospital",
                                                   "rehabilitation hospital digana",
                                                   "rehabilitation hospital ragama",
                                                   "lady ridgeway hospital for children"), ]

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
                                                       "pallekele prison",
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
                                                       "panama base hospital",
                                                       "rehabilitation hospital digana",
                                                       "rehabilitation hospital ragama",
                                                       "lady ridgeway hospital for children",
                                                        "anuradhapura prison"), ]

#Remove the specialist hospitals from the analysis
hospitals_for_analysis = anti_join(all_hospital_coordinates_df, specialist_hospitals)
hospitals_for_analysis_df = hospitals_for_analysis


snakebites_for_analysis = anti_join(Geotagged_Snake_Complete_DF, specialist_hospitals_2)
snakebites_for_analysis_df = snakebites_for_analysis

write.csv(snakebites_for_analysis_df, ".//Geotagged_Snakebites_All_Info.csv")

write.csv(snakebites_for_analysis_df, "Geotagged_Snakes_Excluding_Specialists.csv")


Geotagged_Snakebite_Year_Outcome = snakebites_for_analysis_df%>%
  group_by(Hospital, Year, Outcome)%>%
  summarise(Cases = sum(Count))

#retag:
Geotagged_Snakebite_Year_Outcome_XY = left_join(Geotagged_Snakebite_Year_Outcome, all_hospital_coordinates_df)

write.csv(Geotagged_Snakebite_Year_Outcome_XY, ".//Geotagged_Snakebite_Year_Outcome_XY.csv") #has outcome

Geotagged_Snakebite_Year_Outcome_XY_For_Deldir = Geotagged_Snakebite_Year_Outcome_XY%>%
  group_by(Hospital, Year)%>%
  summarise(Admissions = sum(Cases))

hospital_coords_temp = Geotagged_Snakebite_Year_Outcome_XY%>%
  select("Hospital", "x", "y")%>%
  distinct()

Geotagged_Snakebite_Admissions_For_Deldir = left_join(Geotagged_Snakebite_Year_Outcome_XY_For_Deldir, hospital_coords_temp) 


write.csv(Geotagged_Snakebite_Admissions_For_Deldir, ".//Geotagged_Snakebites_For_Deldir.csv")












