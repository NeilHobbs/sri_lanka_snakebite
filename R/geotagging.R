library(dplyr)
library(stringr)


snakebite_admissions = read.csv("snakebites_hospital_per_year.csv")
hospital_coordinates = read.csv("hosital_coords.csv")

snakebite_admissions = snakebite_admissions%>%
  mutate(Hospital = str_to_lower(Hospital))
  
hospital_coordinates = hospital_coordinates%>%
  mutate(Hospital = str_to_lower(Hospital))

geotag_admissions = left_join(snakebite_admissions, hospital_coordinates, by = "Hospital")

geotag_admissions = geotag_admissions%>%
  select(-"X.x", -"X.y")



write.csv(geotag_admissions, ".//geotagged_hospital_admissions.csv")
