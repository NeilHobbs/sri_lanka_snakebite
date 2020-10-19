library(stringr)
library(dplyr)
library(readxl)
hospital_coordinates = read_excel("./Hospitals_SL_update.xlsx")




hospital_coordinates = hospital_coordinates%>%
  rename(Hospital = Name)
#Find the hospitals with the codes before the names.
Hosp_Coords1 = hospital_coordinates%>%
  filter(str_detect(Hospital, "BHA|BHB|DHC|DHA|CD|DH|DHB|DHC|PGH|PMCU"))

#Those that don't have the codes before their names
Hosp_Coords2 = anti_join(hospital_coordinates, Hosp_Coords1)


Hosp_Coords1 = Hosp_Coords1%>%
  mutate(Hospital = sub(".*? ", "", Hosp_Coords1$Hospital))%>% # gets rid of everything before the first space
  mutate(Hospital = str_to_lower(Hospital))

Hosp_Coords2 = Hosp_Coords2%>%
  mutate(Hospital = str_to_lower(Hospital))


hospitals_updated = union(Hosp_Coords1, Hosp_Coords2)


#Import intitial tidied datasets (see Data Tidying file)

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

snakes_df = snakes_df%>%
  ungroup()%>%
  mutate(Hospital = str_to_lower(Hospital))

write.csv(hospitals_updated, ".//hosital_coords.csv")
write.csv(snakes_df, ".//snakebite_data_tidy.csv")


untagged_snakes = geotagged_snakes%>%
  filter(is.na(Hospital.y))

snakes_data = read.csv("./snakebite_data_tidy.csv")
hospital_coordinates = read.csv("hosital_coords.csv")
  


geotagged_snakes = left_join(snakes_data, hospital_coordinates, by = "Hospital")

geotagged_snakes = geotagged_snakes%>%
  select(-"X")

write.csv(geotagged_snakes, ".//geotagged_snakes.csv")


distinct_hospitals = geotagged_snakes%>%
  filter(is.na(x))%>%
  distinct(Hospital)
  




