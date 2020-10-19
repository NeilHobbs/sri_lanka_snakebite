#Summary Statistics and Simple Data Analysis of Snakebite
library(ggplot2)
library(dplyr)
library(stringr)

#How many unique hospitals has the snakebite data come from (snakes_df obtained from Geotagging Snakebite Hospitalisation script)
snakes_df%>%
  ungroup()%>%
  distinct(Hospital) # gives 584 distinct hospitals.

#Dataframe of deaths only
snake_deaths = snakes_df%>%
  filter(Outcome == "death")

#data frame of live.discharges only
snake_live_discharge = snakes_df%>%
  filter(Outcome == "live.discharge")

#Gives the number of hospitals the data has come from each year.
table(snake_live_discharge$Year)


#Sum the total number of cases (live discharges and deaths) per hospital per year
hospital_admissions_cases = hospital_admissions%>%
  group_by(Year, Hospital)%>%
  summarise(Admissions = sum(Cases))

#Number of Hospitals the Data has come from per year (all hospitals)
Na_Removed_hospital_admissions = hospital_admissions_cases%>%
  filter(!is.na(Admissions))

table(Na_Removed_hospital_admissions$Year)
#Note that there is an increase in the number of hospitals providing data year on year


#Number of hospitals and admissions used spatial analysis (deldir etc): 
Na_Removed_Admissions_Deldir = hospital_admissions_deldir%>%
  filter(!is.na(Admissions))

table(Na_Removed_Admissions_Deldir$Year)


#Which corresponds to total admissions
Na_Removed_Admissions_Deldir%>%
  group_by(Year)%>%
  summarise(sum(Admissions))

#The total Expected snakebite admissions per year
hospital_admissions_deldir%>%
  group_by(Year)%>%
  summarise(sum(Expected_Admissions, na.rm=TRUE))
