#Snakebite summary statistics and data visualisation:
library(dplyr)
library(ggplot2)
library(knitr)
library(tmap)

#Note: geotaggged_hospital_snakebite includes specialist and prison/military hospitals
geotagged_hospital_snakebite = geotagged_hospital_snakebite%>%
  mutate(Year = as.factor(Year))%>%
  mutate(Quarter = as.character(Quarter))

#1: The number of cases per year
geotagged_hospital_snakebite%>%
  distinct(Hospital) 

#The total snakebite data has come from a total of 584 Hospitals

deaths = geotagged_hospital_snakebite%>%
  filter(Outcome == "death")

deaths = deaths%>%
  group_by(Year, Quarter)%>%
  summarise(Count = sum(Count))

live.discharges = geotagged_hospital_snakebite%>%
  filter(Outcome == "live.discharge")

live.discharges = live.discharges%>%
  group_by(Year, Quarter)%>%
  summarise(Count = sum(Count))


#Number of Hospital Recorded snakebite deaths per year:
ggplot(deaths, aes(x=Year, y=Count, fill=Quarter)) +
  geom_bar(position = "dodge", stat = "identity")+
  theme_classic() +
  ylab("Number of Snakebite Deaths")


#Number of Hospital Recorded snakebite deaths per year:
ggplot(live.discharges, aes(x=Year, y=Count, fill=Quarter)) +
  geom_bar(position = "dodge", stat = "identity")+
  theme_classic() +
  ylab("Number of Snakebite Live Discharges")


#Number of Hospitals data came from per year:



nrow(snakes2008%>%distinct(Hospital)) #422
nrow(snakes2009%>%distinct(Hospital))#437
nrow(snakes2010%>%distinct(Hospital))#451
nrow(snakes2011%>%distinct(Hospital))#468
nrow(snakes2012%>%distinct(Hospital))#476
nrow(snakes2013%>%distinct(Hospital))#488
nrow(snakes2014%>%distinct(Hospital))#496
nrow(snakes2015%>%distinct(Hospital))#506
nrow(snakes2016%>%distinct(Hospital))#532
nrow(snakes2017%>%distinct(Hospital))#539
nrow(snakes2018%>%distinct(Hospital))#557)

hospitals_per_year = c(422, 437, 451, 468, 476, 488, 496, 506, 532, 539, 557)
years = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

hospitals_per_year = data.frame(hospitals_per_year, years)

#Give output as a table
kable(hospitals_per_year)



