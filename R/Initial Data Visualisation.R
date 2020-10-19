#Required Packages
library(dplyr)
library(ggplot2)
library(knitr)
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
  select(-"X", -"Age")%>%
  mutate(Year = as.character(Year))%>%
  mutate(Cases = as.numeric(Count))%>%
  mutate(Quarter = as.factor(Quarter)) #Prevents being treated as a continuous variable

snakes_df = snakes_df%>%
  group_by(Hospital, Year, Outcome)%>%
  summarise(Cases = sum(Cases))


#Total Hospitals per year

#Total Recorded Bites per year
sum(snakes_df$Cases)

snake_cases_year= snakes_df%>%
  group_by(Year, Outcome)%>%
  summarise(Cases = sum(Cases))

# sum(snakes2008$Count) #38381, Match totals provided by MSU
# sum(snakes2009$Count) #39813, Match totals provided by MSU
# sum(snakes2010$Count) #42234, Match totals provided by MSU
# sum(snakes2011$Count) #42308, Match totals provided by MSU
# sum(snakes2012$Count) #41538, Match totals provided by MSU
# sum(snakes2013$Count) #40468, Match totals provided by MSU
# sum(snakes2014$Count) #37309, Match totals provided by MSU
# sum(snakes2015$Count) #36631, Match total provided by MSU
# sum(snakes2016$Count) #34494, Match total provided by MSU
# sum(snakes2017$Count) #31361, Matches total provided by MSU
# sum(snakes2018$Count) #31847, Matches total provided by MSU





#Get case totals grouped by Year, Hospital
hospital_year_df = snakes_df %>% 
  group_by(Year, Hospital, Outcome) %>% 
  summarise(Cases = sum(Cases))

#Total of each outcome per year.
hospital_year_df%>%
  group_by(Outcome)%>%
  summarise(Cases = sum(Cases))

#Totals Calculated as percentages:
892/416384 * 100 #all deaths
415492/416384 * 100 #all discharges


snakes_df%>%
  group_by(Sex, Outcome)%>%
  summarise(Cases = sum(Cases))
  
#Death distributions
564/892 * 100 #Male deaths (% of all deaths)
251770/(251770 + 163722)*100 #Male discarges as % all discharges


#deaths per year
snake_deaths_df = snakes_df%>%
  filter(Outcome == "death")%>%
  filter(Cases > 0)

max(snake_deaths_df$Cases)

snake_discharges = snakes_df%>%
  filter(Outcome =="live.discharge")

max(snake_discharges$Cases)

ggplot(snakes_df, aes(x=Year, y=Cases, colour = Outcome)) +
  geom_point(alpha = 0.3)+
  facet_wrap(~Outcome) +
  theme_classic() +
  ggtitle("Number of snakebite cases in each hospital per year")


#Plot snakebite deaths of occurances where there are deaths
ggplot(snake_deaths_df, aes(x=Hospital, y=Cases, colour = Year)) +
  geom_point(alpha = 0.4)+
  ylab("Number of Snakebite Deaths")+
  ggtitle("Snakebite Deaths in Each Hospital") +
  theme(axis.text.x = element_text(angle = 90, size = 4))
  

