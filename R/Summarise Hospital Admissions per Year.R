library(dplyr)

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
  group_by(Hospital, Year, Outcome)%>%
  summarise(Cases = sum(Count))

write.csv(snakes_df, ".//snakebites_hospital_per_year.csv")
