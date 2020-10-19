#Spatial Analysis:
library(dplyr)
library(sf)
library(raster)
library(deldir)
library(tmap)
library(rgdal)
library(ggplot2)

#Import snakebite incidence raster of Sri Lanka, from PLoS NTDs Paper
snakebite_raster = raster("./Snakebite_map_raster.tif")

#Import geotagged hospital admissions:
hospital_admissions = read.csv("./Geotagged_Snakebites_For_Deldir.csv")

#Just have data frame of hospital name and coordinates
hospital_coordinates_df = hospital_admissions%>%
  dplyr::select("Hospital", "x", "y")%>%
  dplyr::distinct()

#to turn to spatial data frame
hospital_coordinates = hospital_admissions%>%
  dplyr::select("Hospital", "x", "y")%>%
  dplyr::distinct()

#check that hospitals look right over snakebite raster:
coordinates(hospital_coordinates) = ~x+y

plot(snakebite_raster)
points(hospital_coordinates) #Nothing appears to be plotting noticeable oddly.

#Calculate the territories using the deldir package: assume people will go their 
#nearest hospital (in a straight line)
calc_hosp_territories = deldir(hospital_coordinates$x, hospital_coordinates$y)

# then the tiles.
territory_polygon = calc_hosp_territories$delsgs

tile_territory_polygon = tile.list(calc_hosp_territories)

#Make as a polygon for easier data handling
hospital_territories = vector(mode="list", length=length(tile_territory_polygon))
for (i in seq(along=hospital_territories)) {
  pcrds <- cbind(tile_territory_polygon[[i]]$x, tile_territory_polygon[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  hospital_territories[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
}

#make as spatial polygons and dataframes
hospital_territories_spatpoly = SpatialPolygons(hospital_territories)
hospital_territories_spatpolydf = SpatialPolygonsDataFrame(hospital_territories_spatpoly, data = hospital_coordinates_df)

sri_lanka_border = st_read("./Additional_Data/lka_adm_shp/lka_admbnda_adm0_slsd_20200305.shp")

plot(snakebite_raster)#underlying map
lines(hospital_territories_spatpoly)#hospital territories
points(hospital_coordinates) #points of hospitals

plot(sri_lanka_border$geometry)
points(hospital_coordinates)

tm_shape(sri_lanka_border) + tm_fill("darkgreen") + tm_borders(lwd = 2)+
  tm_shape(hospital_coordinates) + tm_dots(col = "black") +
    tm_shape(hospital_territories_spatpolydf) + tm_borders(col = "black")

crs(snakebite_raster)
#Have same crs as the raster files.
crs(hospital_territories_spatpolydf) = CRS("+proj=longlat +datum=WGS84 +no_defs")

#Calculate the mean (fun=mean) snakebite incidence in each hospital territory
incidence_risk = raster::extract(snakebite_raster, hospital_territories_spatpolydf, fun=mean, na.rm=TRUE)

      
#Add snake risk to dataset
hospital_territories_spatpolydf$bite_incidence = incidence_risk[, 1]

hospital_territories_df = data.frame(hospital_territories_spatpolydf)
      #Note some are still have NaN as the incidence_risk 


#Use population count from WorldPop
population_count_2008 = raster("./Additional_Data/lka_ppp_2008.tif")
population_count_2009 = raster("./Additional_Data/lka_ppp_2009.tif")
population_count_2010 = raster("./Additional_Data/lka_ppp_2010.tif")
population_count_2011 = raster("./Additional_Data/lka_ppp_2011.tif")
population_count_2012 = raster("./Additional_Data/lka_ppp_2012.tif")
population_count_2013 = raster("./Additional_Data/lka_ppp_2013.tif")
population_count_2014 = raster("./Additional_Data/lka_ppp_2014.tif")
population_count_2015 = raster("./Additional_Data/lka_ppp_2015.tif")
population_count_2016 = raster("./Additional_Data/lka_ppp_2016.tif")
population_count_2017 = raster("./Additional_Data/lka_ppp_2017.tif")
population_count_2018 = raster("./Additional_Data/lka_ppp_2018.tif")

#Input: the population density raster and the year of that raster
calculate_pop_size = function(pop.count){
  #Calculate the total population size of each hospital territory
  pop.per.territory = raster::extract(pop.count, hospital_territories_spatpolydf, fun = sum, na.rm = TRUE)
  
  return(pop.per.territory)
}

calculate_expected_cases = function(pop.vector, year, pop.dense){
  #   #Have only the year that matches the population raster
 hosp.adm.year = hospital_admissions%>%
     filter(Year == year)

   hospital_coordinates_df$incidence_risk = incidence_risk[, 1] #Add snakebite incidence to hospital coordinates
   hospital_coordinates_df$population_size = pop.vector[, 1] #Add the population of each territory
   hospital_coordinates_df$population_density = pop.dense[, 1]#Add the population density of each territory
   #Match
   final.df = left_join(hospital_coordinates_df, hosp.adm.year)
   
   final.df = final.df%>%
     mutate(Year = year)%>%
     dplyr::select(-"X")%>%
     mutate(Expected_Admissions = incidence_risk * population_size)%>%
     mutate(Difference = Expected_Admissions - Admissions)%>%
     mutate(Proportion_Turning_Up = Admissions / Expected_Admissions)

   return(final.df)
}

#Read in population density raster files
population_density_2008 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2009 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2010 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2011 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2012 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2013 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2014 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2015 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2016 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2017 = raster("./Additional_Data/lka_pd_2013_1km.tif")
population_density_2018 = raster("./Additional_Data/lka_pd_2013_1km.tif")

calculate_pop_density = function(pop.density){
  #Calculate the total population size of each hospital territory
  dense.per.territory = raster::extract(pop.density, hospital_territories_spatpolydf, fun = mean, na.rm = TRUE)
  
  return(dense.per.territory)
}



Pop2008 = calculate_pop_size(population_count_2008)
PopDense2008 = calculate_pop_density(population_density_2008)
hospital_admissions_2008 = calculate_expected_cases(Pop2008, 2008, PopDense2008)

Pop2009 = calculate_pop_size(population_count_2009)
PopDense2009 = calculate_pop_density(population_density_2009)
hospital_admissions_2009 = calculate_expected_cases(Pop2009, 2009, PopDense2009)

Pop2010 = calculate_pop_size(population_count_2010)
PopDense2010 = calculate_pop_density(population_density_2010)
hospital_admissions_2010 = calculate_expected_cases(Pop2010, 2010, PopDense2010)

Pop2011 = calculate_pop_size(population_count_2011)
PopDense2011 = calculate_pop_density(population_density_2011)
hospital_admissions_2011 = calculate_expected_cases(Pop2011, 2011, PopDense2011)

Pop2012 = calculate_pop_size(population_count_2012)
PopDense2012 = calculate_pop_density(population_density_2012)
hospital_admissions_2012 = calculate_expected_cases(Pop2012, 2012, PopDense2012)

Pop2013 = calculate_pop_size(population_count_2013)
PopDense2013 = calculate_pop_density(population_density_2013)
hospital_admissions_2013 = calculate_expected_cases(Pop2013, 2013, PopDense2013)

Pop2014 = calculate_pop_size(population_count_2014)
PopDense2014 = calculate_pop_density(population_density_2014)
hospital_admissions_2014 = calculate_expected_cases(Pop2014, 2014, PopDense2014)

Pop2015 = calculate_pop_size(population_count_2015)
PopDense2015 = calculate_pop_density(population_density_2015)
hospital_admissions_2015 = calculate_expected_cases(Pop2015, 2015, PopDense2015)

Pop2016 = calculate_pop_size(population_count_2016)
PopDense2016 = calculate_pop_density(population_density_2016)
hospital_admissions_2016 = calculate_expected_cases(Pop2016, 2016, PopDense2016)

Pop2017 = calculate_pop_size(population_count_2017)
PopDense2017 = calculate_pop_density(population_density_2017)
hospital_admissions_2017 = calculate_expected_cases(Pop2017, 2017, PopDense2017)

Pop2018 = calculate_pop_size(population_count_2018)
PopDense2018 = calculate_pop_density(population_density_2018)
hospital_admissions_2018 = calculate_expected_cases(Pop2018, 2018, PopDense2018)



#Bind together
hospital_admissions_deldir = rbind(hospital_admissions_2008,
                                   hospital_admissions_2009,
                                   hospital_admissions_2010,
                                   hospital_admissions_2011,
                                   hospital_admissions_2012,
                                   hospital_admissions_2013,
                                   hospital_admissions_2014,
                                   hospital_admissions_2015,
                                   hospital_admissions_2016,
                                   hospital_admissions_2017,
                                   hospital_admissions_2018)



hospital_admissions_allyears_DF = data.frame(hospital_admissions_allyears_SpatPolyDF)

#Agricultural Workers
agry_raster = raster("./Additional_Data/Agry_raster.tif")

#Climatic Zone
climatic_zone_raster = raster("./Additional_Data/Raster_climatic_zones.tif") 

#Altitude
altitude_raster = raster("./Additional_Data/LKA_alt.grd")

library(DescTools)
climatic_zone_raster_test = climatic_zone_raster
#remove the sea which are the zeroes
values(climatic_zone_raster_test)[values(climatic_zone_raster_test) == 0] = NA

#Merge zones 3 and 4 together
values(climatic_zone_raster_test)[values(climatic_zone_raster_test) == 4] = 3


#Extract raster variables for each hospital territory
agry_var = raster::extract(agry_raster, hospital_territories_spatpolydf, fun=mean, na.rm=TRUE)
climatic_zone_var = raster::extract(climatic_zone_raster_test, hospital_territories_spatpolydf, fun=Mode, na.rm=TRUE)
altitude_var = raster::extract(altitude_raster, hospital_territories_spatpolydf, fun=mean, na.rm=TRUE)


#Now add the agry, climate and altitude to the hospital data:

add_clim_agry_alt = function(hosp.adm.year){
  
  hosp.adm.year$Altitude = altitude_var[, 1]
  hosp.adm.year$Climatic_Zone = climatic_zone_var[, 1]
  hosp.adm.year$Agry = agry_var[, 1]
  
  return(hosp.adm.year)
}


hospital_admissions_2008_CAA = add_clim_agry_alt(hospital_admissions_2008)
hospital_admissions_2009_CAA = add_clim_agry_alt(hospital_admissions_2009)
hospital_admissions_2010_CAA = add_clim_agry_alt(hospital_admissions_2010)
hospital_admissions_2011_CAA = add_clim_agry_alt(hospital_admissions_2011)
hospital_admissions_2012_CAA = add_clim_agry_alt(hospital_admissions_2012)
hospital_admissions_2013_CAA = add_clim_agry_alt(hospital_admissions_2013)
hospital_admissions_2014_CAA = add_clim_agry_alt(hospital_admissions_2014)
hospital_admissions_2015_CAA = add_clim_agry_alt(hospital_admissions_2015)
hospital_admissions_2016_CAA = add_clim_agry_alt(hospital_admissions_2016)
hospital_admissions_2017_CAA = add_clim_agry_alt(hospital_admissions_2017)
hospital_admissions_2018_CAA = add_clim_agry_alt(hospital_admissions_2018)

hospital_admissions_CAA = rbind(
  hospital_admissions_2008_CAA,
  hospital_admissions_2009_CAA,
  hospital_admissions_2010_CAA,
  hospital_admissions_2011_CAA,
  hospital_admissions_2012_CAA,
  hospital_admissions_2013_CAA,
  hospital_admissions_2014_CAA,
  hospital_admissions_2015_CAA,
  hospital_admissions_2016_CAA,
  hospital_admissions_2017_CAA,
  hospital_admissions_2018_CAA)

#Add the hospital type and hospital care level
hospital_typedf = readxl::read_excel("./Hospital Type.xlsx")
hospital_admissions_all_vars = left_join(hospital_admissions_CAA, hospital_typedf)
hospital_admissions_all_vars = hospital_admissions_all_vars%>%
  mutate(Hospital_Type = as.factor(Hospital_Type))%>%
  mutate(Care_Level = as.factor(Care_Level))

#This is the final dataset with every explantory variable (Agry, Climate, Pop Dense, Pop Size, Incidence, Expected Admissions and hospital level/type)
write.csv(hospital_admissions_all_vars,".//hospital_admissions_all_vars.csv")



make_spatpolydf = function(hosp.adm.year, year){
  hosp.adm.year = hosp.adm.year%>%
    filter(Year == year)
  
  hosp.adm.year.spatpolydf = hospital_territories_spatpolydf
  hosp.adm.year.spatpolydf$population_size = hosp.adm.year$population_size
  hosp.adm.year.spatpolydf$Admissions = hosp.adm.year$Admissions
  hosp.adm.year.spatpolydf$Expected_Admissions = hosp.adm.year$Expected_Admissions
  hosp.adm.year.spatpolydf$Difference = hosp.adm.year$Difference
  hosp.adm.year.spatpolydf$Proportion_Turning_Up = hosp.adm.year$Proportion_Turning_Up
  hosp.adm.year.spatpolydf$Year = hosp.adm.year$Year
  hosp.adm.year.spatpolydf$Agry = hosp.adm.year$Agry
  hosp.adm.year.spatpolydf$Altitude = hosp.adm.year$Altitude
  hosp.adm.year.spatpolydf$Climatic_Zone= hosp.adm.year$Climatic_Zone
  hosp.adm.year.spatpolydf$population_density = hosp.adm.year$population_density
  hosp.adm.year.spatpolydf$Care_Level = hosp.adm.year$Care_Level
  hosp.adm.year.spatpolydf$Hospital_Type = hosp.adm.year$Hospital_Type
    
  return(hosp.adm.year.spatpolydf)
}

#Each of these can be used for plotting the maps using tmap
hospital_admissions_2008_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2008)
hospital_admissions_2009_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2009)
hospital_admissions_2010_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2010)
hospital_admissions_2011_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2011)
hospital_admissions_2012_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2012)
hospital_admissions_2013_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2013)
hospital_admissions_2014_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2014)
hospital_admissions_2015_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2015)
hospital_admissions_2016_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2016)
hospital_admissions_2017_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2017)
hospital_admissions_2018_SpatPolyDF = make_spatpolydf(hospital_admissions_all_vars, 2018)

hospital_admissions_allyears_SpatPolyDF = rbind(hospital_admissions_2008_SpatPolyDF,
                                                hospital_admissions_2009_SpatPolyDF,
                                                hospital_admissions_2010_SpatPolyDF,
                                                hospital_admissions_2011_SpatPolyDF,
                                                hospital_admissions_2012_SpatPolyDF,
                                                hospital_admissions_2013_SpatPolyDF,
                                                hospital_admissions_2014_SpatPolyDF,
                                                hospital_admissions_2015_SpatPolyDF,
                                                hospital_admissions_2016_SpatPolyDF,
                                                hospital_admissions_2017_SpatPolyDF,
                                                hospital_admissions_2018_SpatPolyDF)



#Turn Care Level into a raster for the inclusion in the analysis:
care_level_raster = rasterize(hospital_admissions_2013_SpatPolyDF,
                              population_count_2013,
                              "Care_Level")

writeRaster(care_level_raster, "./Care_Level_Raster.tif", overwrite = TRUE)




