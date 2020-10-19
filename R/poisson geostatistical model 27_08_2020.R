#Libraries:
library(PrevMap)
library(raster)
library(geoR)
library(splancs)
library(lme4)
library(dplyr)
library(sf)
library(mgcv)
library(dplyr)
library(rgdal)



#Poisson Modelling 27/08/2020#There are a total of ~543 unique hospitals; over a period of 2008-2018. Most hospitals have
#more than one years worth of data. 

#Read in dataset (post deldir; with expected admissions, altitude, climate and agriculture, hospital level etc)
hospital_admissions_all_years_all_vars = read.csv("./hospital_admissions_all_vars.csv")

#About the dataset:
#Hospital : is the name of the hospital
# x , y : x and y coordinates
# incidence_risk : is the snakebite risk for hospital territory/catchment
# population_size: is the number of individuals that live in hospital territory/catchement
# Year : The year the data comes from
# Admissions : The ACTUAL admissions reported at the hospital
# Expected_Admissions : population_size * incidence_risk [the number of snakebite cases we would expect in the territory]
# Difference : Expected_Admissions - Admissions
# Proportion_Turning_Up : Admissions/Expected_Admissions [same as Admi_Div_Expect]
# Altitude : The mean elevation of the hospital territory (in metres)
# Agry : The mean relative greenness
# Climatic_Zone : Zone3 = Wet ; Zone2 = Intermediate, Zone1 = Dry
#Hospital_Type : as proxy for hospital size/quality
#Care Level : Tertiary, Secondary, Primary

#2013 best covers the year of the snakebite survey the incidence was generated from
hospital_admissions_2013_all_vars = hospital_admissions_all_years_all_vars%>%
 filter(Year == 2013)

#Remove all the NAs from the Hospital Admissions Data to prevent errors occurring later on.
hospital_admissions_2013_noNA = hospital_admissions_2013_all_vars%>%
  filter(!is.na(Admissions))%>%
  filter(!is.na(Expected_Admissions))%>%
  filter(!is.na(Altitude))%>%
  filter(!is.na(Climatic_Zone))%>%
  filter(!is.na(Agry))%>%
  filter(!is.na(population_density))%>%
  mutate(Climatic_Zone = as.factor(Climatic_Zone))

#Run the GLM without any splines:
glm.fit.poisson.no.splines = glm(Admissions ~ Agry + Altitude + Climatic_Zone + incidence_risk + Care_Level + population_density,
                                 offset = log(population_size), family = poisson,
                                 data = hospital_admissions_2013_noNA)

summary(glm.fit.poisson.no.splines)

#Run the GAMs:

#Agricultural Workers:
gam.fit.smooth.agry = gam(Admissions ~ s(Agry) + Altitude + Climatic_Zone + incidence_risk + Care_Level + population_density,
                          offset = log(population_size), family = poisson,
                          data = hospital_admissions_2013_noNA)

plot(gam.fit.smooth.agry)#Looks more generally like a upward increase.

#Altitude:
gam.fit.smooth.altitude= gam(Admissions ~ Agry + s(Altitude) + Climatic_Zone + incidence_risk + Care_Level + population_density,
                          offset = log(population_size), family = poisson,
                          data = hospital_admissions_2013_noNA)
plot(gam.fit.smooth.altitude) #create knot at around 500-600m

#incidence_risk:
gam.fit.smooth.risk = gam(Admissions ~ Agry + Altitude + Climatic_Zone + s(incidence_risk) + Care_Level + population_density,
                          offset = log(population_size), family = poisson,
                          data = hospital_admissions_2013_noNA)
plot(gam.fit.smooth.risk)#Looks like the noticeable pattern in the last version of the GAM has gone

#population_density:
gam.fit.smooth.popdensity = gam(Admissions ~ Agry + Altitude + Climatic_Zone + incidence_risk + Care_Level + s(population_density),
                          offset = log(population_size), family = poisson,
                          data = hospital_admissions_2013_noNA)
plot(gam.fit.smooth.popdensity)#There is a noticeable shift, but small number of data points causing it. Place knot @ ~4000.


#Create the initial splines:
hospital_admissions_2013_noNA$Altitude.spline1 = ifelse(hospital_admissions_2013_noNA$Altitude > 500, hospital_admissions_2013_noNA$Altitude-500, 0)
hospital_admissions_2013_noNA$Density.spline1 = ifelse(hospital_admissions_2013_noNA$population_density > 4000, hospital_admissions_2013_noNA$population_density-4000, 0)

find.max.logLik = function(Alt1, Dense1, data){

  data$Altitude.spline1 = ifelse(data$Altitude > Alt1, data$Altitude-Alt1, 0)
  data$Density.spline1  = ifelse(data$population_density > Dense1, data$population_density-Dense1, 0 )
  glm.fit.A = glm(Admissions ~ Climatic_Zone  + Agry + Altitude + 
                              Altitude.spline1 + incidence_risk + 
                              Care_Level + population_density + Density.spline1,
                  offset=log(population_size),
                  data = data, family = poisson)
  
  return(logLik(glm.fit.A))
}
logLik(glm.fit.poisson.no.splines) #-15839.24 (df=9)
#Based off the visual guess of values
find.max.logLik(500, 4000, hospital_admissions_2013_noNA)#-15643.28 (df=11)

#Find Altitude.spine (by 10m intervals)
find.max.logLik(600, 4000, hospital_admissions_2013_noNA) #-15632.98 (df=11)

#Find Dense.spline (by 50people per km^2)
find.max.logLik(600, 3850, hospital_admissions_2013_noNA) #-15625.43 (df=11)

#Update Spines with values that maximise the likelihood
hospital_admissions_2013_noNA$Altitude.spline1 = ifelse(hospital_admissions_2013_noNA$Altitude > 600, hospital_admissions_2013_noNA$Altitude-600, 0)
hospital_admissions_2013_noNA$Density.spline1 = ifelse(hospital_admissions_2013_noNA$population_density > 3850, hospital_admissions_2013_noNA$population_density-3850, 0)


#GLM with maximum likelihood splines:
glm.fit.poisson.with.splines = glm(Admissions ~ Agry + Altitude + Altitude.spline1 + Climatic_Zone + 
                                     incidence_risk + Care_Level + population_density + Density.spline1,
                                 offset = log(population_size), family = poisson,
                                 data = hospital_admissions_2013_noNA)
summary(glm.fit.poisson.with.splines)

#Check if splines make the model "better" statistically
anova(glm.fit.poisson.no.splines, glm.fit.poisson.with.splines, test = "Chisq")

#Create ID.Coords vector
ID.coords = create.ID.coords(hospital_admissions_2013_noNA, ~x+y)

#Check for residual spatial correlation:
spat.corr.glm.fit.poisson.with.splines = spat.corr.diagnostic(Admissions ~ Climatic_Zone +
                                               Agry + incidence_risk + 
                                               Altitude + Altitude.spline1 + 
                                               Care_Level + population_density + Density.spline1,
                                          coords=~x+y, 
                                          units.m = ~population_size,
                                          data = hospital_admissions_2013_noNA, 
                                          ID.coords=ID.coords,
                                          n.sim = 1000,
                                          likelihood = "Poisson",
                                          lse.variogram = TRUE)

#Put in quadratic trend surface...
glm.fit.poisson.QTS = glm(Admissions ~ Agry + Altitude + Altitude.spline1 + Climatic_Zone + 
                                      incidence_risk + Care_Level + population_density +
                                      x + y + I(x^2) + I(y^2) + I(x*y),
                                   offset = log(population_size), family = poisson,
                                   data = hospital_admissions_2013_noNA)

summary(glm.fit.poisson.QTS)

spat.corr.glm.fit.poisson.QTS = spat.corr.diagnostic(Admissions ~ Climatic_Zone +
                                                                Agry + incidence_risk + 
                                                                Altitude + Altitude.spline1 + 
                                                                Care_Level + population_density + Density.spline1 +
                                                                x + y + I(x^2) + I(y^2) + I(x*y),
                                                              coords=~x+y, 
                                                              units.m = ~population_size,
                                                              data = hospital_admissions_2013_noNA, 
                                                              ID.coords=ID.coords,
                                                              n.sim = 1000,
                                                              likelihood = "Poisson",
                                                              lse.variogram = TRUE)


# # Error in (function (fr, X, reTrms, family, nAGQ = 1L, verbose = 0L, maxit = 100L,  : 
# Downdated VtV is not positive definite


#Check for major outliers in the residuals:
hist(glm.fit.poisson.with.splines$residuals)
hospital_admissions_2013_noNA$residuals = glm.fit.poisson.with.splines$residuals

View(hospital_admissions_2013_noNA)


plot(glm.fit.poisson.QTS)
summary(hospital_admissions_2013_CAA_noNA)
library(geoR)
View(hospital_admissions_2013_CAA_noNA)

#Remove the major outlier: Ampan
hospital_admissions_2013_noOutlier = hospital_admissions_2013_noNA[-c(19),]


#Create ID.Coords vector
ID.coords = create.ID.coords(hospital_admissions_2013_noOutlier, ~x+y)
spat.corr.glm.fit.poisson.noOutier = spat.corr.diagnostic(Admissions ~ Climatic_Zone +
                                                                Agry + incidence_risk + 
                                                                Altitude + Altitude.spline1 + 
                                                                Care_Level + population_density + Density.spline1,
                                                              coords=~x+y, 
                                                              units.m = ~population_size,
                                                              data = hospital_admissions_2013_noOutlier, 
                                                              ID.coords=ID.coords,
                                                              n.sim = 1000,
                                                              likelihood = "Poisson",
                                                              lse.variogram = TRUE)




xyz = as.geodata(cbind(hospital_admissions_2013_noOutlier$x, hospital_admissions_2013_noOutlier$y, hospital_admissions_2013_noOutlier$residuals))
plot(xyz)
plot(variog(xyz))
u = 0.05*(1:20)
plot(variog(xyz, uvec = u))

points(xyz, cex.min = 0.5, cex.max=2, pt.divide = "quint")

plot(xyz)


####################
#Rerun glm with updated dataset:

glm.fit.poisson.no.splines.updated = glm(Admissions ~ Agry + Altitude + Climatic_Zone + incidence_risk + Care_Level + population_density,
                                 offset = log(population_size), family = poisson,
                                 data = hospital_admissions_2013_noOutlier)

summary(glm.fit.poisson.no.splines.updated)

#Run the GAMs:

#Agricultural Workers:
gam.fit.smooth.agry.updated = gam(Admissions ~ s(Agry) + Altitude + Climatic_Zone + incidence_risk + Care_Level + population_density,
                          offset = log(population_size), family = poisson,
                          data = hospital_admissions_2013_noOutlier)

plot(gam.fit.smooth.agry.updated)#Looks more generally like a upward increase.

#Altitude:
gam.fit.smooth.altitude.updated= gam(Admissions ~ Agry + s(Altitude) + Climatic_Zone + incidence_risk + Care_Level + population_density,
                             offset = log(population_size), family = poisson,
                             data = hospital_admissions_2013_noOutlier)
plot(gam.fit.smooth.altitude.updated) #create knot at around 500-600m
hospital_admissions_2013_noOutlier$Altitude.spline1 = ifelse(hospital_admissions_2013_noOutlier$Altitude > 500, hospital_admissions_2013_noOutlier$Altitude-500, 0)


#incidence_risk:
gam.fit.smooth.risk.updated = gam(Admissions ~ Agry + Altitude + Climatic_Zone + s(incidence_risk) + Care_Level + population_density,
                          offset = log(population_size), family = poisson,
                          data = hospital_admissions_2013_noOutlier)
plot(gam.fit.smooth.risk.updated)#Include spline at ~0.008
hospital_admissions_2013_noOutlier$Risk.spline1 = ifelse(hospital_admissions_2013_noOutlier$incidence_risk > 0.008, hospital_admissions_2013_noOutlier$incidence_risk-0.008, 0)

#population_density:
gam.fit.smooth.popdensity.updated = gam(Admissions ~ Agry + Altitude + Climatic_Zone + incidence_risk + Care_Level + s(population_density),
                                offset = log(population_size), family = poisson,
                                data = hospital_admissions_2013_noOutlier)
plot(gam.fit.smooth.popdensity.updated)# Include spline at ~4000
hospital_admissions_2013_noOutlier$PopDense.spline1 = ifelse(hospital_admissions_2013_noOutlier$population_density > 4000, hospital_admissions_2013_noOutlier$population_density-4000, 0)



logLik(glm.fit.poisson.no.splines.updated) # -14214.82 (df=9)

glm.fit.poisson.with.splines.updated = glm(Admissions ~ Agry + Altitude + Altitude.spline1 + Climatic_Zone + incidence_risk + 
                                             Care_Level + population_density + PopDense.spline1,
                                         offset = log(population_size), family = poisson,
                                         data = hospital_admissions_2013_noOutlier)
summary(glm.fit.poisson.with.splines.updated)
logLik(glm.fit.poisson.with.splines.updated) # -13985.58 (df=11)

#Maximise the Likelihood:


find.max.logLik = function(Alt1, Dense1, data){
  
  data$Altitude.spline1 = ifelse(data$Altitude > Alt1, data$Altitude-Alt1, 0)
  data$Density.spline1  = ifelse(data$population_density > Dense1, 
                                 data$population_density-Dense1, 0 )
  glm.fit.A = glm(Admissions ~ Climatic_Zone  + Agry + Altitude + 
                    Altitude.spline1 + incidence_risk + 
                    Care_Level + population_density + Density.spline1,
                  offset=log(population_size),
                  data = data, family = poisson)
  
  return(logLik(glm.fit.A))
}

find.max.logLik(500, 4000, hospital_admissions_2013_noOutlier)#-13985.58 (df=11) (df=11)

#Maxise Altitude: @10m intervals
find.max.logLik(610, 4000, hospital_admissions_2013_noOutlier)# -13975.88 (df=11) 

#Maxise Population Density: @50p/km2 intervals
find.max.logLik(610, 3550, hospital_admissions_2013_noOutlier)#-13962.78 (df=11)

#Add in the updated maximised splines:

hospital_admissions_2013_noOutlier$Altitude.spline1 = ifelse(hospital_admissions_2013_noOutlier$Altitude > 610, hospital_admissions_2013_noOutlier$Altitude-610, 0)
hospital_admissions_2013_noOutlier$PopDense.spline1 = ifelse(hospital_admissions_2013_noOutlier$population_density > 3550, hospital_admissions_2013_noOutlier$population_density-3550, 0)

glm.fit.poisson.with.splines.updated.ML = glm(Admissions ~ Agry + Altitude + Altitude.spline1 + Climatic_Zone + incidence_risk + 
                                             Care_Level + population_density + PopDense.spline1,
                                           offset = log(population_size), family = poisson,
                                           data = hospital_admissions_2013_noOutlier)

summary(glm.fit.poisson.with.splines.updated.ML)
hist(glm.fit.poisson.with.splines.updated.ML$residuals)

hospital_admissions_2013_noOutlier$residuals = glm.fit.poisson.with.splines.updated.ML$residuals
####
ID.coords = create.ID.coords(hospital_admissions_2013_noOutlier, ~x+y)

spat.corr.glm.fit.poisson.with.splines.updated.ML = spat.corr.diagnostic(Admissions ~ Climatic_Zone +
                                                                Agry + incidence_risk  + 
                                                                Altitude + Altitude.spline1 + 
                                                                Care_Level + population_density
                                                              + PopDense.spline1,
                                                              coords=~x+y, 
                                                              units.m = ~population_size,
                                                              data = hospital_admissions_2013_noOutlier, 
                                                              ID.coords=ID.coords,
                                                              n.sim = 1000,
                                                              likelihood = "Poisson",
                                                              lse.variogram = TRUE)

sigma2.start = 4459.589 
phi.start = 39028.61
tau2.start = 0.8537592
beta.start = coef(glm.fit.poisson.with.splines.updated.ML)

xyz = as.geodata(cbind(hospital_admissions_2013_noOutlier$x, hospital_admissions_2013_noOutlier$y, hospital_admissions_2013_noOutlier$residuals))
plot(xyz)
plot(variog(xyz))
u = 0.05*(1:20)
plot(variog(xyz, uvec = u))
points(xyz, cex.min = 0.5, cex.max=2, pt.divide = "quint")

plot(xyz)


#Initial La Place Approximation:
# fit.la = glgm.LA(Admissions ~ Agry + Altitude + Altitude.spline1 + Climatic_Zone + incidence_risk + 
#                    Care_Level + population_density + PopDense.spline1,
#                  coords = ~x+y,
#                  units.m = ~population_size,
#                  ID.coords=ID.coords,
#                  kappa = 0.5,
#                  start.cov.pars = c(phi.start, tau2.start/sigma2.start),
#                  fixed.rel.nugget = NULL,
#                  data = hospital_admissions_2013_noOutlier,
#                  method = "nlminb",
#                  family = "Poisson")

# Error in maxOptim(fn = fn, grad = grad, hess = hess, start = start, method = "BFGS",  : 
#                     Infinite initial gradient

# 
# #Calculate kappa
# hospital_admissions_2013_noOutlier$logit = log((hospital_admissions_2013_noOutlier$Admissions +0.5)/
#                                               (hospital_admissions_2013_noOutlier$population_size - 
#                                                  hospital_admissions_2013_noOutlier$Admissions+1))
# # 
# profile.kappa = shape.matern(formula = Admissions ~ Agry + Altitude + Altitude.spline1 + Climatic_Zone + incidence_risk + 
#                                   Care_Level + population_density + PopDense.spline1,
#                              coords = ~x+y,
#                              data = hospital_admissions_2013_noOutlier,
#                              start.par = c(0.2, 0.05), coverage = 0.95,
#                              set.kappa = seq(0.2,10, length = 15))
#Value outside of range.


coords = as.matrix(coords = coords, hospital_admissions_2013_noOutlier[, c("x", "y")])
vari <- variog(coords = coords, data =hospital_admissions_2013_noOutlier$Admissions,
                  uvec = c(0, 0.1, 0.15, 0.2, 0.4, 0.8, 1.4, 1.8, 2, 2.5, 3, 4))
vari.fit <- variofit(vari, ini.cov.pars = c(2, 0.2),
                        cov.model = "matern",
                        fix.nugget = FALSE, nugget = 0 ,
                        fix.kappa = TRUE, kappa = 0.5)

par(mfrow = c(1,2))
plot(coords, pch = 20, asp = 1, cex = 0.5, main = "(a)")
plot(vari, main = "(b)")
lines(vari.fit)
vari.fit
par(mfrow = c(1,1))


geo.fit.no.outlier = poisson.log.MCML(
                formula = Admissions ~ Agry + 
                          Altitude + Altitude.spline1 + 
                          Climatic_Zone + incidence_risk + 
                          Care_Level + population_density + 
                          PopDense.spline1,
                coords=~x+y, 
                units.m = ~population_size,
                data = hospital_admissions_2013_noOutlier, 
                ID.coords=ID.coords,
                par0 = c(beta.start,sigma2.start,phi.start,tau2.start),
                control.mcmc = control.mcmc.MCML(n.sim=12000, burnin =2000, thin = 10),
                fixed.rel.nugget = NULL,
                kappa = 0.5, #Use 0.5 as default
                start.cov.pars = c(phi.start,
                                   tau2.start/sigma2.start),
                method = "nlminb")

##summary of poisson.log.MCLML
summary(geo.fit.no.outlier)

geo.fit.diag = variog.diagnostic.glgm(geo.fit.no.outlier)



##coefficients of MCML
beta.hat.geo = summary(geo.fit.no.outlier)$coefficients[,1:2]

##95% CIs of Estimates from MCML
cbind(beta.hat.geo,
      beta.hat.geo[,1]-qnorm(0.975)*beta.hat.geo[,2],
      beta.hat.geo[,1]+qnorm(0.975)*beta.hat.geo[,2])


##prediction grid
#Agricultural Workers:
agry_raster = raster("./Additional_Data/Agry_raster.tif")

#Climatic Zone:
climatic_zone_raster = raster("./Additional_Data/Raster_climatic_zones.tif")

#Altitude Raster:
altitude_raster = raster("./Additional_Data/LKA_alt.grd")

#Snakebite Raster from PLoS NTDs paper:
snakebite_raster = raster("./Snakebite_map_raster.tif")

#Population Density Raster:
population_density_2013 = raster("./Additional_Data/lka_pd_2013_1km.tif")

#Care Level Raster
care_level_raster = raster("./Care_Level_Raster.tif")

#Sri Lanka Shapefile:
sri_lanka_border = st_read("./Additional_Data/lka_adm_shp/lka_admbnda_adm0_slsd_20200305.shp")

values(climatic_zone_raster)[values(climatic_zone_raster) == 0] = NA
values(climatic_zone_raster)[values(climatic_zone_raster) == 4] = 3

#Make the Prediction Grid using convex hull approach:
coords = unique(hospital_admissions_all_years_all_vars[,c("x", "y")])
plot(coords)
#Make polygon of coords:
poly = coords[chull(coords),]
lines(rbind(poly,poly[1,]), col=2)

#Turn into matrix:
poly = as.matrix(poly)

#Prediction Grid:
sri.lanka.grid = gridpts(poly, xs = 0.015, ys=0.015)

library(DescTools)
#Rasters of the Explantory Variables 
Altitude = raster::extract(altitude_raster,sri.lanka.grid, fun = mean)
Climatic_Zone = raster::extract(climatic_zone_raster_test, sri.lanka.grid, fun = Mode)
Agry = raster::extract(agry_raster, sri.lanka.grid, fun = mean)
PopDensity = raster::extract(population_density_2013, sri.lanka.grid, fun = mean)
Snakebite = raster::extract(snakebite_raster, sri.lanka.grid, fun = mean)
Care_Level = raster::extract(care_level_raster, sri.lanka.grid, fun = Mode)

#Get care levels named correctly
Care_Level = as.factor(c(Care_Level))
levels(Care_Level)[levels(Care_Level)=="1"] = "Primary"
levels(Care_Level)[levels(Care_Level)=="2"] = "Secondary"
levels(Care_Level)[levels(Care_Level)=="3"] = "Tertiary"

#Find all the NAs
ind.na1 = which(is.na(Altitude))
ind.na2 = which(is.na(Climatic_Zone))
ind.na3 = which(is.na(Agry))
ind.na4 = which(is.na(PopDensity))
ind.na5 = which(is.na(Snakebite))
ind.na6 = which(is.na(Care_Level))

ind.na = c(ind.na1, ind.na2, ind.na3, ind.na4, ind.na5, ind.na6)

#Remove all the NAs
Altitude = Altitude[-ind.na]
Climatic_Zone = Climatic_Zone[-ind.na]
Agry = Agry[-ind.na]
population_density = PopDensity[-ind.na]
incidence_risk = Snakebite[-ind.na]
Care_Level = Care_Level[-ind.na]
sri.lanka.grid = sri.lanka.grid[-ind.na, ]

#Make sure climatic zone is as a factor:
Climatic_Zone = as.factor(as.character(Climatic_Zone))

#The Splines
PopDense.spline1 = ifelse(population_density > 3550, population_density-3550, 0)
Altitude.spline1 = ifelse(Altitude > 600, Altitude-600, 0)

plot(st_geometry(sri_lanka_border))  
points(sri.lanka.grid)

#Check the order and names of the explanatory variables in geo.fit.no.outlier
# # formula = Admissions ~ Agry +   
# #                        Altitude + 
#                          Altitude.spline1 + 
# #                        Climatic_Zone + 
#                          incidence_risk +  
#                          Care_Level + 
#                          population_density + 
# #                        PopDense.spline1


#Eight variables
predictor.values = data.frame(Agry = Agry,
                              Altitude = Altitude,
                              Altitude.spline1 = Altitude.spline1,
                              Climatic_Zone = Climatic_Zone,
                              incidence_risk = incidence_risk,
                              Care_Level = Care_Level,
                              population_density = population_density,
                              PopDense.spline1 = PopDense.spline1)


spatial_prediction = spatial.pred.poisson.MCML(
  object = geo.fit.no.outlier, #The output from poisson.log.MCML
  grid.pred = sri.lanka.grid, 
  predictors = predictor.values,
  control.mcmc = control.mcmc.MCML(n.sim=12000, burnin =2000, thin = 10),
  type = "marginal",
  scale.predictions = c("log", "exponential"),
  standard.errors = TRUE,
  quantiles = c(0.025, 0.975))

par(mfrow = c(2, 2))
plot(spatial_prediction,"log", "predictions", main ="Log Predictions")
plot(spatial_prediction,"exponential", "predictions", main = "Exponential Predicitons")
plot(spatial_prediction,"log", "standard.errors", main = "Log Standard Errors")
plot(spatial_prediction,"exponential", "standard.errors", main = "Exponential Standard Errors")
par(mfrow = c(1,1))

log.prediction.raster = rasterFromXYZ(cbind(sri.lanka.grid, spatial_prediction$log$predictions))
log.prediction.se.raster = rasterFromXYZ(cbind(sri.lanka.grid, spatial_prediction$log$standard.errors))
exp.prediction.raster = rasterFromXYZ(cbind(sri.lanka.grid, spatial_prediction$exponential$predictions))
exp.prediction.se.raster = rasterFromXYZ(cbind(sri.lanka.grid, spatial_prediction$exponential$standard.errors))



plot(pred.raster)
sri_lanka_map+
tm_shape(exp.prediction.se.raster) + tm_raster()

summary(spatial_prediction$samples)

GN_Boundary = readOGR("./GN/GN Division.shp")

newcrs = crs(snakebite_raster)
#Make GN boundaries have same CRS as the raster files
GN_Boundary_Updated = spTransform(GN_Boundary, newcrs)


#Extract at GN the required data:
GN_Risk = raster::extract(snakebite_raster, GN_Boundary_Updated, fun=mean, na.rm = TRUE, 
                                    weights = TRUE, normalizeWeights = TRUE)

GN_Pop_Size = raster::extract(population_count_2013, GN_Boundary_Updated, fun = sum, na.rm = TRUE)
                              
Exp_GN_Prediction = raster::extract(exp.prediction.raster, GN_Boundary_Updated, fun = mean, na.rm = TRUE,
                                    weights = TRUE, normalizeWeights = TRUE)

Exp_GN_SE = raster::extract(exp.prediction.se.raster, GN_Boundary_Updated, fun = mean, na.rm=TRUE,
                            weights = TRUE, normalizeWeights = TRUE)

GN_Prediction = Exp_GN_Prediction * GN_Pop_Size
GN_Upper_CI = GN_Pop_Size * (Exp_GN_Prediction + (1.96 * Exp_GN_SE))
GN_Lower_CI = GN_Pop_Size * (Exp_GN_Prediction - (1.96 * Exp_GN_SE))

GN_Expected = GN_Risk * GN_Pop_Size
GN_Difference = GN_Expected - GN_Prediction
GN_Difference_UpperCI = GN_Expected - GN_Upper_CI
GN_Difference_LowerCI = GN_Expected - GN_Lower_CI

sum(GN_Prediction, na.rm = TRUE)

sum(GN_Expected, na.rm = TRUE)


range(GN_Upper_CI, na.rm = TRUE) # 0 to 200
range(GN_Lower_CI, na.rm = TRUE) # 0 to 70

GN_Diff_Proportion = GN_Difference / GN_Expected

GN_Boundary_Updated$GN_Prediction = GN_Prediction[, 1]
GN_Boundary_Updated$GN_Expected = GN_Expected[, 1]
GN_Boundary_Updated$GN_Difference = GN_Difference[, 1]
GN_Boundary_Updated$GN_Upper_CI = GN_Upper_CI[, 1]
GN_Boundary_Updated$GN_Lower_CI = GN_Lower_CI[, 1]
GN_Boundary_Updated$GN_Difference_UpperCI = GN_Difference_UpperCI[, 1]
GN_Boundary_Updated$GN_Difference_LowerCI = GN_Diffference_LowerCI[, 1]
GN_Boundary_Updated$GN_SE = Exp_GN_SE[, 1]
GN_Boundary_Updated$GN_Diff_Proportion = GN_Diff_Proportion[, 1]


GN_Division = GN_Boundary_Updated$GND_N
PROVINCE_N  = GN_Boundary_Updated$PROVINCE_N
GN_Snakebite = data.frame(GN_Prediction,#from geostats model of hosp admissions
                          GN_Expected,
                          GN_Difference,# D = E-P
                          PROVINCE_N,
                          GN_Upper_CI,
                          GN_Lower_CI,
                          GN_Division, 
                          GN_Diff_Proportion) 

View(GN_Snakebite)

GN_Snakebite_noNA = GN_Snakebite%>%
  filter(!is.na(GN_Expected))%>%
  filter(!is.na(GN_Prediction))

sum(GN_Snakebite_noNA$GN_Prediction)

#Estimated number of snakebite admissions detected through only hospital admissions
sum(GN_Snakebite_noNA$GN_Prediction)/sum(GN_Snakebite_noNA$GN_Expected)#0.459846

#Best case scenario:
sum(GN_Snakebite_noNA$GN_Upper_CI)/sum(GN_Snakebite_noNA$GN_Expected) #0.6619132

#Worst case scenario:
sum(GN_Snakebite_noNA$GN_Lower_CI)/sum(GN_Snakebite_noNA$GN_Expected) #0.2577788

nrow(GN_Snakebite)

#Amalgamate Per Province
GN_Snakebite_noNA%>%
  group_by(PROVINCE_N)%>%
  summarise(sum(GN_Prediction)/sum(GN_Expected))

#Lower bound
GN_Snakebite_noNA%>%
  group_by(PROVINCE_N)%>%
  summarise(sum(GN_Lower_CI)/sum(GN_Expected))


#Upper bound
GN_Snakebite_noNA%>%
  group_by(PROVINCE_N)%>%
  summarise(sum(GN_Upper_CI)/sum(GN_Expected))



library(ggplot2)
ggplot(GN_Snakebite, aes(x=GN_Expected, y=GN_Prediction, colour = PROVINCE_N))+
  geom_point(alpha = 0.15)+
  geom_abline(slope = 1, intercept = 0, colour = "black") +
  ylab("Admissions Predicted from Geostatistical Model") +
  xlab("Snakebite Admissions Expected") +
  xlim(0, 55) + #One point out of range: P=134.5848, E=141.712; in Eastern Province
  ylim(0, 55) +
  theme_classic()+
  theme(legend.position = "none") +
  facet_wrap(~PROVINCE_N)

ggplot(GN_Snakebite, aes(x=GN_Diff_Proportion))+
  geom_histogram(binwidth = 0.1)




ggplot(GN_Snakebite_noNA, aes(x=GN_Difference, fill = PROVINCE_N)) +
  geom_histogram(binwidth = 1) +
  theme_classic()+
  facet_wrap(~PROVINCE_N)






library(tmap)
library(RColorBrewer)
#Difference

pals = c("#2b8cbe", "#fee5d9", "#fcae91", "#fb6a4a", "#cb181d")
range(GN_Difference, na.rm = TRUE)
range(GN_Prediction, na.rm = TRUE)
range(GN_Expected, na.rm = TRUE)

Province_Boundaries = st_read("./Additional_Data/lka_adm_shp/lka_admbnda_adm1_slsd_20200305.shp")
print(Province_Boundaries$ADM1_EN)

tm_shape(Province_Boundaries) + tm_polygons("ADM1_EN", legend.show = F) +
  tm_text("ADM1_EN", size = 0.8)

Difference_Map = tm_shape(GN_Boundary_Updated) + tm_fill(col = "GN_Difference",
                                        breaks = c(-30, -20, -10, -1, 1, 10, 20, 30),
                                        palette = rev(brewer.pal(7, "RdBu")),
                                        title = "Difference")+
  tm_shape(sri_lanka_border) + tm_borders() +
  tm_shape(Province_Boundaries) + tm_borders() +
  tm_layout(legend.outside = FALSE,
            title = "A")


       
#Prediction from the hospital admissions geostatistical model.
Prediction_Map = tm_shape(GN_Boundary_Updated) + tm_fill(col = "GN_Prediction",
                                        breaks = c(0, 1, 5, 10, 20, 50, 100, 150),
                                        palette = brewer.pal(7, "Reds"),
                                        title = "Predicted Cases")+
  tm_shape(Province_Boundaries) + tm_borders()+
  tm_layout(title = "B", legend.outside = TRUE)

Expected_Map = tm_shape(GN_Boundary_Updated) + tm_fill(col = "GN_Expected",
                                        breaks = c(0, 1, 5, 10, 20, 50, 100, 150),
                                        palette = brewer.pal(7, "Reds"),
                                        title = "Expected Cases")+
  tm_shape(Province_Boundaries) + tm_borders()+
  tm_layout(title = "A", legend.outside = TRUE)



#Differences Confidence Intervals:
CIPlot1 = tm_shape(GN_Boundary_Updated) + tm_fill(col = "GN_Lower_CI",
                                        breaks = c(0, 1, 10, 25, 50, 100, 150, 200),
                                        palette = "BuPu",
                                        title = "Predicted Cases")+
  tm_shape(Province_Boundaries) + tm_borders()+
  tm_layout(title = "C", legend.outside = TRUE)



CIPlot2 =tm_shape(GN_Boundary_Updated) + tm_fill(col = "GN_Upper_CI",
                                        breaks = c(0, 1, 10, 25, 50, 100, 150, 200),
                                        palette = "BuPu",
                                        title = "Predicted Cases")+
  tm_shape(Province_Boundaries) + tm_borders()+
  tm_layout(title = "D", legend.outside = TRUE)

tmap_arrange(Expected_Map, Prediction_Map, 
             CIPlot1, CIPlot2, nrow = 2, ncol = 2)


pals = c("#ff7f00", "#bdbdbd", "#6a3d9a")
#Map showing locations where underreporting is greater 20%.
 #and where over reporting is greater than 20%
Proportion_Map = tm_shape(GN_Boundary_Updated) + tm_fill(col = "GN_Diff_Proportion",
                                        breaks = c(-4, -0.2, 0.2, 1),
                                        palette = pals,
                                        legend.show = TRUE,
                                        labels = c("Overestimate by 20%", "Difference less than 20%", "Underestimate by 20%"),
                                        title = "Estimate Difference %")+
  tm_shape(Province_Boundaries) + tm_borders() +
  tm_layout(title = "B")

tmap_arrange(Difference_Map, Proportion_Map, asp = 1)

Proportion_Map
