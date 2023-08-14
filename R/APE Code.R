library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(here)
cavmanor <- get_acs(
  geography = "county",
  variables = "C02003_004",
  state = "VA",
  county = c("Portsmouth","Hampton","Norfolk","Chesapeake","Suffolk","Newport News","Virginia Beach"),
  year = 2021,
  geometry = TRUE)
aodcrs <- "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
cavmanor <- st_transform(cavmanor,crs = aodcrs)
cavmanor <- st_bbox(cavmanor)

list.files()
AODfilenames <- list.files()
AODfilenames
AODfilenames <- paste0(substr(AODfilenames,10,13),"_",substr(AODfilenames,14,16), "_",substr(AODfilenames,18,20))
AODfilenames
AODfilenames2 <- substr(AODfilenames,1,8)
table(table(AODfilenames2))
table(AODfilenames2)
which(table(AODfilenames2)==1)
which(table(AODfilenames2)==2)
which(table(AODfilenames2)==3)
which(table(AODfilenames2)==4)



crs(cavmanor)

for(i in 1:length(list.files())){
  a <- sds(list.files()[i])
  a <- a[2]
  a <- mean(a,na.rm=TRUE)
  a <- crop(a, cavmanor)
  a <- project(a,"+proj=longlat +datum=WGS84 +no_defs")
  writeRaster(a,paste0("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/",substr(list.files()[i],10,16)," CavManor AOD.tif"))
  print(i)
}


#setwd("C:/Users/EJLI4/OneDrive - Emory University/Documents")
#setwd("/Users/ethan_j_li/OneDrive - Emory University/Documents/APE Data")
list.files(here::here("data"))

#test<-raster("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI/MOD13A1_NDVI_2005_321.tif")
#target <- raster(resolution=res(test)*7.3,crs=proj4string(test),ext=extent(test))
#res(target)
#test <- resample(test,target)
#test
#setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Shapefile")
#test<-rasterToPolygons(test)
#raster::shapefile(test,"test.shp",overwrite=TRUE)
#test2 <- st_read("test.shp")
#st_write(test2,"test.GeoJSON")

#houston <- get_acs(
#  geography = "county",
#  variables = "C02003_004",
#  state = "TX",
#  county = c(201,"Chambers","Liberty","Fort Bend","Montgomery","Galveston","Brazoria"),
#  year = 2021,
#  geometry = TRUE)
#houston <- st_combine(houston)
#st_write(houston,"houston.shp")
#houston <- st_read("houston.shp")
#plot(houston)

data2020ZCTA <- read.csv(here::here("Data","PLACES__ZCTA_Data__GIS_Friendly_Format___2022_release.csv"))
data2020city <- read.csv(here::here("Data","PLACES__Place_Data__GIS_Friendly_Format___2022_release.csv"))
data2020tract <- read.csv(here::here("Data","PLACES__Census_Tract_Data__GIS_Friendly_Format___2022_release.csv"))
data2019ZCTA <- read.csv(here::here("Data","PLACES__ZCTA_Data__GIS_Friendly_Format___2021_release.csv"))
data2019city <- read.csv(here::here("Data","PLACES__Place_Data__GIS_Friendly_Format___2021_release.csv"))
data2019tract <- read.csv(here::here("Data","PLACES__Census_Tract_Data__GIS_Friendly_Format___2021_release.csv"))
EJI <- read.csv(here::here("Data","Virginia EJI.csv"))
EJICAV <- subset(EJI, GEOID %in% c("51740211600",
                                   "51740211700",
                                   "51740212500",
                                   "51740212600",
                                   "51740212701",
                                   "51740212702",
                                   "51740212801",
                                   "51740212802",
                                   "51740212900"))
VAZips <- read.csv(here::here("Data","va-zip-codes-data.csv"))

#data2020 <- subset(data2020, Measure %in% c("Chronic obstructive pulmonary disease among adults aged >=18 years",
 #                                   "Coronary heart disease among adults aged >=18 years",
  #                                  "Current asthma among adults aged >=18 years",
   #                                 "High blood pressure among adults aged >=18 years",
    #                                "Stroke among adults aged >=18 years"
#                                    ))
#data2019 <- subset(data2019, Measure %in% c("Chronic obstructive pulmonary disease among adults aged >=18 years",
 #                                           "Coronary heart disease among adults aged >=18 years",
  #                                          "Current asthma among adults aged >=18 years",
   #                                         "High blood pressure among adults aged >=18 years",
    #                                        "Stroke among adults aged >=18 years"
#))
VA<-VAZips$zip
CAV <- 23701

#VAdata <- subset(data)
VAdata2020ZCTA <- subset(data2020ZCTA,as.numeric(ZCTA5) %in% VA)
VAdata2019ZCTA <- subset(data2019ZCTA,as.numeric(ZCTA5) %in% VA)

VAdata2020city <- subset(data2020city,StateAbbr == "VA")
VAdata2020city <- subset(VAdata2020city,PlaceName == "Portsmouth")
VAdata2019city <- subset(data2019city,StateAbbr == "VA")
VAdata2019city <- subset(VAdata2019city,PlaceName == "Portsmouth")

VAdata2020tract <- subset(data2020tract,StateDesc=="Virginia")
VAdata2019tract <- subset(data2019tract,StateDesc=="Virginia")

CAVdata2020ZCTA <- subset(VAdata2020ZCTA,as.numeric(ZCTA5) %in% CAV)
CAVdata2019ZCTA <- subset(VAdata2019ZCTA,as.numeric(ZCTA5) %in% CAV)

CAVdata2020tract <- subset(VAdata2020tract,as.numeric(TractFIPS)%in% c(51740212900,
                                                                       51740212802,
                                                                       51740212801,
                                                                       51740212701,
                                                                       51740212702,
                                                                       51740211600,
                                                                       51740211700,
                                                                       51740212600,
                                                                       51740212500))
CAVdata2019tract <- subset(VAdata2019tract,as.numeric(TractFIPS)%in% c(51740212900,
                                                                       51740212802,
                                                                       51740212801,
                                                                       51740212701,
                                                                       51740212702,
                                                                       51740211600,
                                                                       51740211700,
                                                                       51740212600,
                                                                       51740212500))



#2020 Stroke

VAdata2020ZCTA$STROKE_CrudePrev
mean(VAdata2020ZCTA$STROKE_CrudePrev)
sum(VAdata2020ZCTA$TotalPopulation)
#sum(((VAdata2020ZCTA$STROKE_CrudePrev)/100)*VAdata2020ZCTA$TotalPopulation)/(sum(VAdata2020ZCTA$TotalPopulation))

VAdata2020ZCTA$STROKE_Count <- VAdata2020ZCTA$TotalPopulation*VAdata2020ZCTA$STROKE_CrudePrev/100
VAstrokeresult <- t.test(VAdata2020ZCTA$STROKE_CrudePrev)
(sum(VAdata2020ZCTA$STROKE_Count)/sum(VAdata2020ZCTA$TotalPopulation))

VAstrokeresult$estimate
VAstrokeresult$conf.int

CAVdata2020ZCTA$STROKE_CrudePrev
CAVdata2020ZCTA$STROKE_Crude95CI

strokepercentile <- ecdf(VAdata2020ZCTA$STROKE_CrudePrev)
strokepercentile(4.1)

#2020 HTN
CAVdata2020ZCTA$BPHIGH_CrudePrev
HTNpercentile <- ecdf(VAdata2020ZCTA$BPHIGH_CrudePrev)
HTNpercentile(41.2)

#2020 COPD
VAdata2020ZCTA$COPD_CrudePrev
mean(VAdata2020ZCTA$COPD_CrudePrev)
weighted.mean(VAdata2020ZCTA$COPD_CrudePrev,VAdata2020ZCTA$TotalPopulation)
VACOPDresult <- t.test(VAdata2020ZCTA$COPD_CrudePrev)
VACOPDresult$estimate
VACOPDresult$conf.int

CAVdata2020ZCTA$COPD_CrudePrev
CAVdata2020ZCTA$COPD_Crude95CI

COPDpercentile <- ecdf(VAdata2020ZCTA$COPD_CrudePrev)
COPDpercentile(7.1)


#2020 Coronary heart disease
VAdata2020ZCTA$CHD_CrudePrev
mean(VAdata2020ZCTA$CHD_CrudePrev)
VACHDresult <- t.test(VAdata2020ZCTA$CHD_CrudePrev)
VACHDresult$estimate
VACHDresult$conf.int

CAVdata2020ZCTA$CHD_CrudePrev
CAVdata2020ZCTA$CHD_Crude95CI

CHDpercentile <- ecdf(VAdata2020ZCTA$CHD_CrudePrev)
CHDpercentile(6.7)


#2019 High Blood Pressure
VAdata2019ZCTA$BPHIGH_CrudePrev
mean(VAdata2019ZCTA$BPHIGH_CrudePrev)
VAHBPresult <- t.test(VAdata2019ZCTA$BPHIGH_CrudePrev)
VAHBPresult$estimate
VAHBPresult$conf.int

CAVdata2019ZCTA$BPHIGH_CrudePrev
CAVdata2019ZCTA$BPHIGH_Crude95CI

HBPpercentile <- ecdf(VAdata2019ZCTA$BPHIGH_CrudePrev)
HBPpercentile(41.2)


#2020 Asthma
VAdata2020ZCTA$CASTHMA_Crude95CI
VAdata2020ZCTA$CASTHMA_CrudePrev
Asthmapercentile <- ecdf(VAdata2020ZCTA$CASTHMA_CrudePrev)
CAVdata2020ZCTA$CASTHMA_CrudePrev
Asthmapercentile(10.7)


#2020 Diabets
VAdata2020ZCTA$DIABETES_CrudePrev
Diabetespercentile <- ecdf(VAdata2020ZCTA$DIABETES_CrudePrev)
CAVdata2020ZCTA$DIABETES_CrudePrev
CAVdata2020ZCTA$DIABETES_Crude95CI
Diabetespercentile(14.9)

##Socioeconomic
SES <- read_excel("Cavalier Manor Socioeconomic Data.xlsx")
