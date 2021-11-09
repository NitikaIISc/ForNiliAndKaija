
#initialize working space
rm(list=ls()) #clean workspace
graphics.off() #close all plots

####################

library(move)#for downloading data
library(mapproj);library(ggmap) #these packages are necessary to work with google maps
library(spatsoc);library("asnipe");library("igraph"); # for working with the SN parts
library(reshape);library(data.table) #for the manual section where i build the SN myself
library(adehabitatLT);
library(rgdal)
library(rgeos)
library(sf)
library(moveVis)

#############variables##########

MaxSpeedPermited=120 #in movebank units (m/s??) anything above this will be filtered
roostbuffer = 50 #in metres
feedBuff = 100


#### key paramterer values ######
MaxSpeedPermited=120 #in movebank units (m/s??) anything above this will be filtered

#load('movebankPW.rdata')#the PW for movebank
VulturesToPlotMap=10:15 #1:length(unstackedOhad) #out of the vultures in the DB which one to plot? choose a few out of the 83
DistThreshold=50 #changed from 2000 in meters ---at what distance two indi are interacting ~ identifying another vulture on the Ohad 
#(colin pennyquick, 1974)
TimeThreshold='10 minutes' #in this format 'XX units'-- for spatsoc -- for Ohads, maybe 2 mins?? - timegroups - temporally overlapping
MinCoocurForValue=2; #miniimal number of coocurences for considering a viable pair- 
#given that they had the opportunity to interact at least 30 times we know they interacted-- 70 to 100 test sensitivity?- biologically relevant?
##from the data distribution?- start with arbitrary!

minCrossings=2

##add other libraries

################### reading data from movebank ###################

load("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/Complete2019_MinDurTracking_swapped/MoveStackDatasetOhad_2019.RData")

MoveStackDatasetOhad_complete2019<-MoveStackDatasetOhad_2019
summary(MoveStackDatasetOhad_complete2019)


#### just loooking on raw downloaded data ###

##copying this line down here from reading from movebank chunk:
unstackedOhad=split(MoveStackDatasetOhad_complete2019) #splitting Ohads data into individuals ---
class(unstackedOhad)


citations(MoveStackDatasetOhad_complete2019)

equalProj(MoveStackDatasetOhad_complete2019)
n.locs(MoveStackDatasetOhad_complete2019)
timeLag(MoveStackDatasetOhad_complete2019)
#too heavy but works: 
#plot(MoveStackDatasetOhad,  lwd=2, xlab="location_long", ylab="location_lat",col='blue',pch=5)

############# very basic filtering. stage 1  ##############
#without going to a data frame since i cannot go back and doing it one by one (another try)###
## creating an empty metadata storage df
TagsMetaData=setNames(data.frame(matrix(ncol =8, nrow = length(unstackedOhad))), 
                      c('name',"InitialPoints", "PercentThrown", "N_locs","TrackDurationDays","DaysBetweenStartEnd",'FirstDay','LastDay'))

#### Breeding data: December 2018/ Jan 2019 to June 2019 ##########

#### should I include December 2018??########
library(lubridate)

BreedingFeed_2019 <- lapply(unstackedOhad, subset,ground_speed<=5 & 
                              month(timestamp)==1 | month(timestamp)==2| month(timestamp)==3|
                              month(timestamp)==4| month(timestamp)==5| month(timestamp)==6|(month(timestamp)==12 & year(timestamp)==2018))

##Also included Dec 2018 in breeding season 
BreedingFeed_2019 <- BreedingFeed_2019[sapply(BreedingFeed_2019, function(x) dim(x)[1]) > 0] ##only keep non-empty dataframes
length(BreedingFeed_2019)

hist(unique(month(BreedingFeed_2019[[8]]@data$timestamp)), breaks = 13)

#sanity check
min(date(BreedingFeed_2019[[1]]@data$timestamp))
max(date(BreedingFeed_2019[[1]]@data$timestamp))


## converting back to a movestack
Breeding_MS<-moveStack(BreedingFeed_2019)
tz(MoveStackDatasetOhad_complete2019)
idData(Breeding_MS)
#Breeding_DF<-as.data.frame(Breeding_MS)

Breeding_DF <- methods::as(Breeding_MS, "data.frame")
Breeding_DF$DateOnly<-as.Date(as.character(Breeding_DF$timestamp));
head(Breeding_DF)

#########Removing Out of Israel Breeding locations before removing vultures recorded for inadequate duration #########
####################Pruning data to only select points falling inside Israel polygon  ################

setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files")
#FileName='201107_NewRoostsList.kml'
FileName='CutOffRegion.kml'
LayerName='Regional_polygon'
Outline = readOGR(dsn="C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files/CutOffRegion.kml", 
                  layer="CutOffRegion.kml")

plot(Outline)

class(Outline)
Outline@polygons  ##  WGS84

####  convert all Last Locations to a spatial points object

xy <- Breeding_DF[,c("coords.x1","coords.x2")]

Sp_InIsrael_Dataset <- SpatialPointsDataFrame(coords = xy, data = Breeding_DF,
                                              proj4string = CRS(projection(Breeding_MS)))

#pts_in<-Sp_allLastLoc_AllID[!is.na(over(Sp_allLastLoc_AllID,Outline)),]
Breed_INIsrael<-Sp_InIsrael_Dataset[complete.cases(over(Sp_InIsrael_Dataset, Outline)), ]
plot(Outline)
#plot(Breed_INIsrael, col="red", add=TRUE)

##now only keeping the rows with data from within Israel
head(Breed_INIsrael)

#MoveStackDataset2019[[-which(namesIndiv(MoveStackDataset2020)%in%all_INIsrael$ID)]]
Dataset_BreedInIsrael<-as.data.frame(Breed_INIsrael)##renaming only the InIsrael data of 2019 that falls within Israel as the dataset that will be used henceforth 

####Removing vultures that have fewer than 1/3rd of their duration recorded INSIDE ISRAEL ######
library(plyr)
IdByDateLocs_Br<-plyr::ddply(Dataset_BreedInIsrael, .(Dataset_BreedInIsrael$trackId, Dataset_BreedInIsrael$DateOnly), nrow)
colnames(IdByDateLocs_Br)<-c("trackId","DateOnly","no_Relocs")
DaysRecorded_Br<-as.data.frame(table(IdByDateLocs_Br$trackId))
colnames(DaysRecorded_Br)<-c("trackId", "DaysRec")
hist(DaysRecorded_Br$DaysRec)
max(DaysRecorded_Br$DaysRec)*(1/3) #1/3 of 213 max days

plot(DaysRec~trackId, data= DaysRecorded_Br, las=3, cex.axis=0.5)

Vultures_min71Days_Br<-subset(DaysRecorded_Br, DaysRec>max(DaysRecorded_Br$DaysRec)*(1/3))
Vultures_min71Days_Br$trackId<-factor(Vultures_min71Days_Br$trackId)

########## Subsetting Dataset to only include vultures that were recorded long enough ######
nrow(Dataset_BreedInIsrael)
length(unique(Dataset_BreedInIsrael$trackId))
length(unique(Dataset_BreedInIsrael$trackId))

Dataset_BrCoFeed_MinDays<-Dataset_BreedInIsrael[Dataset_BreedInIsrael$trackId %in% Vultures_min71Days_Br$trackId,]
Dataset_BrCoFeed_MinDays$trackId<-factor(Dataset_BrCoFeed_MinDays$trackId)
length(unique(Dataset_BrCoFeed_MinDays$trackId))

table(Dataset_BrCoFeed_MinDays$trackId)
#######################################################################
# use df2move to convert the data.frame into a moveStack
projection(MoveStackDatasetOhad_2019)
Movestacked_BrCoFeed<-df2move(Dataset_BrCoFeed_MinDays,
                              proj = projection(MoveStackDatasetOhad_2019), 
                              x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId",
                              data = Dataset_BrCoFeed_MinDays)

str(Movestacked_BrCoFeed)
summary(Movestacked_BrCoFeed)
colnames(Dataset_BrCoFeed_MinDays)
#unstacked_NBrCoFeed = NonBreedingCoFeed_2019
#unstacked_BrCoFeed = BreedingCoFeed_2019

unstacked_BrCoFeed<-split(Movestacked_BrCoFeed)

length(unstacked_BrCoFeed)



#### just loooking at raw downloaded data ####


############# very basic filtering. stage 1  ##############

## creating an empty metadata storage df
TagsMetaData=setNames(data.frame(matrix(ncol =8, nrow = length(unstacked_BrCoFeed))), 
                      c('name',"InitialPoints", "PercentThrown", "N_locs","TrackDurationDays","DaysBetweenStartEnd",'FirstDay','LastDay'))

library(lubridate)

for (indv in 1:length(unstacked_BrCoFeed) ){## loop on individuals, now in separate Move objects
  TagsMetaData$InitialPoints[indv]=  dim(unstacked_BrCoFeed[[indv]]@data)[1]  ##number of fixes for an individual
  
  TagsMetaData$name[indv]=          names(unstacked_BrCoFeed)[indv]
  plot(unstacked_BrCoFeed[[indv]],col='blue', type='b',main=paste("Indiv=",indv, ', name=',TagsMetaData$name[indv],sep=' '))#just simple plot of this individual's points
  #dim(unstacked_BrCoFeed[[indv]]@coords)
  
  ## removing unneeded columns
  VarsToRemove <- names(unstacked_BrCoFeed[[indv]]@data) %in% c("sensor_type_id","taxon_canonical_name","nick_name","earliest_date_born","sensor","optional",
                                                                 "sensor_type","mw_activity_count","eobs_accelerations_raw","eobs_acceleration_sampling_frequency_per_axis",
                                                                 "eobs_acceleration_axes","argos_valid_location_algorithm","argos_sensor_4","argos_sensor_3","argos_sensor_2",
                                                                 "argos_sensor_1","argos_semi_minor","argos_semi_major","argos_pass_duration","argos_orientation","argos_nopc",
                                                                 "argos_lat1","argos_lat2","1084088","argos_lon1","argos_lon2","argos_nb_mes","argos_nb_mes_120",
                                                                 "eobs_key_bin_checksum","eobs_fix_battery_voltage","eobs_battery_voltage","eobs_status",
                                                                 "eobs_start_timestamp","eobs_type_of_fix","eobs_used_time_to_get_fix","eobs_temperature",
                                                                 "gps_dop","magnetic_field_raw_x","magnetic_field_raw_y","magnetic_field_raw_z","ornitela_transmission_protocol",
                                                                 "tag_voltage","algorithm_marked_outlier","argos_altitude","argos_best_level","argos_lc","argos_iq",
                                                                 "argos_gdop","argos_error_radius","argos_calcul_freq","location_lat.1","location_long.1","timestamps","height_raw",
                                                                 "barometric_pressure","barometric_height","battery_charging_current","eobs_activity","manually_marked_outlier",
                                                                 "eobs_activity_samples", "acceleration_raw_y", "battery_charge_percent", "data_decoding_software","gps_vdop","height_above_ellipsoid",
                                                                 'acceleration_raw_x','acceleration_raw_z',"acceleration_raw_z","eobs_horizontal_accuracy_estimate","eobs_speed_accuracy_estimate");  
  
  
  unstacked_BrCoFeed[[indv]]@data=unstacked_BrCoFeed[[indv]]@data[!VarsToRemove]  ##all columns except above listed unnecessary ones
  dim(unstacked_BrCoFeed[[indv]]@data)#colunms removed?
  
  ## filtering: choosing indices to keep for this individual
  
  indx=1:dim(unstacked_BrCoFeed[[indv]]@data)[1] #starting with all points a
  if(sum(unstacked_BrCoFeed[[indv]]@data$heading <= 360,na.rm=T)){#do i have heading data or this one?
    
    indx=intersect(indx,which(unstacked_BrCoFeed[[indv]]@data$heading <= 360))} #if yes, now index include only points with realistic heading 
  
  if(sum(unstacked_BrCoFeed[[indv]]@data$ground_speed<=MaxSpeedPermited,na.rm=T)){#below threshhold speed?
    indx=intersect(indx,which(unstacked_BrCoFeed[[indv]]@data$ground_speed<=120))}
  #if(sum(unstacked_BrCoFeed[[indv]]@data$gps_time_to_fix<=89,na.rm=T)){
  #   indx=intersect(indx,which(unstacked_BrCoFeed[[indv]]@data$gps_time_to_fix<=89))}
  if(sum(unstacked_BrCoFeed[[indv]]@data$gps_satellite_count>=3,na.rm=T)){#enough satellite numbers?
    indx=intersect(indx,which(unstacked_BrCoFeed[[indv]]@data$gps_satellite_count>=3))}
  
  
  ## subsetting the different slots of this move object
  print(paste("indiv",indv,"name",TagsMetaData$name[indv],'. I throw out', TagsMetaData$InitialPoints[indv]-length(indx), 'points, out of',TagsMetaData$InitialPoints[indv]))
  TagsMetaData$PercentThrown[indv]=(TagsMetaData$InitialPoints[indv]-length(indx))/TagsMetaData$InitialPoints[indv]
  
  unstacked_BrCoFeed[[indv]]@timestamps=unstacked_BrCoFeed[[indv]]@timestamps[indx]
  #unstacked_BrCoFeed[[indv]]@idData
  unstacked_BrCoFeed[[indv]]@sensor=unstacked_BrCoFeed[[indv]]@sensor[indx]
  unstacked_BrCoFeed[[indv]]@data=unstacked_BrCoFeed[[indv]]@data[indx,]
  #unstacked_BrCoFeed[[indv]]@coords.nrs
  if(dim(unstacked_BrCoFeed[[indv]]@data)[1]>1){ ##Nitika = to avoid stalling the loop if not enough data locations
    unstacked_BrCoFeed[[indv]]@coords=unstacked_BrCoFeed[[indv]]@coords[indx,]
    unstacked_BrCoFeed[[indv]]@bbox[1,]=range(unstacked_BrCoFeed[[indv]]@coords[,1]);unstacked_BrCoFeed[[indv]]@bbox[2,]=range(unstacked_BrCoFeed[[indv]]@coords[,2])
    ##latitudes and logitudes min max == bounding box
    #unstacked_BrCoFeed[[indv]]@proj4string
    
    
    ## collecting metadata and plotting fitered track: 
    TagsMetaData$N_locs[indv]=  dim(unstacked_BrCoFeed[[indv]]@data)[1]
    TagsMetaData$TrackDurationDays[indv]=  length(unique(as.Date(as.character(unstacked_BrCoFeed[[indv]]@data$timestamp))))
    #TagsMetaData$DaysBetweenStartEnd[indv]=  (max(as.Date(as.character(unstacked_BrCoFeed[[indv]]@data$timestamp)))-min(as.Date(as.character(unstacked_BrCoFeed[[indv]]@data$timestamp))))
    TagsMetaData$FirstDay[indv]=as.character(min(as.Date(as.character(unstacked_BrCoFeed[[indv]]@data$timestamp))));
    TagsMetaData$LastDay[indv]= as.character(max(as.Date(as.character(unstacked_BrCoFeed[[indv]]@data$timestamp))));
    TagsMetaData$DaysBetweenStartEnd[indv]=as.Date(TagsMetaData$LastDay[indv])-as.Date(TagsMetaData$FirstDay[indv]);
    lines(unstacked_BrCoFeed[[indv]],col='red')
    #plot(unstacked_BrCoFeed[[indv]], type="o", col=3, lwd=2, pch=20, xlab="location_long", ylab="location_lat")
    
    ##logging metadata
    
    head(timeLag(unstacked_BrCoFeed[[indv]], units="mins"))
    head(timestamps(unstacked_BrCoFeed[[indv]]))
  }
}#loop on individuals


################## making coords of feeding locations consistent with UTM   ######

BackStacked_BrCoFeed2019=moveStack(unstacked_BrCoFeed)

Dataset2019=as.data.frame(BackStacked_BrCoFeed2019) #converting to a dataset with all indis and their rows

################## getting all non-flying location ####################

#############   only 2019 datapoints that are at a ground_speed<5 
## check for each date which non-flight datapoints fell inside the food polygons simulataneously on a daily basis
### remove T59w because the tag seems to be working weirdly

Dataset2019 <- subset(Dataset2019, trackId != "T59w")
unique(Dataset2019$trackId)

NonFlyingPts <- subset(Dataset2019, heading <= 360 & gps_time_to_fix<=89 & ground_speed<=5 & gps_satellite_count>=3  )


##############    Remove last locations that fall outside Orr's Israel boundary - no need to remove juveniles then  ##########

setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files")
FileName='CutOffRegion.kml'
LayerName='Regional_polygon'
Outline = readOGR(dsn="C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files/CutOffRegion.kml", 
                  layer="CutOffRegion.kml")

plot(Outline)

class(Outline)
#head(Non_flightMinusNightRoosts) ## WGS84
Outline@polygons  ##  WGS84

####  convert all Last Locations to a spatial points object

xy <- NonFlyingPts[,c("location_long","location_lat")]

Sp_NonFlyingPts_df <- SpatialPointsDataFrame(coords = xy, data = NonFlyingPts,
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

#pts_in<-Sp_allLastLoc_AllID[!is.na(over(Sp_allLastLoc_AllID,Outline)),]
NonFlyingPts_df_IN<-Sp_NonFlyingPts_df[complete.cases(over(Sp_NonFlyingPts_df, Outline)), ]
plot(Outline)
plot(NonFlyingPts_df_IN, col="red", add=TRUE)



#####################Import feeding locations  ####################



setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/food")


FoodSites = read.csv("FeedingSites_AllActiveSouthNorth.csv") ##Also additional south sites that Nili emailed later
head(FoodSites)
summary(FoodSites)

unique(FoodSites$FoodSite)

################## making coords of feeding locations consistent with UTM   ######

BackStacked_BrCoFeed2019=moveStack(unstacked_BrCoFeed)
#preparing a dataframe for mapping:
Dataset2019=as.data.frame(BackStacked_BrCoFeed2019) #converting to a dataset with all indis and their rows
tail(Dataset2019)
Dataset2019$DateOnly=as.Date(as.character(Dataset2019$timestamp));


##for last locations of vultures on each night:
projection(BackStacked_BrCoFeed2019)#this was the original data projection from movebank
class(FoodSites)
colnames(FoodSites)[6:7]<-c("WGS_lat","WGS_long")
FoodSitesF_wgs=FoodSites;
coordinates(FoodSitesF_wgs)<-~WGS_long+WGS_lat
proj4string(FoodSitesF_wgs)<-CRS(projection("+proj=longlat +datum=WGS84 +no_defs"))
#proj4string(FoodSitesF_wgs)<-CRS(projection("+proj=utm +zone=10+datum=WGS84"))
#FoodSites last location projection is the same as BackStacked_BrCoFeed2019 i.e. WGS84

##converting to UTM

#proj4string(x) <-CRS("+proj=utm +zone=10+datum=WGS84")
#newData <- spTransform(x, CRS("+init=epsg:4238"))
utmS <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' #south, but most points are in the north

utmN <- '+proj=utm +zone=36        +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  #north

## converting to UTM36North, but note that not all points are within, just the majority
FoodSitesF_utmN_Poly <- spTransform(FoodSitesF_wgs, CRS(utmN))
head(coordinates(FoodSitesF_utmN_Poly))#now the lat long are in metric
# just plotting the new dataset to see the locations look fine: plot(Dataset2019_utmN,col='blue')

## appending the new coordinates in easting northing- for calculating distance UTM locally
FoodSites$Easting_poly=coordinates(FoodSitesF_utmN_Poly)[,1]
FoodSites$Northing_poly=coordinates(FoodSitesF_utmN_Poly)[,2]


######    creating a circular buffer of radius = 100m #############
coords <- FoodSites[ , c("Easting_poly", "Northing_poly")]   # coordinates
data   <- FoodSites[ , 1:4]          # data
##crs    <- CRS("+init=epsg:28992") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = data, 
                               proj4string = CRS(utmN))

## make spatial points
dat_sf <- st_as_sf(FoodSites, coords = c("Easting_poly", "Northing_poly"), crs = utmN) 

FoodPolys = st_buffer(dat_sf, dist = feedBuff)
FoodPolys$geometry
nrow(FoodSites)
##add buffer to spatialPolygonDataFrame in UTM projection:
plot(FoodPolys$geometry[c(1:2)], border='blue')

class(FoodPolys$geometry)
FoodPolys$FoodSite
##Getting coordinates of circular buffer around feeding stations 
FoodBuffCoods = as.data.frame(st_coordinates(FoodPolys))
colnames(FoodBuffCoods)<-c("Buff_long","Buff_lat","L1","Sno")

Sites = as.data.frame(cbind(FoodPolys$Sno, as.character(FoodPolys$FoodSite)))
colnames(Sites)<-c("Sno", "StationName")

allPolysFood = merge(FoodBuffCoods, Sites, by = "Sno")

###projecting food buffer boundaries to wgs just for visualizing

projection(BackStacked_BrCoFeed2019)#this was the original data projection from movebank
allPolysFood_utm=allPolysFood;
coordinates(allPolysFood_utm)<-~Buff_long+Buff_lat
proj4string(allPolysFood_utm)<-CRS(projection(utmN))


allPolysFood_wgs = spTransform(allPolysFood_utm, CRS(projection("+proj=longlat +datum=WGS84 +no_defs"))) ##transformed utm to wgs
#allPolysFood_wgs = spTransform(allPolysFood_utm, CRS(projection("+proj=utm +zone=10+datum=WGS84"))) ##transformed utm to wgs

## appending the new coordinates in easting northing- for calculating distance UTM locally
allPolysFood$wgsLong=coordinates(allPolysFood_wgs)[,1]
allPolysFood$wgsLat=coordinates(allPolysFood_wgs)[,2]

setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files")
#write.csv(as.data.frame(allPolysFood_wgs), "AllFeedingStnBufferPolys.csv")


#### Importing all roost polygons ######
setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files")
FileName='20210324_RoostsNili.kml'
LayerName='Roosting'
Roosts = readOGR(dsn=FileName, layer=LayerName)
head(Roosts)
summary(Roosts)
nrow(Roosts)
length(levels(Roosts$Name))
length(unique(Roosts$Name))
subset(Roosts, Name =="Missed_negev")


allRoostCoods = data.frame()
for (i in 1:length(Roosts)){
  roost1 = as.data.frame(cbind((as.character(Roosts@data[i,1])),Roosts@polygons[[i]]@Polygons[[1]]@coords))
  allRoostCoods = rbind(allRoostCoods, roost1)
}

colnames(allRoostCoods)<-c("Site","x","y") 
allRoostCoods<-allRoostCoods[,c("x","y","Site")]
allRoostCoods$x<-as.numeric(as.character(allRoostCoods$x))
allRoostCoods$y<-as.numeric(as.character(allRoostCoods$y))




allFoodCoods<-as.data.frame(allPolysFood_wgs)[,c("Buff_long","Buff_lat","StationName")]
colnames(allFoodCoods)<-c("x","y","Site") 
head(allFoodCoods)
head(allRoostCoods)


### joining roost polgons with Food buffer polygons

library(sf)
class(allPolysFood_wgs)

# convert into st_polygon friendly format (all polygons must be closed)
####polygons

#############IF ONLY LOOKING AT ALL NON-FLIGHT INTERACTIONS EXCEPT WITHIN ROOSTS######
#localpolydf<-as.data.frame(rbind(allFoodCoods, allRoostCoods))
localpolydf<-as.data.frame(allRoostCoods)
nrow(allFoodCoods)
nrow(allRoostCoods)
nrow(roostpolydf)



localpoly <- localpolydf %>% split(localpolydf$Site) %>% 
  lapply(function(x) rbind(x,x[1,])) %>%
  lapply(function(x) x[,1:2]) %>%
  lapply(function(x) list(as.matrix(x))) %>%
  lapply(function(x) st_polygon(x))





###points
offsetdf<-as.data.frame(NonFlyingPts_df_IN)[,c("coords.x1","coords.x2","trackId")]
#offsetdf<-NonFlyingPts_df_IN
# convert points into sf object
points <- st_as_sf(offsetdf,coords=c('coords.x1','coords.x2'),remove = F)
class(points)
length(points$geometry)

#convert polygons to sf object and add id column
polys <- localpoly %>% st_sfc() %>% st_sf(geom=.) %>% 
  dplyr::mutate(Stn=factor(unique(localpolydf$Site))) 

#find intersection
# Joined is the points falling inside roost polygons
library(sf)
joined <- polys  %>% st_intersection(points) 

#### selecting points falling OUTSIDE food buffers:
#st_intersects which returns a list of the same length as nc_point. If the point isn't in a polygon then it returns integer(0) elements.

#So you can any of a few ways of turning that output into TRUE/FALSE to then select your points. For example, look for length-0 elements in the list:

NonFlyOutRoostFood<-sapply(st_intersects(points, polys),function(x){length(x)==0})

FeedingOutRoostOnly<-points[NonFlyOutRoostFood,]

class(FeedingOutRoostOnly)

nrow(FeedingOutRoostOnly %>% st_drop_geometry())
nrow(FeedingOutRoostOnly %>% st_drop_geometry())-nrow(points %>% st_drop_geometry())
#79981-9901

setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files")
#st_write(FeedingOutRoostOnly, "ExtraRoostOnlyFeedingSites.kml", driver = "kml", delete_dsn = TRUE)

setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/Complete2019_MinDurTracking_swappedMasks/CoFeeding2019_MinDur/Breeding")
write.csv(FeedingOutRoostOnly, "Breeding_MinDur_NonFlyingPtsOutsideRoostOnly.csv")

nrow(NonFlyingPts_df_IN)
nrow(NonFlyingPts)
nrow(FeedingOutRoostOnly)
#######################   USING THESE outside roost and outside food points to check spatial proximity and deduce interactions##################
########################      brought in physical proximity code as is ###########################

# Apr 23rd :-) 2019 trying to read movebank data using move


#### loading packages #######
library(move)#for downloading data
library(mapproj);library(ggmap) #these packages are necessary to work with google maps
library(spatsoc);library("asnipe");library("igraph"); # for working with the SN parts
library(reshape);library(data.table) #for the manual section where i build the SN myself
library(adehabitatLT);
source('C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyCodes/createDirectedMatrices.R')

####Make sure to include the seasonal non-flying out-roost points only #####
setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/Complete2019_MinDurTracking_swappedMasks/CoFeeding2019_MinDur/Breeding")
FeedingOutRoostOnly<-read.csv("Breeding_MinDur_NonFlyingPtsOutsideRoostOnly.csv")

Dataset<-FeedingOutRoostOnly
library(data.table)
setDT(Dataset, keep.rownames = TRUE)[]
head(Dataset)
colnames(Dataset)<-c("Row","coords.x1","coords.x2", "trackId", "geometry1","geometry2")

head(Dataset)
nrow(Dataset)

NonFlyingPts_IN<-as.data.frame(NonFlyingPts_df_IN)
library(data.table)
setDT(NonFlyingPts_IN, keep.rownames = TRUE)[]
colnames(NonFlyingPts_IN)[1]<-"Row"
head(NonFlyingPts_IN)


Dataset_NonFlightF<-dplyr::left_join(Dataset, NonFlyingPts_IN, by = c("Row"))
nrow(Dataset_NonFlightF)
nrow(Dataset)
nrow(NonFlyingPts_IN)

############    pruned dataset within Israel to move object
head(Dataset_NonFlightF)
library(moveVis)
library(move)
projection(BackStacked_BrCoFeed2019)
BackStacked_NonFlight = df2move(Dataset_NonFlightF,
                                proj = CRS("+proj=longlat +datum=WGS84 +no_defs"), 
                                x = "coords.x1.x", y = "coords.x2.x", time = "timestamps", track_id = "trackId.x")



Dataset_NonFlightF_ltraj=move2ade(BackStacked_NonFlight)
Dataset_NonFlightF=as.data.frame(BackStacked_NonFlight) #converting to a dataset F for filtered
Dataset_NonFlightF=data.table::setDT(Dataset_NonFlightF,keep.rownames=T)#and now to a data.table

## renaming\adding columns
Dataset_NonFlightF$ID=Dataset_NonFlightF$trackId
Dataset_NonFlightF$location_long1=Dataset_NonFlightF$coords.x1
Dataset_NonFlightF$location_lat1=Dataset_NonFlightF$coords.x2

## adding coordinates with a metric value ##### #
## first setting the same coordinate system as in movebank
projection(BackStacked_NonFlight)#this was the original data projection from movebank
Dataset_NonFlightF_wgs=Dataset_NonFlightF;
coordinates(Dataset_NonFlightF_wgs)<-~coords.x1+coords.x2
proj4string(Dataset_NonFlightF_wgs)<-CRS(projection(BackStacked_NonFlight))


utmS <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' #south, but most points are in the north
utmN <- '+proj=utm +zone=36        +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  #north

## converting to UTM36North, but note that not all points are within, just the majority
Dataset_NonFlightF_utmN <- spTransform(Dataset_NonFlightF_wgs, CRS(utmN))
head(coordinates(Dataset_NonFlightF_utmN))#now the lat long are in metric
# just plotting the new dataset to see the locations look fine: plot(Dataset_NonFlightF_utmN,col='blue')

## appending the new coordinates in easting northing- for calculating distance UTM locally
Dataset_NonFlightF$Easting=coordinates(Dataset_NonFlightF_utmN)[,1]
Dataset_NonFlightF$Northing=coordinates(Dataset_NonFlightF_utmN)[,2]


#View(Dataset_NonFlightF)

##spatsoc- timegroups
start_time <- Sys.time()
## using spatsoc for groupping into time groups








######      Earlier  Sensitivity analysis for non-flying distance threshold    ############


library(spatsoc)

##spatsoc- timegroups
start_time <- Sys.time()
## using spatsoc for grouping into time groups
#class(Dataset_NonFlightF$time)

group_times(Dataset_NonFlightF, datetime = 'timestamps', threshold = TimeThreshold) 
##     - group all datapoints that fall within 10 minutes of each other 
##      - timestamp is date time format 'POSIXct' as required by spatsoc
##      - group_times returns the input DT appended with a timegroup column and additional temporal grouping columns to help investigate, troubleshoot and interpret the timegroup.
## 

## using spatsoc for grouping into spatial groups with the metric values --- using northing and easting
tail(Dataset_NonFlightF)

group_pts(Dataset_NonFlightF, threshold = DistThreshold, id = 'ID', coords = c('Northing', 'Easting'), timegroup = 'timegroup')

#use group_lines instead??

##Nitika  - The timegroup argument is optional, but recommended to pair with group_times. 
#The intended framework is to group rows temporally with group_times then 
#spatially with group_pts (or group_lines, group_polys)

#group_pts returns the input DT appended with a group column.

#This column represents the spatial (and if timegroup was provided - 
#spatiotemporal) group. As with the other grouping functions, 
#the actual value of group is arbitrary and represents the identity of a 
#given group where 1 or more individuals are assigned to a group. 
#If the data was reordered, the group may change, but the contents of each group would not.

end_time <- Sys.time()

end_time - start_time
#

####Removing pre-existing calculated interactions at previous distance threshold  ####

rm('SRIlongform')
rm('CoocurCountr')
rm('SimlDataPntCnt')

##### manual distance calculation and SN construction #####
SimlDataPntCnt   = expand.grid(unique(as.character(Dataset_NonFlightF$ID)),unique(as.character(Dataset_NonFlightF$ID)))
#a long form of all possible dyads to count interaction
#expand.grid = Create a data frame from all combinations of the supplied vectors or factors.

SimlDataPntCnt$counter=0;names(SimlDataPntCnt)=c('ID','ID2','counter') ##setting empty matrix of dimensions pf ID1*ID2
##counter is to see if they co-occur


CoocurCountr = SimlDataPntCnt
#a long form of all possible dyads to count intervals both were at the same timegroup
#creating empty matrix for SRI
SRIlongform=CoocurCountr;names(SRIlongform)[3]='SRI'#--- exhaustive pair-wise
subset(SRIlongform, SRI != 0)

#if (CountTwice) Add=0.5 else Add=1
start_time <- Sys.time()

source('C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyCodes/createDirectedMatrices.R')

matrices = createDirectedMatrices(Dataset_NonFlightF, DistThreshold)
end_time <- Sys.time()

end_time - start_time ##Calculate total time taken in run

SimlDataPntCnt = matrices[[1]]
CoocurCountr = matrices [[2]]
head(SimlDataPntCnt,45)
subset(SimlDataPntCnt, ID=="A74.T02.white"|ID=="S75.T03.white" & ID2=="A74.T02.white"|ID2=="S75.T03.white" )
subset(SimlDataPntCnt, counter == 67)
nrow(SimlDataPntCnt)
nrow(CoocurCountr)

# Time difference of  mins

##    SRIlongform = actual co-occurence / opportunities of co-occurences
##    SimlDataPntCnt = opportunities of co-occurences
##    CoocurCountr = actual co-occurence


CoocurCountr$counter = ifelse(CoocurCountr$counter<MinCoocurForValue, 0, CoocurCountr$counter) #eliminating dyads which co-occured less than the minimum cut-off

### Now the self dyads aren't counted twice but the non-self are separately counted but we need to aggregate them
##but first change the ID and ID2 so that order becomes immaterial

##order of ID1 & ID2 shouldn't matter
library(igraph)
nrow(SimlDataPntCnt)
SimlDataPntCnt_ordered = get.data.frame(graph.data.frame(SimlDataPntCnt[,c('ID','ID2')], directed=FALSE),"edges") 
SimlDataPntCnt$ID=SimlDataPntCnt_ordered[,1]
SimlDataPntCnt$ID2=SimlDataPntCnt_ordered[,2]
### I did not aggregate the values of co=occurence because in a given time when A and B co-occured it doesn't matter for us if A co-occured with B or B co-occured with A
SimlDataPntCnt=unique(SimlDataPntCnt)
nrow(SimlDataPntCnt)

library(dplyr)

library(igraph)
nrow(CoocurCountr)
CoocurCountr_ordered = get.data.frame(graph.data.frame(CoocurCountr[,c('ID','ID2')], directed=FALSE),"edges") 
CoocurCountr$ID=CoocurCountr_ordered[,1]
CoocurCountr$ID2=CoocurCountr_ordered[,2]
CoocurCountr=unique(CoocurCountr)
nrow(CoocurCountr)

summary(SRIlongform)# ok now no values above 1.... typo corrected

##order of ID1 & ID2 shouldn't matter
library(igraph)
nrow(SRIlongform)
SRIlongform_ordered = get.data.frame(graph.data.frame(SRIlongform[,c('ID','ID2')], directed=FALSE),"edges") 
SRIlongform$ID=SRIlongform_ordered[,1]
SRIlongform$ID2=SRIlongform_ordered[,2]
SRIlongform=unique(SRIlongform)
nrow(SRIlongform)

SRIlongform$SRI=as.numeric(CoocurCountr$counter/SimlDataPntCnt$counter)# ratio of number of co-occurences/number of simluatnous datapoints 
#SRIlongform has exhaustive dyads which may or may not have had the opportunity to co-occur
hist(SRIlongform$SRI);range(SRIlongform$SRI,na.rm=T)


SRI_mrtx=as.matrix(tidyr::spread(data=SRIlongform, key= ID2, value=SRI)) ##ID2 is key meaning it becomes column name in the matrix
Coocur_mrtx=as.matrix(tidyr::spread(data=CoocurCountr, key= ID2, value=counter))

#rownames(SRI_mrtx)=SRI_mrtx[,1];SRI_mrtx=SRI_mrtx[,-1]#just setting row names from the dataframe
NamesInMtrx=SRI_mrtx[,1];SRI_mrtx=SRI_mrtx[,-1]#just setting row names from the dataframe
M=mapply(SRI_mrtx, FUN=as.numeric)#converting to numeric matrix
SRI_mrtx<- matrix(data=M, ncol=ncol(SRI_mrtx), nrow=nrow(SRI_mrtx))
rownames(SRI_mrtx)=NamesInMtrx;colnames(SRI_mrtx)=NamesInMtrx

Diag=diag(SRI_mrtx);unique(Diag);#self, indeed always 1, lets remove them:
diag(SRI_mrtx)=NA
#just for testing: replace NA with zero 0:  SRI_mrtx[is.na(SRI_mrtx)] <- 0
MaxIndSRI=apply(SRI_mrtx,2,max, na.rm=T)#the max value in each row
#second argument in apply is 'margin', here = 2; a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns
#How much does ID1 overlap most often with any ID2? 
#for example max(SRI_mrtx[,3], na.rm = T)
#[1] 0.3080808


print(paste('even after removing diagonal self connection there are still', sum(MaxIndSRI==1),'fully connected dyads with MinCoocurForValue of',MinCoocurForValue))


#    }                          
#  }
#}

setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/Complete2019_MinDurTracking_swappedMasks/CoFeeding2019_MinDur/Breeding")
write.csv(SimlDataPntCnt,"20210831_BrNonFlightNonRoostOnly_SimlDataPntCnt.csv")
write.csv(CoocurCountr,"20210831_BrNonFlightNonRoostOnly_CoocurCountr.csv")
write.csv(SRIlongform,"20210831_BrNonFlightNonRoostOnly_SRIlongform.csv")

###########       Co-Feeding network    #######################
setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/Complete2019_MinDurTracking_swappedMasks/CoFeeding2019_MinDur/Breeding")
SRIlongform <- read.csv("20210831_BrNonFlightNonRoostOnly_SRIlongform.csv")

range(SRIlongform$SRI, na.rm = TRUE)
hist(SRIlongform$SRI)

CoFeedEdgelist=SRIlongform[,c("ID","ID2","SRI")]
colnames(CoFeedEdgelist)[3]<-"wt"

thresh=0

CoFeedSubEdgelist<-subset(CoFeedEdgelist,wt>thresh & ID != ID2)

nrow(CoFeedEdgelist)
nrow(CoFeedSubEdgelist)
el=as.matrix(CoFeedEdgelist[,1:3]) #igraph needs the edgelist to be in matrix format

g=graph.edgelist(el[,1:2], directed=FALSE)#We first create a network from the first two columns, which has the list of vertices

el_subset = as.matrix(CoFeedSubEdgelist[,1:3]) #igraph needs the edgelist to be in matrix format

g_subset=graph.edgelist(el_subset[,1:2], directed=FALSE)

nrow(el_subset)
nrow(el)


#View(el)
##DIRECTION DOES NOT MAKE SENSE FOR ED
E(g)$weight=as.numeric(el[,3]) #We then add the edge weights to this network by assigning an edge attribute called 'weight'.

E(g_subset)$weight=as.numeric(el_subset[,3]) #We then add the edge weights to this network by assigning an edge attribute called 'weight'.

## no weights

l <- layout_in_circle(g) ##if you want the layout to be circular
V(g)$label <- V(g)$name
length(V(g)$name)

length(E(g)$weight)

#attr.net[i]<-c(list((cbind(levels(dat$nest)[i],V(g)$label , deg.o[[i]]))))
####FULL NETWORK

par(mar=c(0,0,1,0))

plot.igraph(simplify(g),
            layout=layout.fruchterman.reingold ##spring embedded
            ,vertex.color="Red"
            ,vertex.frame.color="black",
            vertex.size=12, ##in-degree means who got more food
            vertex.label=V(g)$name,
            edge.color= "gray"
            ,edge.width=E(g)$weight*0.0002
            ,main = "Co-NonFlight feeding network of vultures outside feeding stations in 2019"
            , edge.arrow.size=.4 ##don't want big arrowheads
            ,vertex.label.cex=0.8 ##label font size
            ,vertex.frame.width=4
            )


