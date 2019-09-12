#set working directory to external harddrive

#open new gbif set and then convert to excel

ReptileText2<-read.delim("Reptileoccurrence.txt", quote = "")
write.csv(ReptileText2, file = "AttemptReptile2.csv")
#this is after adding the quote part to deal with warning message that may or may not matter
write.csv(ReptileText2, file = "AttemptReptile3.csv")

#okay it's all better from using excel and ready to be loaded in. 
#got rid of all empties and such

Reptile3<-read.csv("AttemptReptile3EditedFixed.csv")
View(Reptile3)
#this looks good

###########let's put this through a coordinate cleaner

library(CoordinateCleaner)
CleanReptile<-clean_coordinates(Reptile3, lon = "decimalLongitude", lat = "decimalLatitude", species = "species",
                  countries = NULL, tests = c("capitals", "centroids", "equal", "gbif", "institutions",
                  "outliers", "seas", "zeros"), capitals_rad = 10000, centroids_rad = 1000, centroids_detail = "both",
                  inst_rad = 100, outliers_method = "quantile", outliers_mtp = 5, outliers_td = 1000,
                  outliers_size = 7, range_rad = 0, zeros_rad = 0.5, capitals_ref = NULL, centroids_ref = NULL,
                  country_ref = NULL, inst_ref = NULL, range_ref = NULL, seas_ref = NULL, seas_scale = 50,
                  urban_ref = NULL, value = "spatialvalid", verbose = TRUE, report= FALSE)
#write this as a csv for future reference
write.csv(CleanReptile, file = "cleanreptile.csv")

#make a new one with smaller buffer and just for ocean
CleanReptileSeasOnly<-clean_coordinates(Reptile3, lon = "decimalLongitude", lat = "decimalLatitude", species = "species",
                                countries = NULL, tests = c("seas"), country_ref = NULL, seas_ref = NULL, seas_scale = 110,
                                value = "spatialvalid", verbose = TRUE, report= FALSE)

write.csv(CleanReptileSeasOnly, file = "cleanreptileSeasOnly.csv")
#let's make a new sheet that gets rid of any points where the .sea column is false






########################################################

#time to make sure i have elevation and coastal rasters in here

#elevation first
#need to get raster function
elevation<-raster("mn30_grd")
View(elevation)
plot(elevation)

#next do coast
#using rnaturalearth for this one
coast<-ne_coastline()
plot(coast)

################################
###now convert reptile thing into spatial points dataframe
Reptile3CSVSpatial<-SpatialPointsDataFrame(Reptile3[,c("decimalLongitude", "decimalLatitude"),], data = Reptile3, proj4string = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
#didn't get any warnings so theoretically that worked
View(Reptile3CSVSpatial)
#got error when tried to view it:   #####"Error in if (more || nchar(output) > 80) { : 
##########################################missing value where TRUE/FALSE needed"
#so something is going wrong when converting to spatial points dataframe
#try plotting
plot(Reptile3CSVSpatial)
#when i plot it, it looks right so yay i think that actually did work


########IGNORE THESE###################################################################
#try different way
Reptile4Spatial<-SpatialPointsDataFrame(Reptile3[,c("decimalLongitude", "decimalLatitude")], data = Reptile3,
                                        proj4string = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
View(Reptile4Spatial)
#get same error

#try one other way
Reptile5Spatial<-st_as_sf(x=Reptile3,
                         coords = c("decimalLongitude", "decimalLatitude"),
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
View(Reptile5Spatial)
plot(Reptile5Spatial)
#this gives some other data frame, not spatial points
######################################################################################

#### Get elevation for each occurrence for reptiles
#now try to extract elevation from raster to points in spatial frame
library(raster)
ReptileElevation3<-extract(elevation, Reptile3CSVSpatial)
View(ReptileElevation3)
#got elevation for each one but lost the labeling of which species it each matches

#try to add species back in
#######################################################
#########IGNORE THIS
ReptileElevation3$species<-Reptile3$species
#got warning message: "In ReptileElevation3$species <- Reptile3$species : Coercing LHS to a list"
View(ReptileElevation3)
#this didn't work at all
#########################################################

#try again but make reptile elevation3 a data frame first maybe
data.reptile.elevation<-as.data.frame(ReptileElevation3)
View(data.reptile.elevation)
data.reptile.elevation$species<-Reptile3$species
View(data.reptile.elevation)
#good. now add in excel ID too to keep track
data.reptile.elevation$ExcelID<-Reptile3$Excel.ID
View(data.reptile.elevation)
#now check to make sure things still match proper excel ID's with correct spp
View(Reptile3)
#yes! it looks right!
#now let's make this final one into a csv to make life easier

write.csv(data.reptile.elevation, file = "ReptileElevation3Fixed.csv")

#Great! now let's sort it by species and find the max elevation per species

#######sort and pull out max
#add packages needed (doBy)


sortreptileelev3<-read.csv("ReptileElevation3Fixed.csv", row.names = 1)
View(sortreptileelev3)
printedmaxreptileelev3<-summaryBy(ReptileElevation3 ~ species, data=sortreptileelev3, FUN= max)
View(printedmaxreptileelev3)
#this looks right
write.csv(printedmaxreptileelev3, file = "ReptileMaxElevPerSpp3Fixed.csv")
#okay great, got it. but it's still telling me the max elevation for S. notatus is 31 which doesn't make sense
#from looking in arc map, points on GBIF are slightly off from where they should be.
#like points that say the keys show up on gulf coast of florida for example
#it's not an issue with the code extracting because it is extracting the correct values at
####where the points are showing up, but the points aren't projecting correctly onto the elevation
#okay it was a georeferencing error in gbif so i deleted those points

#######################################################################################
#######################################################################################

#NEED TO DO EVERYTHING OVER AGAIN ADDING BACK IN THE FAMILIES. CAN'T DO THIS IN EXCEL BECAUSE CAN'T SELECT BLANKS ANYMORE
#so load in sheet with species and families and then delete everything that is blank in spp column

reptilewithfamilies<-read.csv("AttemptReptile3WithFamilies.csv")

Reptilefamilynoblankspp<-subset(reptilewithfamilies, (! (species == "")))
View(Reptilefamilynoblankspp)

#convert to spatial points dataframe

ReptileFamilyCSVSpatial<-SpatialPointsDataFrame(Reptilefamilynoblankspp[,c("decimalLongitude", "decimalLatitude"),], data = Reptilefamilynoblankspp, proj4string = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

#figure out elevation per occurrence
ReptileElevationWithFamilies<-extract(elevation, ReptileFamilyCSVSpatial)
View(ReptileElevationWithFamilies)

#now add labels back in for elevation per occurrence
#make a data frame first maybe
data.reptile.elevation.families<-as.data.frame(ReptileElevationWithFamilies)
View(data.reptile.elevation.families)
data.reptile.elevation.families$species<-Reptilefamilynoblankspp$species
View(data.reptile.elevation.families)
#good. now add in excel ID too to keep track
data.reptile.elevation.families$ExcelID<-Reptilefamilynoblankspp$Excel.ID
View(data.reptile.elevation.families)
#now add in families to match each one
data.reptile.elevation.families$family<-Reptilefamilynoblankspp$family
View(data.reptile.elevation.families)

#now check to make sure things still match proper excel ID's with correct spp
View(Reptilefamilynoblankspp)
#yes! it looks right!
#now let's make this final one into a csv to make life easier

write.csv(data.reptile.elevation.families, file = "ReptileElevationWithFamilies.csv")

#Great! now let's sort it by species and find the max elevation per species

#######sort and pull out max
#add packages needed (doBy)


sortreptileElevationFamily<-read.csv("ReptileElevationWithFamilies.csv", row.names = 1)
View(sortreptileElevationFamily)
printedMaxReptileElevationFamily<-summaryBy(ReptileElevationWithFamilies ~ species, data=sortreptileElevationFamily, FUN= max, keep.names = TRUE)
View(printedMaxReptileElevationFamily)
#this looks right and gives me the max per species
#try to also get mean per species
printedMeanReptileElevationFamily<-summaryBy(ReptileElevationWithFamilies ~ species, data=sortreptileElevationFamily, FUN= mean)
View(printedMeanReptileElevationFamily)
######IGNORE THIS
#try to get both mean and max at same time
printedReptileElevationFamilyMeanMax<-summaryBy(ReptileElevationWithFamilies ~ species, data=sortreptileElevationFamily, FUN= c[mean, max])
#######not working

#try to just add the max column to data sheet with the mean
printedMeanReptileElevationFamily$MaxElevation<-printedMaxReptileElevationFamily$ReptileElevationWithFamilies.max
View(printedMeanReptileElevationFamily)
#cool that worked

###now add in families?
printedMeanReptileElevationFamily$family<-sortreptileElevationFamily$family
###this doesn't work
#maybe add families as a summary piece per species? idk if that will work since it's nonnumeric though
printedFamilyNameReptileElevationFamily<-summaryBy(family ~ species, data=sortreptileElevationFamily, FUN= mean)
View(printedFamilyNameReptileElevationFamily)
#this didn't work it came up with numbers... although i think each number represents a family.. how do i make it name the family?
#try using keep.names and see if it keeps name instead of number?
printedFamilyNameReptileElevationFamily<-summaryBy(family ~ species, data=sortreptileElevationFamily, FUN= max, keep.names = TRUE)
View(printedFamilyNameReptileElevationFamily)
#KEEP.NAMES doesn't seem to do anything
#I have no idea how to keep family names per species since it's no longer equal row numbers

###
printedReptileElevationFamilyspfam<-summaryBy(ReptileElevationWithFamilies ~ species+family, data=sortreptileElevationFamily, FUN= mean)
View(printedReptileElevationFamilyspfam)
printedReptileElevationFamilyspfamMAX<-summaryBy(ReptileElevationWithFamilies ~ species+family, data=sortreptileElevationFamily, FUN= max)
View(printedReptileElevationFamilyspfamMAX)

printedReptileElevationFamilyspfam$MAXIMUM<-printedReptileElevationFamilyspfamMAX$ReptileElevationWithFamilies.max
View(printedReptileElevationFamilyspfam)

write.csv(printedReptileElevationFamilyspfam, file = "FINALRepElevationwithFAMILY.csv")

write.csv(printedmaxreptileelev3, file = "ReptileMaxElevPerSpp3Fixed.csv")
