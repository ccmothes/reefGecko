### Start new Reef Gecko Analysis
#Reptiles first, but adding back in other data columns Chris wants
#Remember to keep lat/long, locality description, family, uncertainty, year, presence vs observ

#May as well download new GBIF dataset first that's more recent since starting over anyway
#downloading for US only, reptiles, no geospatial issue, has coordinates
#don't forget to note citation for GBIF download

#need to get text file from gbif into excel

ReptileOcc<-read.delim("ReptileOccurrenceNew.txt", quote = "")
write.csv(ReptileOcc, file = "NewReptile.csv")

#try organizing in R instead of Excel using "Select" in dplyr package
View(ReptileOcc)
GBIFReptileSelect<-select(ReptileOcc, year, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, family, species, basisOfRecord, locality, verbatimLocality)
View(GBIFReptileSelect)
write.csv(GBIFReptileSelect, file = "ShortenedReptile.csv")

#ShortenedReptile is the file without all the extra columns, but it still has blanks
#get rid of anything with "species" column as blank

ShortenedReptile<-read.csv("ShortenedReptile.csv")
ReptileNoBlankSpp<-subset(ShortenedReptile, (! (species == "")))

#now let's write this one without blank species into an excel file
write.csv(ReptileNoBlankSpp, file = "ReptileNoBlankSpp.csv")

#need to delete all the occurrences that are fossil records
reptilegetridoffossils<-read.csv("ReptileNoBlankSpp.csv")
ReptileNoFossils<-subset(reptilegetridoffossils, (! (basisOfRecord == "FOSSIL_SPECIMEN")))

#now we should have one ready to delete wonky coordinates
write.csv(ReptileNoFossils, file = "ReptileNoFossils.csv")

############SKIP TO THIS PART FROM HERE ON OUT JUST LOAD IN THIS EXCEL FILE#######
#load in the final one with the weird reef geckos deleted (this is the one to use to get elevations)
ReptileFinal<-read.csv("ReptileFinal.csv")

###now need to convert to spatial points dataframe
#load packages sp and spdep first
ReptileSpatial<-SpatialPointsDataFrame(ReptileFinal[,c("decimalLongitude", "decimalLatitude"),], data = ReptileFinal, proj4string = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

#now to find the elevation per occurrence
#need to load some packages to be able to do this
#first make sure you have loaded in the maps to have elevation data

elevation<-raster("mn30_grd")
View(elevation)
plot(elevation)

#also load in raster package
#figure out elevation per occurrence
ReptileElevation<-extract(elevation, ReptileSpatial)
View(ReptileElevation)

#now add this elevation column to the other sheet
#############IGNORE THIS##########################
################# add elevation to the other sheet (ReptileFinal) instead of adding all the others to elevation
#let's try. probably still make dataframe first?
data.reptile.elevation<-as.data.frame(ReptileElevation)
View(data.reptile.elevation)
ReptileFinal$Elevation<-data.reptile.elevation
View(ReptileFinal)

#yes! it looks right!
#now let's make this final one into a csv to make life easier

write.csv(ReptileFinal, file = "ReptileWithElevation.csv")

#so now this new excel file is the one with elevation and can be used from here on
##############################################################################
#csv taking forever to write, so let's try it the other way instead
#restart here
#first make it into dataframe
data.reptile.elevation<-as.data.frame(ReptileElevation)
View(data.reptile.elevation)
#add other columns to this data frame instead of vice versa
data.reptile.elevation$species<-ReptileFinal$species
data.reptile.elevation$family<-ReptileFinal$family
data.reptile.elevation$ExcelID<-ReptileFinal$ExcelID
data.reptile.elevation$year<-ReptileFinal$year
data.reptile.elevation$decimalLatitude<-ReptileFinal$decimalLatitude
data.reptile.elevation$decimalLongitude<-ReptileFinal$decimalLongitude
data.reptile.elevation$coordinateUncertaintyInMeters<-ReptileFinal$coordinateUncertaintyInMeters
data.reptile.elevation$basisOfRecord<-ReptileFinal$basisOfRecord
data.reptile.elevation$locality<-ReptileFinal$locality

View(data.reptile.elevation)
#this looks like it's working so let's try to convert this one to csv instead
write.csv(data.reptile.elevation, file = "ReptileOccurrenceWithElevation.csv")

#GREAT now we have a csv file

#reorganized/ renamed it in excel
#use CSV called "ReptileOccurrenceElevationOrganized" for reference/ give to Chris for checking stuff





#START HERE FOR GETTING MAX/MEAN ELEVATION PER SPECIES
#######################

#make new CSV with less information to use for pulling out max/mean per species
#make the new file in excel cause easier

###file to use is ReptileElevationData.csv

#Great! now let's sort it by species and find the max elevation per species

#######sort and pull out max
#add packages needed (doBy)


SortedReptileElevation<-read.csv("ReptileElevationData.csv", row.names = 1)
View(SortedReptileElevation)
View(ReptileElevation)
MaxReptileElevation<-summaryBy(ReptileElevation ~ species, data=SortedReptileElevation, FUN= max, keep.names = TRUE)
#needed to use the numeric "ReptileElevation" to make this work. So, make sure they're still organized 
  #the same way or the numbers will be off

View(MaxReptileElevation)

#cool that worked. 
#let's try it this way to have both included

SortedReptileElevationMEAN<-summaryBy(ReptileElevation ~ species+family, data=SortedReptileElevation, FUN= mean)
View(SortedReptileElevationMEAN)
SortedReptileElevationMAX<-summaryBy(ReptileElevation ~ species+family, data=SortedReptileElevation, FUN= max)
View(SortedReptileElevationMAX)


#now get max and mean on the same sheet

SortedReptileElevationMEAN$MAX<-SortedReptileElevationMAX$ReptileElevation.max
View(SortedReptileElevationMEAN)

#want to rename "reptileelevation.mean" column to just say MEAN

colnames(SortedReptileElevationMEAN)[colnames(SortedReptileElevationMEAN)=="ReptileElevation.mean"] <- "MEAN"
View(SortedReptileElevationMEAN)
#YES okay, looks done!

#now just write it into a .csv file to send the two to Chris

write.csv(SortedReptileElevationMEAN, file = "FINALReptileElevationMaxAndMean.csv")


#so the 2 files to give to Chris are:
#ReptileOccurrenceElevationOrganized and FINALReptileElvationMaxAndMean


#Okay, needs a smaller version of the first file with only the 10 spp he is interested in
#so need to figure out how to only keep the information related to those species

CompleteReptileFile<-read.csv("ReptileOccurrenceElevationOrganized.csv")

View(CompleteReptileFile)
#only keep these species: Ophisaurus compressus, Caretta caretta, Lepidochelys kempii, Lepidochelys olivacea, 
#     Drymobius margaritiferus, Crocodylus acutus, Dermochelys coriacea, Hydrophis platurus, Malaclemys terrapin,
#     Nerodia clarkii, Sphaerodactylus notatus

SmallRep<-subset(CompleteReptileFile, species== "Ophisaurus compressus" | "Caretta caretta" | "Lepidochelys kempii" | "Lepidochelys olivacea" | "Drymobius margaritiferus" | "Crocodylus acutus" | "Dermochelys coriacea" | "Hydrophis platurus" | "Malaclemys terrapin" | "Nerodia clarkii" | "Sphaerodactylus notatus", 
                 select=ExcelID:locality)
#this didn't work.let's try it a different way
#try using dplyr package

SmallRep2<- filter(CompleteReptileFile, species %in% c("Ophisaurus compressus", "Caretta caretta", "Lepidochelys kempii", "Lepidochelys olivacea", "Drymobius margaritiferus", "Crocodylus acutus", "Dermochelys coriacea", "Hydrophis platurus", "Malaclemys terrapin",  "Nerodia clarkii", "Sphaerodactylus notatus"))  # %>% 
                     # pull(species) %>%     ###if want to check
 # unique()   ###if want to check

#### %>%  is pipe (control, shift, m) means "and then" 

View(SmallRep2)

write.csv(SmallRep2, file = "GBIF10Species.csv")


########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################

#now do same thing for amphibians






