library(rgdal)
library(spdplyr)
library(sp)
library(leaflet)
library(shiny)
library(shinydashboard)
library(rgeos)
library(rmapshaper)
library(maptools)
library(gtools)

#Load Shapefiles
BRMA_England <- readOGR(dsn = "Input_Geographies", layer = "BRMA0412", GDAL1_integer64_policy = TRUE)
BRMA_Scotland <- readOGR(dsn = "Input_Geographies", layer = "Scotland", GDAL1_integer64_policy = TRUE)
BRMA_Wales <- readOGR(dsn = "Input_Geographies", layer = "wales_brma", GDAL1_integer64_policy = TRUE)

#Getting rid of unvanted attributes
BRMA_England <- BRMA_England %>% select("Name", "LOCALITY_I")
BRMA_Scotland <- BRMA_Scotland %>% select("Name", "LOCALITY_I")
BRMA_Wales <- BRMA_Wales %>% select("brma", "brma_name")

#Renaming attributes to uniform structure
BRMA_England <- BRMA_England %>% rename(BRMA = Name, ID = LOCALITY_I)
BRMA_Scotland <- BRMA_Scotland %>% rename(BRMA = Name, ID = LOCALITY_I)
BRMA_Wales <- BRMA_Wales %>% rename(BRMA = brma_name, ID = brma)

#Reprojection to uniform Projection System
BRMA_England <-spTransform(BRMA_England, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
BRMA_Scotland <-spTransform(BRMA_Scotland, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
BRMA_Wales <-spTransform(BRMA_Wales, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

#Adding the country attribute
BRMA_England$Country <- "England"
BRMA_Scotland$Country <- "Scotland"
BRMA_Wales$Country <- "Wales"

#Merging 3 shapefiles into one
BRMA_GB <- rbind(BRMA_England, BRMA_Scotland, BRMA_Wales, makeUniqueIDs = TRUE)

#Creating bounding box coordintes for the individual BRMAs
BRMA_BoundingBoxes <- lapply(BRMA_GB@polygons, bbox)
BRMA_BoundingBoxes <- data.frame(matrix(unlist(BRMA_BoundingBoxes), nrow = 192, byrow = T))
BRMA_Names <- as.data.frame(BRMA_GB$BRMA)
BRMA_BoundingTable <- cbind(BRMA_Names, BRMA_BoundingBoxes)
BRMA_BoundingTable <- BRMA_BoundingTable %>% rename(BRMA = `BRMA_GB$BRMA`)

#Merging the 2 West Chesire polygons into 1 (originally 1 from England and 1 from Wales)
#Tested some methods such as unionSpatialPolygons and gUnionCascaded, but they didn't work
#Probably easier to do in Q GIS

#Simplifying the dataset geometry
BRMA_GB <- ms_simplify(input = BRMA_GB, keep = 0.001)

#Topology corrections should happen here
#Tested a few packages including Cleangeo and rgrass7
#Probably easier to do in Q GIS

#Write Produced shapefiles
writeOGR(BRMA_England, dsn = "Prepared_Data", layer = "BRMA_Eng", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(BRMA_Scotland, dsn = "Prepared_Data", layer = "BRMA_Sco", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(BRMA_Wales, dsn = "Prepared_Data", layer = "BRMA_Wal", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(BRMA_GB, dsn = "Prepared_Data", layer = "BRMA_GB", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#Removing unvanted data from R Studio environment
rm(BRMA_England, BRMA_Scotland, BRMA_Wales)

#Defining Urban and Rural BRMAs
#Prepare Cities dataset from Populated Places shapefile
Cities <- readOGR(dsn = "Input_Geographies", layer = "ne_10m_populated_places_simple", GDAL1_integer64_policy = TRUE)
Cities <- Cities %>% select("name", "pop_max", "latitude", "longitude")
Cities <- Cities %>% rename(Population = pop_max)
proj4string(Cities) <- proj4string(BRMA_GB)
writeOGR(Cities, dsn = "Prepared_Data", layer = "Cities", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#Creat a buffer circle (e.g. 50 km radius) around each city point
#Tested a few methods, e.g. sf library, st_buffer
#The problem is with the projection
#Probably easier to sort in Q GIS

#Add Urban/Rural attributes to SpatialPolygonsDataFrames
BRMA_GB$URClass <- "Rural"

#Selecting Urban BRMAs
over(Cities, BRMA_GB)
Urban_BRMA_List <- over(Cities, BRMA_GB)

BRMA_GB %>%
  mutate(
    URClass = ifelse(BRMA %in% Urban_BRMA_List$BRMA, 'Urban', 'Rural')
  ) -> BRMA_GB

#Defining Coastal and Inland BRMAs
#Prepare Coastline point dataset from Great Britain polyline shapefile
Coastline <- readOGR(dsn = "Input_Geographies", layer = "Great Britain")
Coastline_Points <- as(Coastline, "SpatialPointsDataFrame")
writeOGR(Coastline_Points, dsn = "Prepared_Data", layer = "Coastline_Points", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#Add Coastal and Inland attributes
BRMA_GB$CIClass <- "Inland"

#Selecting Coastal BRMAs
over(Coastline_Points, BRMA_GB)
Coastal_BRMA_List <- over(Coastline_Points, BRMA_GB)

BRMA_GB %>%
  mutate(
    CIClass = ifelse(BRMA %in% Coastal_BRMA_List$BRMA, 'Coastal', 'Inland')
  ) -> BRMA_GB

#Write Produced shapefiles
writeOGR(BRMA_GB, dsn = "Prepared_Data", layer = "BRMA_GB", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#At this stage the code can be stopped and changes can be made to the shapefile in Q GIS, such as
#Merging the 2 West Chesire polygons, topology checks, and manual changes for Urban/Rural and Inland/Coastal attributes
#If changes made, uncomment the next row and continue the code, and check row 287!
#BRMA_GB <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)

#Creating Filters
Filter_Urban <- subset(BRMA_GB, URClass == "Urban")
Filter_Rural <- subset(BRMA_GB, URClass == "Rural")
Filter_Inland <- subset(BRMA_GB, CIClass == "Inland")
Filter_Coastal <- subset(BRMA_GB, CIClass == "Coastal")
Filter_England <- subset(BRMA_GB, Country == "England")
Filter_Scotland <- subset(BRMA_GB, Country == "Scotland")
Filter_Wales <- subset(BRMA_GB, Country == "Wales")

#Importing Spatial datasets with appropriate naming
SAR_2015 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED1_2015 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED2_2015 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED3_2015 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED4_2015 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
SAR_2016 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED1_2016 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED2_2016 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED3_2016 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED4_2016 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
SAR_2017 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED1_2017 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED2_2017 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED3_2017 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED4_2017 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
SAR_2018 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED1_2018 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED2_2018 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED3_2018 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)
BED4_2018 <- readOGR(dsn = "Prepared_Data", layer = "BRMA_GB", GDAL1_integer64_policy = TRUE)

#Importing the rent data and producing subsets
Rent_Data <- read.csv(file ="Input_Rent Data/Affordability.csv", header = TRUE, sep = ",")

RD_SAR_2015 <- subset(Rent_Data, select = c(1,2))
RD_1BED_2015 <- subset(Rent_Data, select = c(1,3))
RD_2BED_2015 <- subset(Rent_Data, select = c(1,4))
RD_3BED_2015 <- subset(Rent_Data, select = c(1,5))
RD_4BED_2015 <- subset(Rent_Data, select = c(1,6))
RD_SAR_2016 <- subset(Rent_Data, select = c(1,7))
RD_1BED_2016 <- subset(Rent_Data, select = c(1,8))
RD_2BED_2016 <- subset(Rent_Data, select = c(1,9))
RD_3BED_2016 <- subset(Rent_Data, select = c(1,10))
RD_4BED_2016 <- subset(Rent_Data, select = c(1,11))
RD_SAR_2017 <- subset(Rent_Data, select = c(1,12))
RD_1BED_2017 <- subset(Rent_Data, select = c(1,13))
RD_2BED_2017 <- subset(Rent_Data, select = c(1,14))
RD_3BED_2017 <- subset(Rent_Data, select = c(1,15))
RD_4BED_2017 <- subset(Rent_Data, select = c(1,16))
RD_SAR_2018 <- subset(Rent_Data, select = c(1,17))
RD_1BED_2018 <- subset(Rent_Data, select = c(1,18))
RD_2BED_2018 <- subset(Rent_Data, select = c(1,19))
RD_3BED_2018 <- subset(Rent_Data, select = c(1,20))
RD_4BED_2018 <- subset(Rent_Data, select = c(1,21))

#Joining the shapefile with the rent data
#The raster library is only loaded here as it affects how subsetting works
library(raster)

#Merging spatial datasets with corresponding rent data
SAR_2015 <- merge(SAR_2015, RD_SAR_2015, by = "BRMA")
BED1_2015 <- merge(BED1_2015, RD_1BED_2015, by = "BRMA")
BED2_2015 <- merge(BED2_2015, RD_2BED_2015, by = "BRMA")
BED3_2015 <- merge(BED3_2015, RD_3BED_2015, by = "BRMA")
BED4_2015 <- merge(BED4_2015, RD_4BED_2015, by = "BRMA")
SAR_2016 <- merge(SAR_2016, RD_SAR_2016, by = "BRMA")
BED1_2016 <- merge(BED1_2016, RD_1BED_2016, by = "BRMA")
BED2_2016 <- merge(BED2_2016, RD_2BED_2016, by = "BRMA")
BED3_2016 <- merge(BED3_2016, RD_3BED_2016, by = "BRMA")
BED4_2016 <- merge(BED4_2016, RD_4BED_2016, by = "BRMA")
SAR_2017 <- merge(SAR_2017, RD_SAR_2017, by = "BRMA")
BED1_2017 <- merge(BED1_2017, RD_1BED_2017, by = "BRMA")
BED2_2017 <- merge(BED2_2017, RD_2BED_2017, by = "BRMA")
BED3_2017 <- merge(BED3_2017, RD_3BED_2017, by = "BRMA")
BED4_2017 <- merge(BED4_2017, RD_4BED_2017, by = "BRMA")
SAR_2018 <- merge(SAR_2018, RD_SAR_2018, by = "BRMA")
BED1_2018 <- merge(BED1_2018, RD_1BED_2018, by = "BRMA")
BED2_2018 <- merge(BED2_2018, RD_2BED_2018, by = "BRMA")
BED3_2018 <- merge(BED3_2018, RD_3BED_2018, by = "BRMA")
BED4_2018 <- merge(BED4_2018, RD_4BED_2018, by = "BRMA")

#Unloading the raster package, which can cause other functions to fail
detach("package:raster", unload = TRUE)

#Removing Non-Spatial elements
rm(RD_SAR_2015, RD_1BED_2015, RD_2BED_2015, RD_3BED_2015, RD_4BED_2015)
rm(RD_SAR_2016, RD_1BED_2016, RD_2BED_2016, RD_3BED_2016, RD_4BED_2016)
rm(RD_SAR_2017, RD_1BED_2017, RD_2BED_2017, RD_3BED_2017, RD_4BED_2017)
rm(RD_SAR_2018, RD_1BED_2018, RD_2BED_2018, RD_3BED_2018, RD_4BED_2018)

#Further editing the dataset
#Renaming the specific rent data attribute to RentData
SAR_2015 <- SAR_2015 %>% rename(RentData = X2015.SAR)
BED1_2015 <- BED1_2015 %>% rename(RentData = X2015.1.BED)
BED2_2015 <- BED2_2015 %>% rename(RentData = X2015.2.BED)
BED3_2015 <- BED3_2015 %>% rename(RentData = X2015.3.BED)
BED4_2015 <- BED4_2015 %>% rename(RentData = X2015.4.BED)
SAR_2016 <- SAR_2016 %>% rename(RentData = X2016.SAR)
BED1_2016 <- BED1_2016 %>% rename(RentData = X2016.1.BED)
BED2_2016 <- BED2_2016 %>% rename(RentData = X2016.2.BED)
BED3_2016 <- BED3_2016 %>% rename(RentData = X2016.3.BED)
BED4_2016 <- BED4_2016 %>% rename(RentData = X2016.4.BED)
SAR_2017 <- SAR_2017 %>% rename(RentData = X2017.SAR)
BED1_2017 <- BED1_2017 %>% rename(RentData = X2017.1.BED)
BED2_2017 <- BED2_2017 %>% rename(RentData = X2017.2.BED)
BED3_2017 <- BED3_2017 %>% rename(RentData = X2017.3.BED)
BED4_2017 <- BED4_2017 %>% rename(RentData = X2017.4.BED)
SAR_2018 <- SAR_2018 %>% rename(RentData = X2018.SAR)
BED1_2018 <- BED1_2018 %>% rename(RentData = X2018.1.BED)
BED2_2018 <- BED2_2018 %>% rename(RentData = X2018.2.BED)
BED3_2018 <- BED3_2018 %>% rename(RentData = X2018.3.BED)
BED4_2018 <- BED4_2018 %>% rename(RentData = X2018.4.BED)

#Add Year attribute
SAR_2015$Year <- 2015
BED1_2015$Year <- 2015
BED2_2015$Year <- 2015
BED3_2015$Year <- 2015
BED4_2015$Year <- 2015
SAR_2016$Year <- 2016
BED1_2016$Year <- 2016
BED2_2016$Year <- 2016
BED3_2016$Year <- 2016
BED4_2016$Year <- 2016
SAR_2017$Year <- 2017
BED1_2017$Year <- 2017
BED2_2017$Year <- 2017
BED3_2017$Year <- 2017
BED4_2017$Year <- 2017
SAR_2018$Year <- 2018
BED1_2018$Year <- 2018
BED2_2018$Year <- 2018
BED3_2018$Year <- 2018
BED4_2018$Year <- 2018

#Add Entitlement attribute
SAR_2015$Room <- "SAR"
BED1_2015$Room <- "BED1"
BED2_2015$Room <- "BED2"
BED3_2015$Room <- "BED3"
BED4_2015$Room <- "BED4"
SAR_2016$Room <- "SAR"
BED1_2016$Room <- "BED1"
BED2_2016$Room <- "BED2"
BED3_2016$Room <- "BED3"
BED4_2016$Room <- "BED4"
SAR_2017$Room <- "SAR"
BED1_2017$Room <- "BED1"
BED2_2017$Room <- "BED2"
BED3_2017$Room <- "BED3"
BED4_2017$Room <- "BED4"
SAR_2018$Room <- "SAR"
BED1_2018$Room <- "BED1"
BED2_2018$Room <- "BED2"
BED3_2018$Room <- "BED3"
BED4_2018$Room <- "BED4"

#Merging Data subsets by room entitlements
SAR <- rbind(SAR_2015, SAR_2016, SAR_2017, SAR_2018, makeUniqueIDs = TRUE)
BED1 <- rbind(BED1_2015, BED1_2016, BED1_2017, BED1_2018, makeUniqueIDs = TRUE)
BED2 <- rbind(BED2_2015, BED2_2016, BED2_2017, BED2_2018, makeUniqueIDs = TRUE)
BED3 <- rbind(BED3_2015, BED3_2016, BED3_2017, BED3_2018, makeUniqueIDs = TRUE)
BED4 <- rbind(BED4_2015, BED4_2016, BED4_2017, BED4_2018, makeUniqueIDs = TRUE)
RentDataPolygons <- rbind(SAR, BED1, BED2, BED3, BED4, makeUniqueIDs = TRUE)

writeOGR(RentDataPolygons, dsn = "Prepared_Data", layer = "RentDataPolygons", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#Creating the dataframe table
#Note that for avoiding display problems, the West Chesire BRMA is removed from the table
#If duplication issues sorted that line should be removed
RentDataTable <- as.data.frame(RentDataPolygons)
RentDataTable <- subset(RentDataTable, BRMA != "West Cheshire")

#Removing unwanted items
rm(SAR_2015, BED1_2015, BED2_2015, BED3_2015, BED4_2015)
rm(SAR_2016, BED1_2016, BED2_2016, BED3_2016, BED4_2016)
rm(SAR_2017, BED1_2017, BED2_2017, BED3_2017, BED4_2017)
rm(SAR_2018, BED1_2018, BED2_2018, BED3_2018, BED4_2018)
rm(SAR, BED1, BED2, BED3, BED4)
rm(BRMA_GB)
rm(Urban_BRMA_List)
rm(Coastline, Coastal_BRMA_List, Coastline_Points)
rm(Rent_Data)
rm(BRMA_BoundingBoxes, BRMA_Names)

#Creating summary table (Down to row 922. I suspect this is not efficient, but safe.)
averages_2015_SAR <- subset(subset(RentDataTable, Year == 2015), Room == "SAR")
averages_2015_BED1 <- subset(subset(RentDataTable, Year == 2015), Room == "BED1")
averages_2015_BED2 <- subset(subset(RentDataTable, Year == 2015), Room == "BED2")
averages_2015_BED3 <- subset(subset(RentDataTable, Year == 2015), Room == "BED3")
averages_2015_BED4 <- subset(subset(RentDataTable, Year == 2015), Room == "BED4")
averages_2016_SAR <- subset(subset(RentDataTable, Year == 2016), Room == "SAR")
averages_2016_BED1 <- subset(subset(RentDataTable, Year == 2016), Room == "BED1")
averages_2016_BED2 <- subset(subset(RentDataTable, Year == 2016), Room == "BED2")
averages_2016_BED3 <- subset(subset(RentDataTable, Year == 2016), Room == "BED3")
averages_2016_BED4 <- subset(subset(RentDataTable, Year == 2016), Room == "BED4")
averages_2017_SAR <- subset(subset(RentDataTable, Year == 2017), Room == "SAR")
averages_2017_BED1 <- subset(subset(RentDataTable, Year == 2017), Room == "BED1")
averages_2017_BED2 <- subset(subset(RentDataTable, Year == 2017), Room == "BED2")
averages_2017_BED3 <- subset(subset(RentDataTable, Year == 2017), Room == "BED3")
averages_2017_BED4 <- subset(subset(RentDataTable, Year == 2017), Room == "BED4")
averages_2018_SAR <- subset(subset(RentDataTable, Year == 2018), Room == "SAR")
averages_2018_BED1 <- subset(subset(RentDataTable, Year == 2018), Room == "BED1")
averages_2018_BED2 <- subset(subset(RentDataTable, Year == 2018), Room == "BED2")
averages_2018_BED3 <- subset(subset(RentDataTable, Year == 2018), Room == "BED3")
averages_2018_BED4 <- subset(subset(RentDataTable, Year == 2018), Room == "BED4")

averages_2015_SAR_England <- subset(averages_2015_SAR, Country == "England")
averages_2015_SAR_Scotland <- subset(averages_2015_SAR, Country == "Scotland")
averages_2015_SAR_Wales <- subset(averages_2015_SAR, Country == "Wales")
averages_2015_SAR_Urban <- subset(averages_2015_SAR, URClass == "Urban")
averages_2015_SAR_Rural <- subset(averages_2015_SAR, URClass == "Rural")
averages_2015_SAR_Coastal <- subset(averages_2015_SAR, CIClass == "Coastal")
averages_2015_SAR_Inland <- subset(averages_2015_SAR, CIClass == "Inland")
averages_2016_SAR_England <- subset(averages_2016_SAR, Country == "England")
averages_2016_SAR_Scotland <- subset(averages_2016_SAR, Country == "Scotland")
averages_2016_SAR_Wales <- subset(averages_2016_SAR, Country == "Wales")
averages_2016_SAR_Urban <- subset(averages_2016_SAR, URClass == "Urban")
averages_2016_SAR_Rural <- subset(averages_2016_SAR, URClass == "Rural")
averages_2016_SAR_Coastal <- subset(averages_2016_SAR, CIClass == "Coastal")
averages_2016_SAR_Inland <- subset(averages_2016_SAR, CIClass == "Inland")
averages_2017_SAR_England <- subset(averages_2017_SAR, Country == "England")
averages_2017_SAR_Scotland <- subset(averages_2017_SAR, Country == "Scotland")
averages_2017_SAR_Wales <- subset(averages_2017_SAR, Country == "Wales")
averages_2017_SAR_Urban <- subset(averages_2017_SAR, URClass == "Urban")
averages_2017_SAR_Rural <- subset(averages_2017_SAR, URClass == "Rural")
averages_2017_SAR_Coastal <- subset(averages_2017_SAR, CIClass == "Coastal")
averages_2017_SAR_Inland <- subset(averages_2017_SAR, CIClass == "Inland")
averages_2018_SAR_England <- subset(averages_2018_SAR, Country == "England")
averages_2018_SAR_Scotland <- subset(averages_2018_SAR, Country == "Scotland")
averages_2018_SAR_Wales <- subset(averages_2018_SAR, Country == "Wales")
averages_2018_SAR_Urban <- subset(averages_2018_SAR, URClass == "Urban")
averages_2018_SAR_Rural <- subset(averages_2018_SAR, URClass == "Rural")
averages_2018_SAR_Coastal <- subset(averages_2018_SAR, CIClass == "Coastal")
averages_2018_SAR_Inland <- subset(averages_2018_SAR, CIClass == "Inland")
averages_2015_BED1_England <- subset(averages_2015_BED1, Country == "England")
averages_2015_BED1_Scotland <- subset(averages_2015_BED1, Country == "Scotland")
averages_2015_BED1_Wales <- subset(averages_2015_BED1, Country == "Wales")
averages_2015_BED1_Urban <- subset(averages_2015_BED1, URClass == "Urban")
averages_2015_BED1_Rural <- subset(averages_2015_BED1, URClass == "Rural")
averages_2015_BED1_Coastal <- subset(averages_2015_BED1, CIClass == "Coastal")
averages_2015_BED1_Inland <- subset(averages_2015_BED1, CIClass == "Inland")
averages_2016_BED1_England <- subset(averages_2016_BED1, Country == "England")
averages_2016_BED1_Scotland <- subset(averages_2016_BED1, Country == "Scotland")
averages_2016_BED1_Wales <- subset(averages_2016_BED1, Country == "Wales")
averages_2016_BED1_Urban <- subset(averages_2016_BED1, URClass == "Urban")
averages_2016_BED1_Rural <- subset(averages_2016_BED1, URClass == "Rural")
averages_2016_BED1_Coastal <- subset(averages_2016_BED1, CIClass == "Coastal")
averages_2016_BED1_Inland <- subset(averages_2016_BED1, CIClass == "Inland")
averages_2017_BED1_England <- subset(averages_2017_BED1, Country == "England")
averages_2017_BED1_Scotland <- subset(averages_2017_BED1, Country == "Scotland")
averages_2017_BED1_Wales <- subset(averages_2017_BED1, Country == "Wales")
averages_2017_BED1_Urban <- subset(averages_2017_BED1, URClass == "Urban")
averages_2017_BED1_Rural <- subset(averages_2017_BED1, URClass == "Rural")
averages_2017_BED1_Coastal <- subset(averages_2017_BED1, CIClass == "Coastal")
averages_2017_BED1_Inland <- subset(averages_2017_BED1, CIClass == "Inland")
averages_2018_BED1_England <- subset(averages_2018_BED1, Country == "England")
averages_2018_BED1_Scotland <- subset(averages_2018_BED1, Country == "Scotland")
averages_2018_BED1_Wales <- subset(averages_2018_BED1, Country == "Wales")
averages_2018_BED1_Urban <- subset(averages_2018_BED1, URClass == "Urban")
averages_2018_BED1_Rural <- subset(averages_2018_BED1, URClass == "Rural")
averages_2018_BED1_Coastal <- subset(averages_2018_BED1, CIClass == "Coastal")
averages_2018_BED1_Inland <- subset(averages_2018_BED1, CIClass == "Inland")
averages_2015_BED2_England <- subset(averages_2015_BED2, Country == "England")
averages_2015_BED2_Scotland <- subset(averages_2015_BED2, Country == "Scotland")
averages_2015_BED2_Wales <- subset(averages_2015_BED2, Country == "Wales")
averages_2015_BED2_Urban <- subset(averages_2015_BED2, URClass == "Urban")
averages_2015_BED2_Rural <- subset(averages_2015_BED2, URClass == "Rural")
averages_2015_BED2_Coastal <- subset(averages_2015_BED2, CIClass == "Coastal")
averages_2015_BED2_Inland <- subset(averages_2015_BED2, CIClass == "Inland")
averages_2016_BED2_England <- subset(averages_2016_BED2, Country == "England")
averages_2016_BED2_Scotland <- subset(averages_2016_BED2, Country == "Scotland")
averages_2016_BED2_Wales <- subset(averages_2016_BED2, Country == "Wales")
averages_2016_BED2_Urban <- subset(averages_2016_BED2, URClass == "Urban")
averages_2016_BED2_Rural <- subset(averages_2016_BED2, URClass == "Rural")
averages_2016_BED2_Coastal <- subset(averages_2016_BED2, CIClass == "Coastal")
averages_2016_BED2_Inland <- subset(averages_2016_BED2, CIClass == "Inland")
averages_2017_BED2_England <- subset(averages_2017_BED2, Country == "England")
averages_2017_BED2_Scotland <- subset(averages_2017_BED2, Country == "Scotland")
averages_2017_BED2_Wales <- subset(averages_2017_BED2, Country == "Wales")
averages_2017_BED2_Urban <- subset(averages_2017_BED2, URClass == "Urban")
averages_2017_BED2_Rural <- subset(averages_2017_BED2, URClass == "Rural")
averages_2017_BED2_Coastal <- subset(averages_2017_BED2, CIClass == "Coastal")
averages_2017_BED2_Inland <- subset(averages_2017_BED2, CIClass == "Inland")
averages_2018_BED2_England <- subset(averages_2018_BED2, Country == "England")
averages_2018_BED2_Scotland <- subset(averages_2018_BED2, Country == "Scotland")
averages_2018_BED2_Wales <- subset(averages_2018_BED2, Country == "Wales")
averages_2018_BED2_Urban <- subset(averages_2018_BED2, URClass == "Urban")
averages_2018_BED2_Rural <- subset(averages_2018_BED2, URClass == "Rural")
averages_2018_BED2_Coastal <- subset(averages_2018_BED2, CIClass == "Coastal")
averages_2018_BED2_Inland <- subset(averages_2018_BED2, CIClass == "Inland")
averages_2015_BED3_England <- subset(averages_2015_BED3, Country == "England")
averages_2015_BED3_Scotland <- subset(averages_2015_BED3, Country == "Scotland")
averages_2015_BED3_Wales <- subset(averages_2015_BED3, Country == "Wales")
averages_2015_BED3_Urban <- subset(averages_2015_BED3, URClass == "Urban")
averages_2015_BED3_Rural <- subset(averages_2015_BED3, URClass == "Rural")
averages_2015_BED3_Coastal <- subset(averages_2015_BED3, CIClass == "Coastal")
averages_2015_BED3_Inland <- subset(averages_2015_BED3, CIClass == "Inland")
averages_2016_BED3_England <- subset(averages_2016_BED3, Country == "England")
averages_2016_BED3_Scotland <- subset(averages_2016_BED3, Country == "Scotland")
averages_2016_BED3_Wales <- subset(averages_2016_BED3, Country == "Wales")
averages_2016_BED3_Urban <- subset(averages_2016_BED3, URClass == "Urban")
averages_2016_BED3_Rural <- subset(averages_2016_BED3, URClass == "Rural")
averages_2016_BED3_Coastal <- subset(averages_2016_BED3, CIClass == "Coastal")
averages_2016_BED3_Inland <- subset(averages_2016_BED3, CIClass == "Inland")
averages_2017_BED3_England <- subset(averages_2017_BED3, Country == "England")
averages_2017_BED3_Scotland <- subset(averages_2017_BED3, Country == "Scotland")
averages_2017_BED3_Wales <- subset(averages_2017_BED3, Country == "Wales")
averages_2017_BED3_Urban <- subset(averages_2017_BED3, URClass == "Urban")
averages_2017_BED3_Rural <- subset(averages_2017_BED3, URClass == "Rural")
averages_2017_BED3_Coastal <- subset(averages_2017_BED3, CIClass == "Coastal")
averages_2017_BED3_Inland <- subset(averages_2017_BED3, CIClass == "Inland")
averages_2018_BED3_England <- subset(averages_2018_BED3, Country == "England")
averages_2018_BED3_Scotland <- subset(averages_2018_BED3, Country == "Scotland")
averages_2018_BED3_Wales <- subset(averages_2018_BED3, Country == "Wales")
averages_2018_BED3_Urban <- subset(averages_2018_BED3, URClass == "Urban")
averages_2018_BED3_Rural <- subset(averages_2018_BED3, URClass == "Rural")
averages_2018_BED3_Coastal <- subset(averages_2018_BED3, CIClass == "Coastal")
averages_2018_BED3_Inland <- subset(averages_2018_BED3, CIClass == "Inland")
averages_2015_BED4_England <- subset(averages_2015_BED4, Country == "England")
averages_2015_BED4_Scotland <- subset(averages_2015_BED4, Country == "Scotland")
averages_2015_BED4_Wales <- subset(averages_2015_BED4, Country == "Wales")
averages_2015_BED4_Urban <- subset(averages_2015_BED4, URClass == "Urban")
averages_2015_BED4_Rural <- subset(averages_2015_BED4, URClass == "Rural")
averages_2015_BED4_Coastal <- subset(averages_2015_BED4, CIClass == "Coastal")
averages_2015_BED4_Inland <- subset(averages_2015_BED4, CIClass == "Inland")
averages_2016_BED4_England <- subset(averages_2016_BED4, Country == "England")
averages_2016_BED4_Scotland <- subset(averages_2016_BED4, Country == "Scotland")
averages_2016_BED4_Wales <- subset(averages_2016_BED4, Country == "Wales")
averages_2016_BED4_Urban <- subset(averages_2016_BED4, URClass == "Urban")
averages_2016_BED4_Rural <- subset(averages_2016_BED4, URClass == "Rural")
averages_2016_BED4_Coastal <- subset(averages_2016_BED4, CIClass == "Coastal")
averages_2016_BED4_Inland <- subset(averages_2016_BED4, CIClass == "Inland")
averages_2017_BED4_England <- subset(averages_2017_BED4, Country == "England")
averages_2017_BED4_Scotland <- subset(averages_2017_BED4, Country == "Scotland")
averages_2017_BED4_Wales <- subset(averages_2017_BED4, Country == "Wales")
averages_2017_BED4_Urban <- subset(averages_2017_BED4, URClass == "Urban")
averages_2017_BED4_Rural <- subset(averages_2017_BED4, URClass == "Rural")
averages_2017_BED4_Coastal <- subset(averages_2017_BED4, CIClass == "Coastal")
averages_2017_BED4_Inland <- subset(averages_2017_BED4, CIClass == "Inland")
averages_2018_BED4_England <- subset(averages_2018_BED4, Country == "England")
averages_2018_BED4_Scotland <- subset(averages_2018_BED4, Country == "Scotland")
averages_2018_BED4_Wales <- subset(averages_2018_BED4, Country == "Wales")
averages_2018_BED4_Urban <- subset(averages_2018_BED4, URClass == "Urban")
averages_2018_BED4_Rural <- subset(averages_2018_BED4, URClass == "Rural")
averages_2018_BED4_Coastal <- subset(averages_2018_BED4, CIClass == "Coastal")
averages_2018_BED4_Inland <- subset(averages_2018_BED4, CIClass == "Inland")

valAverages_2015_SAR <- mean(averages_2015_SAR[["RentData"]])
valAverages_2015_BED1 <- mean(averages_2015_BED1[["RentData"]])
valAverages_2015_BED2 <- mean(averages_2015_BED2[["RentData"]])
valAverages_2015_BED3 <- mean(averages_2015_BED3[["RentData"]])
valAverages_2015_BED4 <- mean(averages_2015_BED4[["RentData"]])
valAverages_2016_SAR <- mean(averages_2016_SAR[["RentData"]])
valAverages_2016_BED1 <- mean(averages_2016_BED1[["RentData"]])
valAverages_2016_BED2 <- mean(averages_2016_BED2[["RentData"]])
valAverages_2016_BED3 <- mean(averages_2016_BED3[["RentData"]])
valAverages_2016_BED4 <- mean(averages_2016_BED4[["RentData"]])
valAverages_2017_SAR <- mean(averages_2017_SAR[["RentData"]])
valAverages_2017_BED1 <- mean(averages_2017_BED1[["RentData"]])
valAverages_2017_BED2 <- mean(averages_2017_BED2[["RentData"]])
valAverages_2017_BED3 <- mean(averages_2017_BED3[["RentData"]])
valAverages_2017_BED4 <- mean(averages_2017_BED4[["RentData"]])
valAverages_2018_SAR <- mean(averages_2018_SAR[["RentData"]])
valAverages_2018_BED1 <- mean(averages_2018_BED1[["RentData"]])
valAverages_2018_BED2 <- mean(averages_2018_BED2[["RentData"]])
valAverages_2018_BED3 <- mean(averages_2018_BED3[["RentData"]])
valAverages_2018_BED4 <- mean(averages_2018_BED4[["RentData"]])

valAverages_2015_SAR_England <- mean(averages_2015_SAR_England[["RentData"]])
valAverages_2015_SAR_Scotland <- mean(averages_2015_SAR_Scotland[["RentData"]])
valAverages_2015_SAR_Wales <- mean(averages_2015_SAR_Wales[["RentData"]])
valAverages_2015_SAR_Urban <- mean(averages_2015_SAR_Urban[["RentData"]])
valAverages_2015_SAR_Rural <- mean(averages_2015_SAR_Rural[["RentData"]])
valAverages_2015_SAR_Coastal <- mean(averages_2015_SAR_Coastal[["RentData"]])
valAverages_2015_SAR_Inland <- mean(averages_2015_SAR_Inland[["RentData"]])
valAverages_2016_SAR_England <- mean(averages_2016_SAR_England[["RentData"]])
valAverages_2016_SAR_Scotland <- mean(averages_2016_SAR_Scotland[["RentData"]])
valAverages_2016_SAR_Wales <- mean(averages_2016_SAR_Wales[["RentData"]])
valAverages_2016_SAR_Urban <- mean(averages_2016_SAR_Urban[["RentData"]])
valAverages_2016_SAR_Rural <- mean(averages_2016_SAR_Rural[["RentData"]])
valAverages_2016_SAR_Coastal <- mean(averages_2016_SAR_Coastal[["RentData"]])
valAverages_2016_SAR_Inland <- mean(averages_2016_SAR_Inland[["RentData"]])
valAverages_2017_SAR_England <- mean(averages_2017_SAR_England[["RentData"]])
valAverages_2017_SAR_Scotland <- mean(averages_2017_SAR_Scotland[["RentData"]])
valAverages_2017_SAR_Wales <- mean(averages_2017_SAR_Wales[["RentData"]])
valAverages_2017_SAR_Urban <- mean(averages_2017_SAR_Urban[["RentData"]])
valAverages_2017_SAR_Rural <- mean(averages_2017_SAR_Rural[["RentData"]])
valAverages_2017_SAR_Coastal <- mean(averages_2017_SAR_Coastal[["RentData"]])
valAverages_2017_SAR_Inland <- mean(averages_2017_SAR_Inland[["RentData"]])
valAverages_2018_SAR_England <- mean(averages_2018_SAR_England[["RentData"]])
valAverages_2018_SAR_Scotland <- mean(averages_2018_SAR_Scotland[["RentData"]])
valAverages_2018_SAR_Wales <- mean(averages_2018_SAR_Wales[["RentData"]])
valAverages_2018_SAR_Urban <- mean(averages_2018_SAR_Urban[["RentData"]])
valAverages_2018_SAR_Rural <- mean(averages_2018_SAR_Rural[["RentData"]])
valAverages_2018_SAR_Coastal <- mean(averages_2018_SAR_Coastal[["RentData"]])
valAverages_2018_SAR_Inland <- mean(averages_2018_SAR_Inland[["RentData"]])
valAverages_2015_BED1_England <- mean(averages_2015_BED1_England[["RentData"]])
valAverages_2015_BED1_Scotland <- mean(averages_2015_BED1_Scotland[["RentData"]])
valAverages_2015_BED1_Wales <- mean(averages_2015_BED1_Wales[["RentData"]])
valAverages_2015_BED1_Urban <- mean(averages_2015_BED1_Urban[["RentData"]])
valAverages_2015_BED1_Rural <- mean(averages_2015_BED1_Rural[["RentData"]])
valAverages_2015_BED1_Coastal <- mean(averages_2015_BED1_Coastal[["RentData"]])
valAverages_2015_BED1_Inland <- mean(averages_2015_BED1_Inland[["RentData"]])
valAverages_2016_BED1_England <- mean(averages_2016_BED1_England[["RentData"]])
valAverages_2016_BED1_Scotland <- mean(averages_2016_BED1_Scotland[["RentData"]])
valAverages_2016_BED1_Wales <- mean(averages_2016_BED1_Wales[["RentData"]])
valAverages_2016_BED1_Urban <- mean(averages_2016_BED1_Urban[["RentData"]])
valAverages_2016_BED1_Rural <- mean(averages_2016_BED1_Rural[["RentData"]])
valAverages_2016_BED1_Coastal <- mean(averages_2016_BED1_Coastal[["RentData"]])
valAverages_2016_BED1_Inland <- mean(averages_2016_BED1_Inland[["RentData"]])
valAverages_2017_BED1_England <- mean(averages_2017_BED1_England[["RentData"]])
valAverages_2017_BED1_Scotland <- mean(averages_2017_BED1_Scotland[["RentData"]])
valAverages_2017_BED1_Wales <- mean(averages_2017_BED1_Wales[["RentData"]])
valAverages_2017_BED1_Urban <- mean(averages_2017_BED1_Urban[["RentData"]])
valAverages_2017_BED1_Rural <- mean(averages_2017_BED1_Rural[["RentData"]])
valAverages_2017_BED1_Coastal <- mean(averages_2017_BED1_Coastal[["RentData"]])
valAverages_2017_BED1_Inland <- mean(averages_2017_BED1_Inland[["RentData"]])
valAverages_2018_BED1_England <- mean(averages_2018_BED1_England[["RentData"]])
valAverages_2018_BED1_Scotland <- mean(averages_2018_BED1_Scotland[["RentData"]])
valAverages_2018_BED1_Wales <- mean(averages_2018_BED1_Wales[["RentData"]])
valAverages_2018_BED1_Urban <- mean(averages_2018_BED1_Urban[["RentData"]])
valAverages_2018_BED1_Rural <- mean(averages_2018_BED1_Rural[["RentData"]])
valAverages_2018_BED1_Coastal <- mean(averages_2018_BED1_Coastal[["RentData"]])
valAverages_2018_BED1_Inland <- mean(averages_2018_BED1_Inland[["RentData"]])
valAverages_2015_BED2_England <- mean(averages_2015_BED2_England[["RentData"]])
valAverages_2015_BED2_Scotland <- mean(averages_2015_BED2_Scotland[["RentData"]])
valAverages_2015_BED2_Wales <- mean(averages_2015_BED2_Wales[["RentData"]])
valAverages_2015_BED2_Urban <- mean(averages_2015_BED2_Urban[["RentData"]])
valAverages_2015_BED2_Rural <- mean(averages_2015_BED2_Rural[["RentData"]])
valAverages_2015_BED2_Coastal <- mean(averages_2015_BED2_Coastal[["RentData"]])
valAverages_2015_BED2_Inland <- mean(averages_2015_BED2_Inland[["RentData"]])
valAverages_2016_BED2_England <- mean(averages_2016_BED2_England[["RentData"]])
valAverages_2016_BED2_Scotland <- mean(averages_2016_BED2_Scotland[["RentData"]])
valAverages_2016_BED2_Wales <- mean(averages_2016_BED2_Wales[["RentData"]])
valAverages_2016_BED2_Urban <- mean(averages_2016_BED2_Urban[["RentData"]])
valAverages_2016_BED2_Rural <- mean(averages_2016_BED2_Rural[["RentData"]])
valAverages_2016_BED2_Coastal <- mean(averages_2016_BED2_Coastal[["RentData"]])
valAverages_2016_BED2_Inland <- mean(averages_2016_BED2_Inland[["RentData"]])
valAverages_2017_BED2_England <- mean(averages_2017_BED2_England[["RentData"]])
valAverages_2017_BED2_Scotland <- mean(averages_2017_BED2_Scotland[["RentData"]])
valAverages_2017_BED2_Wales <- mean(averages_2017_BED2_Wales[["RentData"]])
valAverages_2017_BED2_Urban <- mean(averages_2017_BED2_Urban[["RentData"]])
valAverages_2017_BED2_Rural <- mean(averages_2017_BED2_Rural[["RentData"]])
valAverages_2017_BED2_Coastal <- mean(averages_2017_BED2_Coastal[["RentData"]])
valAverages_2017_BED2_Inland <- mean(averages_2017_BED2_Inland[["RentData"]])
valAverages_2018_BED2_England <- mean(averages_2018_BED2_England[["RentData"]])
valAverages_2018_BED2_Scotland <- mean(averages_2018_BED2_Scotland[["RentData"]])
valAverages_2018_BED2_Wales <- mean(averages_2018_BED2_Wales[["RentData"]])
valAverages_2018_BED2_Urban <- mean(averages_2018_BED2_Urban[["RentData"]])
valAverages_2018_BED2_Rural <- mean(averages_2018_BED2_Rural[["RentData"]])
valAverages_2018_BED2_Coastal <- mean(averages_2018_BED2_Coastal[["RentData"]])
valAverages_2018_BED2_Inland <- mean(averages_2018_BED2_Inland[["RentData"]])
valAverages_2015_BED3_England <- mean(averages_2015_BED3_England[["RentData"]])
valAverages_2015_BED3_Scotland <- mean(averages_2015_BED3_Scotland[["RentData"]])
valAverages_2015_BED3_Wales <- mean(averages_2015_BED3_Wales[["RentData"]])
valAverages_2015_BED3_Urban <- mean(averages_2015_BED3_Urban[["RentData"]])
valAverages_2015_BED3_Rural <- mean(averages_2015_BED3_Rural[["RentData"]])
valAverages_2015_BED3_Coastal <- mean(averages_2015_BED3_Coastal[["RentData"]])
valAverages_2015_BED3_Inland <- mean(averages_2015_BED3_Inland[["RentData"]])
valAverages_2016_BED3_England <- mean(averages_2016_BED3_England[["RentData"]])
valAverages_2016_BED3_Scotland <- mean(averages_2016_BED3_Scotland[["RentData"]])
valAverages_2016_BED3_Wales <- mean(averages_2016_BED3_Wales[["RentData"]])
valAverages_2016_BED3_Urban <- mean(averages_2016_BED3_Urban[["RentData"]])
valAverages_2016_BED3_Rural <- mean(averages_2016_BED3_Rural[["RentData"]])
valAverages_2016_BED3_Coastal <- mean(averages_2016_BED3_Coastal[["RentData"]])
valAverages_2016_BED3_Inland <- mean(averages_2016_BED3_Inland[["RentData"]])
valAverages_2017_BED3_England <- mean(averages_2017_BED3_England[["RentData"]])
valAverages_2017_BED3_Scotland <- mean(averages_2017_BED3_Scotland[["RentData"]])
valAverages_2017_BED3_Wales <- mean(averages_2017_BED3_Wales[["RentData"]])
valAverages_2017_BED3_Urban <- mean(averages_2017_BED3_Urban[["RentData"]])
valAverages_2017_BED3_Rural <- mean(averages_2017_BED3_Rural[["RentData"]])
valAverages_2017_BED3_Coastal <- mean(averages_2017_BED3_Coastal[["RentData"]])
valAverages_2017_BED3_Inland <- mean(averages_2017_BED3_Inland[["RentData"]])
valAverages_2018_BED3_England <- mean(averages_2018_BED3_England[["RentData"]])
valAverages_2018_BED3_Scotland <- mean(averages_2018_BED3_Scotland[["RentData"]])
valAverages_2018_BED3_Wales <- mean(averages_2018_BED3_Wales[["RentData"]])
valAverages_2018_BED3_Urban <- mean(averages_2018_BED3_Urban[["RentData"]])
valAverages_2018_BED3_Rural <- mean(averages_2018_BED3_Rural[["RentData"]])
valAverages_2018_BED3_Coastal <- mean(averages_2018_BED3_Coastal[["RentData"]])
valAverages_2018_BED3_Inland <- mean(averages_2018_BED3_Inland[["RentData"]])
valAverages_2015_BED4_England <- mean(averages_2015_BED4_England[["RentData"]])
valAverages_2015_BED4_Scotland <- mean(averages_2015_BED4_Scotland[["RentData"]])
valAverages_2015_BED4_Wales <- mean(averages_2015_BED4_Wales[["RentData"]])
valAverages_2015_BED4_Urban <- mean(averages_2015_BED4_Urban[["RentData"]])
valAverages_2015_BED4_Rural <- mean(averages_2015_BED4_Rural[["RentData"]])
valAverages_2015_BED4_Coastal <- mean(averages_2015_BED4_Coastal[["RentData"]])
valAverages_2015_BED4_Inland <- mean(averages_2015_BED4_Inland[["RentData"]])
valAverages_2016_BED4_England <- mean(averages_2016_BED4_England[["RentData"]])
valAverages_2016_BED4_Scotland <- mean(averages_2016_BED4_Scotland[["RentData"]])
valAverages_2016_BED4_Wales <- mean(averages_2016_BED4_Wales[["RentData"]])
valAverages_2016_BED4_Urban <- mean(averages_2016_BED4_Urban[["RentData"]])
valAverages_2016_BED4_Rural <- mean(averages_2016_BED4_Rural[["RentData"]])
valAverages_2016_BED4_Coastal <- mean(averages_2016_BED4_Coastal[["RentData"]])
valAverages_2016_BED4_Inland <- mean(averages_2016_BED4_Inland[["RentData"]])
valAverages_2017_BED4_England <- mean(averages_2017_BED4_England[["RentData"]])
valAverages_2017_BED4_Scotland <- mean(averages_2017_BED4_Scotland[["RentData"]])
valAverages_2017_BED4_Wales <- mean(averages_2017_BED4_Wales[["RentData"]])
valAverages_2017_BED4_Urban <- mean(averages_2017_BED4_Urban[["RentData"]])
valAverages_2017_BED4_Rural <- mean(averages_2017_BED4_Rural[["RentData"]])
valAverages_2017_BED4_Coastal <- mean(averages_2017_BED4_Coastal[["RentData"]])
valAverages_2017_BED4_Inland <- mean(averages_2017_BED4_Inland[["RentData"]])
valAverages_2018_BED4_England <- mean(averages_2018_BED4_England[["RentData"]])
valAverages_2018_BED4_Scotland <- mean(averages_2018_BED4_Scotland[["RentData"]])
valAverages_2018_BED4_Wales <- mean(averages_2018_BED4_Wales[["RentData"]])
valAverages_2018_BED4_Urban <- mean(averages_2018_BED4_Urban[["RentData"]])
valAverages_2018_BED4_Rural <- mean(averages_2018_BED4_Rural[["RentData"]])
valAverages_2018_BED4_Coastal <- mean(averages_2018_BED4_Coastal[["RentData"]])
valAverages_2018_BED4_Inland <- mean(averages_2018_BED4_Inland[["RentData"]])

rm(averages_2015_SAR, averages_2015_BED1, averages_2015_BED2, averages_2015_BED3, averages_2015_BED4)
rm(averages_2016_SAR, averages_2016_BED1, averages_2016_BED2, averages_2016_BED3, averages_2016_BED4)
rm(averages_2017_SAR, averages_2017_BED1, averages_2017_BED2, averages_2017_BED3, averages_2017_BED4)
rm(averages_2018_SAR, averages_2018_BED1, averages_2018_BED2, averages_2018_BED3, averages_2018_BED4)

rm(averages_2015_SAR_England, averages_2015_SAR_Scotland, averages_2015_SAR_Wales,
   averages_2015_SAR_Urban, averages_2015_SAR_Rural, averages_2015_SAR_Coastal, averages_2015_SAR_Inland)
rm(averages_2015_BED1_England, averages_2015_BED1_Scotland, averages_2015_BED1_Wales,
   averages_2015_BED1_Urban, averages_2015_BED1_Rural, averages_2015_BED1_Coastal, averages_2015_BED1_Inland)
rm(averages_2015_BED2_England, averages_2015_BED2_Scotland, averages_2015_BED2_Wales,
   averages_2015_BED2_Urban, averages_2015_BED2_Rural, averages_2015_BED2_Coastal, averages_2015_BED2_Inland)
rm(averages_2015_BED3_England, averages_2015_BED3_Scotland, averages_2015_BED3_Wales,
   averages_2015_BED3_Urban, averages_2015_BED3_Rural, averages_2015_BED3_Coastal, averages_2015_BED3_Inland)
rm(averages_2015_BED4_England, averages_2015_BED4_Scotland, averages_2015_BED4_Wales,
   averages_2015_BED4_Urban, averages_2015_BED4_Rural, averages_2015_BED4_Coastal, averages_2015_BED4_Inland)
rm(averages_2016_SAR_England, averages_2016_SAR_Scotland, averages_2016_SAR_Wales,
   averages_2016_SAR_Urban, averages_2016_SAR_Rural, averages_2016_SAR_Coastal, averages_2016_SAR_Inland)
rm(averages_2016_BED1_England, averages_2016_BED1_Scotland, averages_2016_BED1_Wales,
   averages_2016_BED1_Urban, averages_2016_BED1_Rural, averages_2016_BED1_Coastal, averages_2016_BED1_Inland)
rm(averages_2016_BED2_England, averages_2016_BED2_Scotland, averages_2016_BED2_Wales,
   averages_2016_BED2_Urban, averages_2016_BED2_Rural, averages_2016_BED2_Coastal, averages_2016_BED2_Inland)
rm(averages_2016_BED3_England, averages_2016_BED3_Scotland, averages_2016_BED3_Wales,
   averages_2016_BED3_Urban, averages_2016_BED3_Rural, averages_2016_BED3_Coastal, averages_2016_BED3_Inland)
rm(averages_2016_BED4_England, averages_2016_BED4_Scotland, averages_2016_BED4_Wales,
   averages_2016_BED4_Urban, averages_2016_BED4_Rural, averages_2016_BED4_Coastal, averages_2016_BED4_Inland)
rm(averages_2017_SAR_England, averages_2017_SAR_Scotland, averages_2017_SAR_Wales,
   averages_2017_SAR_Urban, averages_2017_SAR_Rural, averages_2017_SAR_Coastal, averages_2017_SAR_Inland)
rm(averages_2017_BED1_England, averages_2017_BED1_Scotland, averages_2017_BED1_Wales,
   averages_2017_BED1_Urban, averages_2017_BED1_Rural, averages_2017_BED1_Coastal, averages_2017_BED1_Inland)
rm(averages_2017_BED2_England, averages_2017_BED2_Scotland, averages_2017_BED2_Wales,
   averages_2017_BED2_Urban, averages_2017_BED2_Rural, averages_2017_BED2_Coastal, averages_2017_BED2_Inland)
rm(averages_2017_BED3_England, averages_2017_BED3_Scotland, averages_2017_BED3_Wales,
   averages_2017_BED3_Urban, averages_2017_BED3_Rural, averages_2017_BED3_Coastal, averages_2017_BED3_Inland)
rm(averages_2017_BED4_England, averages_2017_BED4_Scotland, averages_2017_BED4_Wales,
   averages_2017_BED4_Urban, averages_2017_BED4_Rural, averages_2017_BED4_Coastal, averages_2017_BED4_Inland)
rm(averages_2018_SAR_England, averages_2018_SAR_Scotland, averages_2018_SAR_Wales,
   averages_2018_SAR_Urban, averages_2018_SAR_Rural, averages_2018_SAR_Coastal, averages_2018_SAR_Inland)
rm(averages_2018_BED1_England, averages_2018_BED1_Scotland, averages_2018_BED1_Wales,
   averages_2018_BED1_Urban, averages_2018_BED1_Rural, averages_2018_BED1_Coastal, averages_2018_BED1_Inland)
rm(averages_2018_BED2_England, averages_2018_BED2_Scotland, averages_2018_BED2_Wales,
   averages_2018_BED2_Urban, averages_2018_BED2_Rural, averages_2018_BED2_Coastal, averages_2018_BED2_Inland)
rm(averages_2018_BED3_England, averages_2018_BED3_Scotland, averages_2018_BED3_Wales,
   averages_2018_BED3_Urban, averages_2018_BED3_Rural, averages_2018_BED3_Coastal, averages_2018_BED3_Inland)
rm(averages_2018_BED4_England, averages_2018_BED4_Scotland, averages_2018_BED4_Wales,
   averages_2018_BED4_Urban, averages_2018_BED4_Rural, averages_2018_BED4_Coastal, averages_2018_BED4_Inland)

RentData <- c(valAverages_2015_SAR_England, valAverages_2015_SAR_Scotland, valAverages_2015_SAR_Wales, 
              valAverages_2015_SAR_Urban, valAverages_2015_SAR_Rural, 
              valAverages_2015_SAR_Coastal, valAverages_2015_SAR_Inland, valAverages_2015_SAR)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015)
Room <- c("SAR", "SAR", "SAR", "SAR", "SAR", "SAR", "SAR", "SAR")
Summary_SAR_2015 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2016_SAR_England, valAverages_2016_SAR_Scotland, valAverages_2016_SAR_Wales, 
              valAverages_2016_SAR_Urban, valAverages_2016_SAR_Rural, 
              valAverages_2016_SAR_Coastal, valAverages_2016_SAR_Inland, valAverages_2016_SAR)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016)
Room <- c("SAR", "SAR", "SAR", "SAR", "SAR", "SAR", "SAR", "SAR")
Summary_SAR_2016 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2017_SAR_England, valAverages_2017_SAR_Scotland, valAverages_2017_SAR_Wales, 
              valAverages_2017_SAR_Urban, valAverages_2017_SAR_Rural, 
              valAverages_2017_SAR_Coastal, valAverages_2017_SAR_Inland, valAverages_2017_SAR)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017)
Room <- c("SAR", "SAR", "SAR", "SAR", "SAR", "SAR", "SAR", "SAR")
Summary_SAR_2017 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2018_SAR_England, valAverages_2018_SAR_Scotland, valAverages_2018_SAR_Wales, 
              valAverages_2018_SAR_Urban, valAverages_2018_SAR_Rural, 
              valAverages_2018_SAR_Coastal, valAverages_2018_SAR_Inland, valAverages_2018_SAR)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018)
Room <- c("SAR", "SAR", "SAR", "SAR", "SAR", "SAR", "SAR", "SAR")
Summary_SAR_2018 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2015_BED1_England, valAverages_2015_BED1_Scotland, valAverages_2015_BED1_Wales, 
              valAverages_2015_BED1_Urban, valAverages_2015_BED1_Rural, 
              valAverages_2015_BED1_Coastal, valAverages_2015_BED1_Inland, valAverages_2015_BED1)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015)
Room <- c("BED1", "BED1", "BED1", "BED1", "BED1", "BED1", "BED1", "BED1")
Summary_BED1_2015 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2016_BED1_England, valAverages_2016_BED1_Scotland, valAverages_2016_BED1_Wales, 
              valAverages_2016_BED1_Urban, valAverages_2016_BED1_Rural, 
              valAverages_2016_BED1_Coastal, valAverages_2016_BED1_Inland, valAverages_2016_BED1)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016)
Room <- c("BED1", "BED1", "BED1", "BED1", "BED1", "BED1", "BED1", "BED1")
Summary_BED1_2016 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2017_BED1_England, valAverages_2017_BED1_Scotland, valAverages_2017_BED1_Wales, 
              valAverages_2017_BED1_Urban, valAverages_2017_BED1_Rural, 
              valAverages_2017_BED1_Coastal, valAverages_2017_BED1_Inland, valAverages_2017_BED1)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017)
Room <- c("BED1", "BED1", "BED1", "BED1", "BED1", "BED1", "BED1", "BED1")
Summary_BED1_2017 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2018_BED1_England, valAverages_2018_BED1_Scotland, valAverages_2018_BED1_Wales, 
              valAverages_2018_BED1_Urban, valAverages_2018_BED1_Rural, 
              valAverages_2018_BED1_Coastal, valAverages_2018_BED1_Inland, valAverages_2018_BED1)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018)
Room <- c("BED1", "BED1", "BED1", "BED1", "BED1", "BED1", "BED1", "BED1")
Summary_BED1_2018 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2015_BED2_England, valAverages_2015_BED2_Scotland, valAverages_2015_BED2_Wales, 
              valAverages_2015_BED2_Urban, valAverages_2015_BED2_Rural, 
              valAverages_2015_BED2_Coastal, valAverages_2015_BED2_Inland, valAverages_2015_BED2)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015)
Room <- c("BED2", "BED2", "BED2", "BED2", "BED2", "BED2", "BED2", "BED2")
Summary_BED2_2015 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2016_BED2_England, valAverages_2016_BED2_Scotland, valAverages_2016_BED2_Wales, 
              valAverages_2016_BED2_Urban, valAverages_2016_BED2_Rural, 
              valAverages_2016_BED2_Coastal, valAverages_2016_BED2_Inland, valAverages_2016_BED2)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016)
Room <- c("BED2", "BED2", "BED2", "BED2", "BED2", "BED2", "BED2", "BED2")
Summary_BED2_2016 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2017_BED2_England, valAverages_2017_BED2_Scotland, valAverages_2017_BED2_Wales, 
              valAverages_2017_BED2_Urban, valAverages_2017_BED2_Rural, 
              valAverages_2017_BED2_Coastal, valAverages_2017_BED2_Inland, valAverages_2017_BED2)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017)
Room <- c("BED2", "BED2", "BED2", "BED2", "BED2", "BED2", "BED2", "BED2")
Summary_BED2_2017 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2018_BED2_England, valAverages_2018_BED2_Scotland, valAverages_2018_BED2_Wales, 
              valAverages_2018_BED2_Urban, valAverages_2018_BED2_Rural, 
              valAverages_2018_BED2_Coastal, valAverages_2018_BED2_Inland, valAverages_2018_BED2)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018)
Room <- c("BED2", "BED2", "BED2", "BED2", "BED2", "BED2", "BED2", "BED2")
Summary_BED2_2018 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2015_BED3_England, valAverages_2015_BED3_Scotland, valAverages_2015_BED1_Wales, 
              valAverages_2015_BED3_Urban, valAverages_2015_BED3_Rural, 
              valAverages_2015_BED3_Coastal, valAverages_2015_BED3_Inland, valAverages_2015_BED3)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015)
Room <- c("BED3", "BED3", "BED3", "BED3", "BED3", "BED3", "BED3", "BED3")
Summary_BED3_2015 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2016_BED3_England, valAverages_2016_BED3_Scotland, valAverages_2016_BED3_Wales, 
              valAverages_2016_BED3_Urban, valAverages_2016_BED3_Rural, 
              valAverages_2016_BED3_Coastal, valAverages_2016_BED3_Inland, valAverages_2016_BED3)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016)
Room <- c("BED3", "BED3", "BED3", "BED3", "BED3", "BED3", "BED3", "BED3")
Summary_BED3_2016 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2017_BED3_England, valAverages_2017_BED3_Scotland, valAverages_2017_BED3_Wales, 
              valAverages_2017_BED3_Urban, valAverages_2017_BED3_Rural, 
              valAverages_2017_BED3_Coastal, valAverages_2017_BED3_Inland, valAverages_2017_BED3)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017)
Room <- c("BED3", "BED3", "BED3", "BED3", "BED3", "BED3", "BED3", "BED3")
Summary_BED3_2017 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2018_BED3_England, valAverages_2018_BED3_Scotland, valAverages_2018_BED3_Wales, 
              valAverages_2018_BED3_Urban, valAverages_2018_BED3_Rural, 
              valAverages_2018_BED3_Coastal, valAverages_2018_BED3_Inland, valAverages_2018_BED3)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018)
Room <- c("BED3", "BED3", "BED3", "BED3", "BED3", "BED3", "BED3", "BED3")
Summary_BED3_2018 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2015_BED4_England, valAverages_2015_BED4_Scotland, valAverages_2015_BED4_Wales, 
              valAverages_2015_BED4_Urban, valAverages_2015_BED4_Rural, 
              valAverages_2015_BED4_Coastal, valAverages_2015_BED4_Inland, valAverages_2015_BED4)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015)
Room <- c("BED4", "BED4", "BED4", "BED4", "BED4", "BED4", "BED4", "BED4")
Summary_BED4_2015 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2016_BED4_England, valAverages_2016_BED4_Scotland, valAverages_2016_BED4_Wales, 
              valAverages_2016_BED4_Urban, valAverages_2016_BED4_Rural, 
              valAverages_2016_BED4_Coastal, valAverages_2016_BED4_Inland, valAverages_2016_BED4)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016)
Room <- c("BED4", "BED4", "BED4", "BED4", "BED4", "BED4", "BED4", "BED4")
Summary_BED4_2016 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2017_BED4_England, valAverages_2017_BED4_Scotland, valAverages_2017_BED4_Wales, 
              valAverages_2017_BED4_Urban, valAverages_2017_BED4_Rural, 
              valAverages_2017_BED4_Coastal, valAverages_2017_BED4_Inland, valAverages_2017_BED4)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017)
Room <- c("BED4", "BED4", "BED4", "BED4", "BED4", "BED4", "BED4", "BED4")
Summary_BED4_2017 <- data.frame(Type, RentData, Year, Room)

RentData <- c(valAverages_2018_BED4_England, valAverages_2018_BED4_Scotland, valAverages_2018_BED4_Wales, 
              valAverages_2018_BED4_Urban, valAverages_2018_BED4_Rural, 
              valAverages_2018_BED4_Coastal, valAverages_2018_BED4_Inland, valAverages_2018_BED4)
Type <- c("England", "Scotland", "Wales", "Urban", "Rural", "Coastal", "Inland", "Average")
Year <- c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018)
Room <- c("BED4", "BED4", "BED4", "BED4", "BED4", "BED4", "BED4", "BED4")
Summary_BED4_2018 <- data.frame(Type, RentData, Year, Room)

summaryTable <- rbind(Summary_SAR_2015, Summary_SAR_2016, Summary_SAR_2017, Summary_SAR_2018,
                      Summary_BED1_2015, Summary_BED1_2016, Summary_BED1_2017, Summary_BED1_2018, 
                      Summary_BED2_2015, Summary_BED2_2016, Summary_BED2_2017, Summary_BED2_2018, 
                      Summary_BED3_2015, Summary_BED3_2016, Summary_BED3_2017, Summary_BED3_2018, 
                      Summary_BED4_2015, Summary_BED4_2016, Summary_BED4_2017, Summary_BED4_2018)

displayText <- c(rep("2015 SAR", 8), rep("2016 SAR", 8), rep("2017 SAR", 8), rep("2018 SAR", 8),
                 rep("2015 BED1", 8), rep("2016 BED1", 8), rep("2017 BED1", 8), rep("2018 BED1", 8),
                 rep("2015 BED2", 8), rep("2016 BED2", 8), rep("2017 BED2", 8), rep("2018 BED2", 8),
                 rep("2015 BED3", 8), rep("2016 BED3", 8), rep("2017 BED3", 8), rep("2018 BED3", 8),
                 rep("2015 BED4", 8), rep("2016 BED4", 8), rep("2017 BED4", 8), rep("2018 BED4", 8))

summaryTable <- cbind(summaryTable, displayText)

rm(displayText)

rm(valAverages_2015_SAR_England, valAverages_2015_SAR_Scotland, valAverages_2015_SAR_Wales)
rm(valAverages_2015_SAR_Urban, valAverages_2015_SAR_Rural)
rm(valAverages_2015_SAR_Inland, valAverages_2015_SAR_Coastal)
rm(valAverages_2015_BED1_England, valAverages_2015_BED1_Scotland, valAverages_2015_BED1_Wales)
rm(valAverages_2015_BED1_Urban, valAverages_2015_BED1_Rural)
rm(valAverages_2015_BED1_Inland, valAverages_2015_BED1_Coastal)
rm(valAverages_2015_BED2_England, valAverages_2015_BED2_Scotland, valAverages_2015_BED2_Wales)
rm(valAverages_2015_BED2_Urban, valAverages_2015_BED2_Rural)
rm(valAverages_2015_BED2_Inland, valAverages_2015_BED2_Coastal)
rm(valAverages_2015_BED3_England, valAverages_2015_BED3_Scotland, valAverages_2015_BED3_Wales)
rm(valAverages_2015_BED3_Urban, valAverages_2015_BED3_Rural)
rm(valAverages_2015_BED3_Inland, valAverages_2015_BED3_Coastal)
rm(valAverages_2015_BED4_England, valAverages_2015_BED4_Scotland, valAverages_2015_BED4_Wales)
rm(valAverages_2015_BED4_Urban, valAverages_2015_BED4_Rural)
rm(valAverages_2015_BED4_Inland, valAverages_2015_BED4_Coastal)
rm(valAverages_2016_SAR_England, valAverages_2016_SAR_Scotland, valAverages_2016_SAR_Wales)
rm(valAverages_2016_SAR_Urban, valAverages_2016_SAR_Rural)
rm(valAverages_2016_SAR_Inland, valAverages_2016_SAR_Coastal)
rm(valAverages_2016_BED1_England, valAverages_2016_BED1_Scotland, valAverages_2016_BED1_Wales)
rm(valAverages_2016_BED1_Urban, valAverages_2016_BED1_Rural)
rm(valAverages_2016_BED1_Inland, valAverages_2016_BED1_Coastal)
rm(valAverages_2016_BED2_England, valAverages_2016_BED2_Scotland, valAverages_2016_BED2_Wales)
rm(valAverages_2016_BED2_Urban, valAverages_2016_BED2_Rural)
rm(valAverages_2016_BED2_Inland, valAverages_2016_BED2_Coastal)
rm(valAverages_2016_BED3_England, valAverages_2016_BED3_Scotland, valAverages_2016_BED3_Wales)
rm(valAverages_2016_BED3_Urban, valAverages_2016_BED3_Rural)
rm(valAverages_2016_BED3_Inland, valAverages_2016_BED3_Coastal)
rm(valAverages_2016_BED4_England, valAverages_2016_BED4_Scotland, valAverages_2016_BED4_Wales)
rm(valAverages_2016_BED4_Urban, valAverages_2016_BED4_Rural)
rm(valAverages_2016_BED4_Inland, valAverages_2016_BED4_Coastal)
rm(valAverages_2017_SAR_England, valAverages_2017_SAR_Scotland, valAverages_2017_SAR_Wales)
rm(valAverages_2017_SAR_Urban, valAverages_2017_SAR_Rural)
rm(valAverages_2017_SAR_Inland, valAverages_2017_SAR_Coastal)
rm(valAverages_2017_BED1_England, valAverages_2017_BED1_Scotland, valAverages_2017_BED1_Wales)
rm(valAverages_2017_BED1_Urban, valAverages_2017_BED1_Rural)
rm(valAverages_2017_BED1_Inland, valAverages_2017_BED1_Coastal)
rm(valAverages_2017_BED2_England, valAverages_2017_BED2_Scotland, valAverages_2017_BED2_Wales)
rm(valAverages_2017_BED2_Urban, valAverages_2017_BED2_Rural)
rm(valAverages_2017_BED2_Inland, valAverages_2017_BED2_Coastal)
rm(valAverages_2017_BED3_England, valAverages_2017_BED3_Scotland, valAverages_2017_BED3_Wales)
rm(valAverages_2017_BED3_Urban, valAverages_2017_BED3_Rural)
rm(valAverages_2017_BED3_Inland, valAverages_2017_BED3_Coastal)
rm(valAverages_2017_BED4_England, valAverages_2017_BED4_Scotland, valAverages_2017_BED4_Wales)
rm(valAverages_2017_BED4_Urban, valAverages_2017_BED4_Rural)
rm(valAverages_2017_BED4_Inland, valAverages_2017_BED4_Coastal)
rm(valAverages_2018_SAR_England, valAverages_2018_SAR_Scotland, valAverages_2018_SAR_Wales)
rm(valAverages_2018_SAR_Urban, valAverages_2018_SAR_Rural)
rm(valAverages_2018_SAR_Inland, valAverages_2018_SAR_Coastal)
rm(valAverages_2018_BED1_England, valAverages_2018_BED1_Scotland, valAverages_2018_BED1_Wales)
rm(valAverages_2018_BED1_Urban, valAverages_2018_BED1_Rural)
rm(valAverages_2018_BED1_Inland, valAverages_2018_BED1_Coastal)
rm(valAverages_2018_BED2_England, valAverages_2018_BED2_Scotland, valAverages_2018_BED2_Wales)
rm(valAverages_2018_BED2_Urban, valAverages_2018_BED2_Rural)
rm(valAverages_2018_BED2_Inland, valAverages_2018_BED2_Coastal)
rm(valAverages_2018_BED3_England, valAverages_2018_BED3_Scotland, valAverages_2018_BED3_Wales)
rm(valAverages_2018_BED3_Urban, valAverages_2018_BED3_Rural)
rm(valAverages_2018_BED3_Inland, valAverages_2018_BED3_Coastal)
rm(valAverages_2018_BED4_England, valAverages_2018_BED4_Scotland, valAverages_2018_BED4_Wales)
rm(valAverages_2018_BED4_Urban, valAverages_2018_BED4_Rural)
rm(valAverages_2018_BED4_Inland, valAverages_2018_BED4_Coastal)

rm(valAverages_2015_SAR, valAverages_2015_BED1, valAverages_2015_BED2, valAverages_2015_BED3, valAverages_2015_BED4)
rm(valAverages_2016_SAR, valAverages_2016_BED1, valAverages_2016_BED2, valAverages_2016_BED3, valAverages_2016_BED4)
rm(valAverages_2017_SAR, valAverages_2017_BED1, valAverages_2017_BED2, valAverages_2017_BED3, valAverages_2017_BED4)
rm(valAverages_2018_SAR, valAverages_2018_BED1, valAverages_2018_BED2, valAverages_2018_BED3, valAverages_2018_BED4)

rm(Summary_SAR_2015, Summary_SAR_2016, Summary_SAR_2017, Summary_SAR_2018,
   Summary_BED1_2015, Summary_BED1_2016, Summary_BED1_2017, Summary_BED1_2018, 
   Summary_BED2_2015, Summary_BED2_2016, Summary_BED2_2017, Summary_BED2_2018, 
   Summary_BED3_2015, Summary_BED3_2016, Summary_BED3_2017, Summary_BED3_2018, 
   Summary_BED4_2015, Summary_BED4_2016, Summary_BED4_2017, Summary_BED4_2018)

rm(RentData, Room, Type, Year)

#Exporting all the features from the Global Environment, so loading it online wont take as long
writeOGR(Cities, dsn = "Deploy", layer = "Cities", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Coastal, dsn = "Deploy", layer = "Filter_Coastal", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Inland, dsn = "Deploy", layer = "Filter_Inland", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Urban, dsn = "Deploy", layer = "Filter_Urban", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Rural, dsn = "Deploy", layer = "Filter_Rural", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_England, dsn = "Deploy", layer = "Filter_England", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Scotland, dsn = "Deploy", layer = "Filter_Scotland", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Wales, dsn = "Deploy", layer = "Filter_Wales", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(RentDataPolygons, dsn = "Deploy", layer = "RentDataPolygons", driver = "ESRI Shapefile", overwrite_layer = TRUE)
write.csv(BRMA_BoundingTable,  file ="Deploy/BRMA_BoundingTable.csv")
write.csv(RentDataTable,  file ="Deploy/RentDataTable.csv")
write.csv(summaryTable,  file ="Deploy/summaryTable.csv")