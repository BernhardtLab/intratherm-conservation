### getting elevation for terrestrial populations in the intratherm database 
library(elevatr)
library(raster)
library(sp)
library(rgdal)
library(rlist)
library(tidyverse)
library(rgbif)
library(ncdf4)
library(dplyr)


## 		GETTING GRID SQUARE COORDINATES		##
##############################################
## record lat and long of centre of each grid cell population falls in in Berkeley Earth data
## (recall: grid cells of NA populations were adjusted, so change these grid cells)
## will then get average elevation across each of these grid squares 

## read in Berkeley Earth grid square vectors 
filename <- paste("./Berkeley_Tavg/Complete_TAVG_Daily_LatLong1_1880.nc", sep = "")
ncfile <- nc_open(filename)

## create variables for things needed to use data
lat <- ncvar_get(ncfile, "latitude")
long <- ncvar_get(ncfile, "longitude")

## close the file
nc_close(ncfile)

## subset data to only terrestrial populations with known location
elev_sub <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv") 
elev_sub <- elev_sub %>%
	filter(realm_general2 == "Terrestrial") %>%
	filter(!is.na(latitude)) %>%
	filter(!is.na(longitude))

elev <- elev_sub %>% ##subset to unique pairs of lat long 
	subset(select=c(latitude, longitude, elevation_of_collection)) %>% 
	unique()

latitude_of_raster <- c() 
longitude_of_raster <- c()
## "raster_of_latitude" and "raster_of_longitude" represent the centre coordinates of 1 degree lat x 1 degree long grid cells 

## get grid square coordinates for each population
num_unique <- 1
while (num_unique < nrow(elev)+1) {
	loc_long_index <- which.min(abs(long - elev$longitude[num_unique]))
	loc_lat_index <- which.min(abs(lat - elev$latitude[num_unique]))
	
	latitude_of_raster <- append(latitude_of_raster, lat[loc_lat_index])
	longitude_of_raster <- append(longitude_of_raster, long[loc_long_index])
	
	num_unique <- num_unique + 1
}

## manually adjust ones that were changed when no temp data was retrieved: 
longitude_of_raster[15] <- -70.5
latitude_of_raster[15] <- 41.5

longitude_of_raster[16:20] <- -60.5
latitude_of_raster[16:20] <- 14.5

longitude_of_raster[66] <- NA
latitude_of_raster[66] <- NA

longitude_of_raster[85] <- NA
latitude_of_raster[85] <- NA
longitude_of_raster[87] <- NA
latitude_of_raster[87] <- NA

longitude_of_raster[89] <- 150.5
latitude_of_raster[89] <- -33.5
longitude_of_raster[101] <- 150.5
latitude_of_raster[101] <- -33.5

longitude_of_raster[99] <- 150.5
latitude_of_raster[99] <- -33.5

longitude_of_raster[95] <- 146.5
latitude_of_raster[95] <- -38.5

longitude_of_raster[185] <- NA
latitude_of_raster[185] <- NA


elev$latitude_of_raster <- latitude_of_raster
elev$longitude_of_raster <- longitude_of_raster

elev_sub <- left_join(elev_sub, elev) 





##      GETTING RASTER ELEVATION      ##
########################################
## first, get the average elevation across each raster we took Berekely Earth temperature data from 

data <- elev_sub

unique_locs <- data %>%
	subset(select=c(latitude_of_raster, longitude_of_raster)) %>% 
	filter(!is.na(latitude_of_raster) & !is.na(longitude_of_raster)) %>%
	unique() 

latitude <- unique_locs$latitude_of_raster[which(!is.na(unique_locs$latitude_of_raster))]
longitude <- unique_locs$longitude_of_raster[which(!is.na(unique_locs$longitude_of_raster))]


## 1. create a SpatialPolygonsDataFrame of 1 degree lat x 1 degree long rectangles representing each grid cell we need elevation for

## set projection to lat long WGS84
myProj <- "+proj=longlat +datum=WGS84 +ellps=WGS84"

raster_means <- c()

## for each grid cell we took temp data from:
i <- 1
while (i < nrow(unique_locs)+1) {
	
	## 1. create a rectangle representing grid cell:
	## draw square with coords corresponding to corners 
	ybottom = unique_locs$latitude_of_raster[i] - 0.5
	xleft = unique_locs$longitude_of_raster[i] - 0.5
	ytop = unique_locs$latitude_of_raster[i] + 0.5
	xright = unique_locs$longitude_of_raster[i] + 0.5
	
	print("Drawing grid cell...")
	rectangle <- Polygon(cbind(c(xleft,xleft,xright,xright,xleft),c(ybottom, ytop, ytop, ybottom, ybottom)))
	
	poly <- Polygons(list(rectangle), ID = "A")
	
	## create spatial object of polygon 
	spPolygon = SpatialPolygons(list(poly))
	## assign projection
	proj4string(spPolygon) <- myProj
	
	## create dataframe: 
	df = matrix(data = c(0))
	rownames(df) = "A"
	spp = SpatialPolygonsDataFrame(spPolygon, data = as.data.frame(df))
	
	
	## 2. get GDEM raster object with data for every cell:
	lat_og <- unique_locs$latitude_of_raster[i]
	long_og <- unique_locs$longitude_of_raster[i]
	
	y <- c(lat_og) ## latitudes of points to get GDEM data for 
	x <- c(long_og) ## longitudes of points to get GDEM data for 
	
	## add 9 points in and around edge of grid cell to query to ensure whole grid cell GDEM data is downloaded
	n = 1
	while (n < 3) {
		if(n < 2) {
			latitude <- lat_og + 0.5
			longitude <- long_og + 0.5
			
			y <- append(y, latitude)
			x <- append(x, longitude)
		}
		if (n >= 2) {
			latitude <- lat_og - 0.5
			longitude <- long_og - 0.5
			
			y <- append(y, latitude)
			x <- append(x, longitude)
		}
		n = n + 1
	}
	loc_df <- data.frame(crossing(x, y)) ## data frame with the 9 query coordinates 
	
	
	## get GDEM data for loc_df
	print(paste("Getting GDEM data for grid cell ", i, "...", sep = ""))
	cell_raster <- get_elev_raster(locations = loc_df, prj = myProj, z=10)
	##plot(cell_raster)
	
	## check that dimensions overlap and projections are the same:
	##show(cell_raster)
	##show(spp)
	##plot(cell_raster)
	##plot(spp, add=TRUE)
	
	
	## 3. calculate average elevation in each grid cell and record: 
	print(paste("Done! Extracting pixels under grid cell ", i, "...", sep = ""))
	under_cell <- data.frame(extract(cell_raster, spp)) ## extract elevation of all GDEM pixels under the grid cell rectangle
	colnames(under_cell) <- c("pixel_elevation")
	print("Done! Calculating mean... ")
	under_cell <- subset(under_cell, subset = under_cell$pixel_elevation >= 0) ## remove all negative elevations corresponding to ocean 
	raster_mean <- lapply(under_cell, FUN=mean) ## calculate the mean of the pixels to get mean elevation of the grid cell
	raster_means <- append(raster_means, raster_mean)
	
	
	## 4. move on to the next grid cell 
	print(paste("Finished grid cell number ", i, ", moving on to next cell...", sep = ""))
	i <- i + 1
}

unique_locs$raster_mean <- unlist(raster_means, use.names=FALSE)
precious_elevation <- unique_locs

unique_locs <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_elevation.rds")


merged <- left_join(data, unique_locs) 




##       GETTING POINT ELEVATION      ##
########################################
## remove locations in which studies reported the collection point elevation 
unique_locs <- data %>%
	dplyr::select(latitude, longitude, elevation_of_collection) %>% 
	unique()

no_elev <- unique_locs %>%
	filter(is.na(elevation_of_collection)) %>%
	mutate(decimalLatitude = latitude, decimalLongitude = longitude) %>% 
	dplyr::select(-latitude, -longitude, -elevation_of_collection) %>% 
	unique()

## set parameter elevation_model to 'astergdem' to get 30m x 30m elevation at exact collection locations
## this will be used to correct the average temperature across the grid cell for differing elevations
point_elev <- elevation(input = no_elev, elevation_model = "astergdem", username = "nicole_a_moore")

point_elev <- point_elev %>%
	mutate(elevation_of_collection = elevation_geonames) %>% 
	dplyr::select(-elevation_geonames)


## put all data together:
no_elev$elevation_of_collection <- as.numeric(point_elev$elevation_of_collection)
colnames(no_elev) <- c("latitude","longitude", "elevation_of_collection")


## flag all elevation_of_collection that were reported by study
data <- data %>% 
	mutate(elevation_was_reported = ifelse(is.na(elevation_of_collection), "FALSE", "TRUE"))

data_hasElev <- data %>%
	filter(elevation_was_reported == TRUE) 

data_noElev <- data %>%
	dplyr::select(-elevation_of_collection) %>%
	filter(elevation_was_reported == FALSE) %>%
	left_join(., no_elev)

data <- rbind(data_hasElev, data_noElev) %>%
	dplyr::select(-elevation_was_reported)


## final data: 
merged <- merged %>%
	dplyr::select(-elevation_of_collection)

raster_and_point <- left_join(data, merged) %>%
	dplyr::select(-latitude_of_raster, -longitude_of_raster)

## merge with non-terrestrial data:
old_squeaky <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv") %>%
	filter(realm_general2 != "Terrestrial") %>%
	mutate(raster_mean = NA)

new_squeaky <- rbind(old_squeaky, raster_and_point)

write.csv(new_squeaky, "./data-processed/intratherm-with-elev.csv", row.names = FALSE)
