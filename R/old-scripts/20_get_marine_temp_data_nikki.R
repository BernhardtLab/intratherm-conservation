## extracting marine temperature data from NOAA OI SST V2 High Resolution Dataset 
## SST, Daily Optimum Interpolation (OI), AVHRR Only, Version 2, Final+Preliminary, 1981-present
library(tidyverse)
library(rerddap)
library(ncdf4)

## bring in population data
cadillac <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")

## filter out rows of data we cannot use and that are not marine
marine <- cadillac %>%
	subset(subset = !is.na(latitude)) %>%
	subset(subset = !is.na(longitude)) %>%
	subset(realm_general2 == "Marine")

marine <- droplevels(marine)

## get rid of populations with same latitude, longitude and elevation since all will have the same temp data
unique_pairs <- marine[!duplicated(marine[,c("latitude", "longitude", "elevation_of_collection")]),]


## load info about NOAA data:
info <- info("ncdcOisst21Agg_LonPM180")


## make latitude and longitude vectors based on NOAA format
## longitude: Uniform grid with centers from -179.875 to 179.875 by 0.25 degrees.
lon <- rep(-179.875, times = 1439)
n = 2
while (n < 1441) {
	lon[n] <- lon[n -1] + 0.25
	n = n+1
}
## latitude: Uniform grid with centers from -89.875 to 89.875 by 0.25 degrees.
lat <- rep(-89.875, times = 719)
n = 2
while (n < 721) {
	lat[n] <- lat[n -1] + 0.25
	n = n+1
}


unique_pairs$grid_lat <- c()
unique_pairs$grid_lon <- c()

## find closest lat lon grid cell to each population collection location 
num_unique <- 1
while (num_unique < length(unique_pairs$population_id) + 1) {
	loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[num_unique]))
	loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
	
	unique_pairs$grid_lon[num_unique] <- lon[loc_lon_index]
	unique_pairs$grid_lat[num_unique] <- lat[loc_lat_index]
	
	num_unique = num_unique + 1
}


## create dataframe for temp data
## nValues for time attribute = 14096
temperature_data <- data.frame(matrix(nrow = 14096))
colnames(temperature_data) = c("date")

## loop through each population getting temp data for its grid cell and adding to temp data
num_unique <- 1
while (num_unique < length(unique_pairs$population_id) + 1) {
	print(paste("On population number", num_unique))
	time_series <- griddap(info,
						   time = c("1981-09-01", "2020-04-04"), 
						   latitude = c(unique_pairs$grid_lat[num_unique],unique_pairs$grid_lat[num_unique]),
						   longitude = c(unique_pairs$grid_lon[num_unique], unique_pairs$grid_lon[num_unique]),
						   url = "https://upwell.pfeg.noaa.gov/erddap/")
	temps <- time_series$data$sst
	print(paste("Successfully got time series of length", length(temps)))
	
	if (num_unique == 1) {
		times <- time_series$data$time
		temperature_data$date <- times
	}
	
	temperature_data$temp <- temps
	pop_id <- paste(unique_pairs$population_id[num_unique], unique_pairs$longitude[num_unique], sep = "_")
	colnames(temperature_data)[num_unique+1]<- pop_id
	
	print("Stored data in temperature_data and moving on to next population!")
	
	num_unique <- num_unique + 1
}

## save
precious_marine_temps <- temperature_data
temperature_data <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_marine_temps.rds")


## fix NAs:
isNA <- temperature_data[,which(is.na(temperature_data[1,]))]
uniqueNA <- unique_pairs[which(is.na(temperature_data[1,]))-1,]

## manually change brackish and coastal species collection locations to grid cells with temperature data 
brackish <-c(612, 1096, 1430, 1165, 969, 2987, 2947, 2763)
 

## find new grid squares by looking on google maps:
uniqueNA$grid_lon[which(uniqueNA$intratherm_id == 612)] <- -64.375
uniqueNA$grid_lat[which(uniqueNA$intratherm_id == 612)] <- 10.625

uniqueNA$grid_lon[which(uniqueNA$intratherm_id == 1096)] <- -122.875
uniqueNA$grid_lat[which(uniqueNA$intratherm_id == 1096)] <- 37.625

uniqueNA$grid_lon[which(uniqueNA$intratherm_id == 1430)] <- -9.375
uniqueNA$grid_lat[which(uniqueNA$intratherm_id == 1430)] <- 38.625

uniqueNA$grid_lon[which(uniqueNA$intratherm_id == 1165)] <- -122.875
uniqueNA$grid_lat[which(uniqueNA$intratherm_id == 1165)] <- 37.625

uniqueNA$grid_lon[which(uniqueNA$intratherm_id == 2987)] <- 10.375
uniqueNA$grid_lat[which(uniqueNA$intratherm_id == 2987)] <- 54.625

uniqueNA$grid_lon[which(uniqueNA$intratherm_id == 2947)] <- -128.125
uniqueNA$grid_lat[which(uniqueNA$intratherm_id == 2947)] <- 50.875

uniqueNA$grid_lon[which(uniqueNA$intratherm_id == 969)] <- -70.625
uniqueNA$grid_lat[which(uniqueNA$intratherm_id == 969)] <- 43.125

uniqueNA$grid_lon[which(uniqueNA$intratherm_id == 2763)] <- -116.875
uniqueNA$grid_lat[which(uniqueNA$intratherm_id == 2763)] <- 31.875

## create dataframe for temp data
## nValues for time attribute = 14096
temperature_data <- data.frame(matrix(nrow = 14096))
colnames(temperature_data) = c("date")

## loop through each population getting temp data for its grid cell and adding to temp data
num_unique <- 1
while (num_unique < length(uniqueNA$population_id) + 1) {
	print(paste("On population number", num_unique))
	time_series <- griddap(info,
						   time = c("1981-09-01", "2020-04-04"), 
						   latitude = c(uniqueNA$grid_lat[num_unique],uniqueNA$grid_lat[num_unique]),
						   longitude = c(uniqueNA$grid_lon[num_unique], uniqueNA$grid_lon[num_unique]),
						   url = "https://upwell.pfeg.noaa.gov/erddap/")
	temps <- time_series$data$sst
	print(paste("Successfully got time series of length", length(temps)))
	
	if (num_unique == 1) {
		times <- time_series$data$time
		temperature_data$date <- times
	}
	
	temperature_data$temp <- temps
	pop_id <- paste(uniqueNA$population_id[num_unique], uniqueNA$longitude[num_unique], sep = "_")
	colnames(temperature_data)[num_unique+1]<- pop_id
	
	print("Stored data in temperature_data and moving on to next population!")
	
	num_unique <- num_unique + 1
}

precious_marine_temps_NA <- temperature_data


NA_temps <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_marine_temps_NA.rds")
NA_temps <- NA_temps %>% 
	select(-date)


## combine missing temps with other temps:
temperature_data <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_marine_temps.rds")

temperature_data <- temperature_data[,-which(is.na(temperature_data[1,]))] ## remove NA columns
temperature_data <- cbind(temperature_data, NA_temps) ## add columns that were NA back


## add rows for populations with the same lat, long and elevation but different species 
## will have same temperature data 
populations <- marine[!duplicated(marine[,c("genus_species", "latitude", "longitude", "elevation_of_collection")]),]

i <- 1
while (i < length(unique_pairs$population_id) + 1) {
	z <- 1
	while (z < length(populations$population_id) + 1) {
		same_pop <- (unique_pairs$latitude[i] == populations$latitude[z] & 
					 	unique_pairs$longitude[i] == populations$longitude[z] & 
					 	unique_pairs$genus_species[i] == populations$genus_species[z])
		
		same_loc <- (unique_pairs$latitude[i] == populations$latitude[z] & 
					 	unique_pairs$longitude[i] == populations$longitude[z])
		
		if (!same_pop & same_loc) {
			temperature_data$temps <- temperature_data[,i+1]
			pop_id <- paste(populations$population_id[z], populations$longitude[z], sep = "_")
			colnames(temperature_data)[length(temperature_data)]<- pop_id
			same_pop = FALSE
			same_loc = FALSE
			z = z+1
		}
		else {
			z = z+1
		}
	}
	i = i+1
}


## write to file:
write.csv(temperature_data, "./data-processed/intratherm-marine-temp-data.csv", row.names = FALSE)

 

