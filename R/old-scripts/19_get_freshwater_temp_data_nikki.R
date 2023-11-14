## getting modelled monthly freshwater temperature time series' from 1981-2014 for each freshwater population 
## uses data from https://zenodo.org/record/3337659#.Xr3YcxNKjOR

library(ncdf4)
library(tidyverse)

## open nc file and get lat lon and time vectors
filename <- paste("waterTemperature_monthly_1981-2014.nc", sep = "")
ncfile <- nc_open(filename)


lon <- ncvar_get(ncfile, "lon") ## units: degrees - intervals of 0.0833 (probably 0.5/60)
lat <- ncvar_get(ncfile, "lat") ## units: degrees - intervals of 0.08333 (probably 0.5/60)
time <- ncvar_get(ncfile, "time") ## units: days since 1901-01-01

## close the file
nc_close(ncfile)

## bring in population data
cadillac <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")

## filter out rows of data we cannot use and that are not freshwater
freshwater <- cadillac %>%
	subset(subset = !is.na(latitude)) %>%
	subset(subset = !is.na(longitude)) %>%
	filter(realm_general2 == "Freshwater")

freshwater <- droplevels(freshwater)

## get rid of populations with same latitude, longitude and elevation since all will have the same temp data
unique_pairs <- freshwater[!duplicated(freshwater[,c("latitude", "longitude", "elevation_of_collection")]),]

## temps:
temperature_data <- data.frame(matrix(nrow = length(time)))
colnames(temperature_data) = c("date")
temperature_data$date <- time

## for each population:
num_unique <- 1
while (num_unique < length(unique_pairs$intratherm_id) + 1) {
	
	## find closest lat lon coordinates to population collection location
	loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[num_unique]))
	loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
	
	## get waterTemp time series for closest lat lon coordinates 
	ncfile <- nc_open(filename)
	waterTemp <- ncvar_get(ncfile, "waterTemp", start = c(loc_lon_index, loc_lat_index, 1), count = c(1, 1, -1))
	nc_close(ncfile)
	
	## add to column in temperature_data and rename after column's population_id with longitude added onto the end
	temperature_data$temp <- waterTemp
	pop_id <- paste(unique_pairs$population_id[num_unique], unique_pairs$longitude[num_unique], sep = "_")
	colnames(temperature_data)[num_unique+1]<- pop_id

	num_unique <- num_unique + 1
}

## save to RDS
precious_temps <- temperature_data

temperature_data <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_temps.rds")

## convert from degrees K to degrees C
converted <- temperature_data
converted[, 2:193] <- converted[, 2:193] - 273.15

## convert date to years and month fractions 
## starts at Jan 1981

months_num <- c((1 - 0.5)/12, (2 - 0.5)/12, (3 - 0.5)/12, (4 - 0.5)/12, (5 - 0.5)/12, (6 - 0.5)/12, (7 - 0.5)/12, (8 - 0.5)/12, (9 - 0.5)/12, (10 - 0.5)/12, (11 - 0.5)/12, (12-0.5)/12)

months_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

date_num <- c()
date_name <- c()
year <- 1981

x <- 1
while (x < 408/12 + 1) {
		
	date_num <- append(date_num, year + months_num)
	date_name <- append(date_name, paste(year, months_name))

	year = year + 1
	x = x + 1
}


## add columns
converted$date <- as.vector(date_name)
converted$date_number <- as.vector(date_num)

converted <- converted %>%
	dplyr::select(date, date_number, everything())

temperature_data <- converted

## add rows for populations with the same lat, long and elevation but different species 
## will have same temperature data 
populations <- freshwater[!duplicated(freshwater[,c("genus_species", "latitude", "longitude", "elevation_of_collection")]),]

z = 1
while (z < length(populations$population_id) + 1) {
	i = 1
	while (i < length(unique_pairs$population_id) + 1) {
		if (is.na(unique_pairs$elevation_of_collection[i]) || is.na(populations$elevation_of_collection[z])) {
			same_pop <- (unique_pairs$latitude[i] == populations$latitude[z] & 
						 	unique_pairs$longitude[i] == populations$longitude[z] & 
						 	unique_pairs$genus_species[i] == populations$genus_species[z] &
						 	is.na(unique_pairs$elevation_of_collection[i]) & is.na(populations$elevation_of_collection[z]))
			same_loc <- (unique_pairs$latitude[i] == populations$latitude[z] & 
						 	unique_pairs$longitude[i] == populations$longitude[z] &
						 	is.na(unique_pairs$elevation_of_collection[i]) & is.na(populations$elevation_of_collection[z]))
		}
		else {
			same_pop <- (unique_pairs$latitude[i] == populations$latitude[z] & 
						 	unique_pairs$longitude[i] == populations$longitude[z] & 
						 	unique_pairs$genus_species[i] == populations$genus_species[z] & 
						 	unique_pairs$elevation_of_collection[i] == populations$elevation_of_collection[z])
			same_loc <- (unique_pairs$latitude[i] == populations$latitude[z] & 
						 	unique_pairs$longitude[i] == populations$longitude[z] &
						 	unique_pairs$elevation_of_collection[i] == populations$elevation_of_collection[z])
		}
		if (!same_pop & same_loc) {
			temperature_data$temps <- temperature_data[,i+2]
			pop_id <- paste(populations$population_id[z], populations$longitude[z], sep = "_")
			colnames(temperature_data)[length(temperature_data)]<- pop_id
			same_pop = FALSE
			same_loc = FALSE
			i = i+1
		}
		else {
			same_pop = FALSE
			same_loc = FALSE
			i = i + 1
		}
	}
	z = z+1
}



## write temperature file to processed data 
write.csv(temperature_data, "./data-processed/intratherm-freshwater-temp-data.csv")






## getting daily data that Rens gave me:
########################################
## this data has a 30' spatial resolution 
## he says he will try and get me 5' resolution, but in the meantime we can use this 

library(ncdf4)
library(tidyverse)

## open nc file and get lat lon and time vectors
filename <- paste("watertemperature_wfd_historical_1958-2001.nc", sep = "")
ncfile <- nc_open(filename)

lon <- ncvar_get(ncfile, "longitude") ## units: degrees - intervals of 0.5 (30')
lat <- ncvar_get(ncfile, "latitude") ## units: degrees - intervals of 0.5 (30')
time <- ncvar_get(ncfile, "time") ## units: hours since 1901-01-01 (first time is 1958-01-01) 

## close the file
nc_close(ncfile)


## bring in population data
cadillac <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")

## filter out rows of data we cannot use and that are not freshwater
freshwater <- cadillac %>%
	subset(subset = !is.na(latitude)) %>%
	subset(subset = !is.na(longitude)) %>%
	filter(realm_general2 == "Freshwater")

freshwater <- droplevels(freshwater)

## get rid of populations with same latitude, longitude and elevation since all will have the same temp data
unique_pairs <- freshwater[!duplicated(freshwater[,c("latitude", "longitude", "elevation_of_collection")]),]

## temps:
temperature_data <- data.frame(matrix(nrow = length(time)))
colnames(temperature_data) = c("date")
temperature_data$date <- time

## for each population:
num_unique <- 1
while (num_unique < length(unique_pairs$intratherm_id) + 1) {
	
	## find closest lat lon coordinates to population collection location
	loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[num_unique]))
	loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
	
	## get waterTemp time series for closest lat lon coordinates 
	ncfile <- nc_open(filename)
	waterTemp <- ncvar_get(ncfile, "waterTemperature", start = c(loc_lon_index, loc_lat_index, 1), count = c(1, 1, -1))
	nc_close(ncfile)
	
	## add to column in temperature_data and rename after column's population_id with longitude added onto the end
	temperature_data$temp <- waterTemp
	pop_id <- paste(unique_pairs$population_id[num_unique], unique_pairs$longitude[num_unique], sep = "_")
	colnames(temperature_data)[num_unique+1]<- pop_id
	
	num_unique <- num_unique + 1
}

## save to RDS
precious_temps_freshdaily <- temperature_data

temperature_data <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/precious_temps_freshdaily.rds")

## convert from degrees K to degrees C
converted <- temperature_data
converted[, 2:196] <- converted[, 2:196] - 273.15

## convert date 
## starts at 1958-01-01
year <- c(round(0.5/365, digits = 3))
leap_year <- c(round(0.5/366, digits = 3))

i = 1
while (i < 366) {
	if (i < 365) {
		year = append(year, round((i+0.5)/365, digits = 3))
	}
	leap_year = append(leap_year, round((i+0.5)/366, digits = 3))
	i = i+1
}

rep = 1958
last_year = 2002
date <- c()

while (rep < last_year) {
	if (rep %% 4 == 0){
		if (rep == 1900) { 
			date <- append(date, rep+year, after = length(date))
		}
		else {
			date <- append(date, rep+leap_year, after = length(date))
		}
	}
	else {
		date <- append(date, rep+year, after = length(date))
	}
	rep = rep + 1
}

## replace column for date
converted$date <- as.vector(date)


temperature_data <- converted

## add rows for populations with the same lat, long and elevation but different species 
## will have same temperature data 
populations <- freshwater[!duplicated(freshwater[,c("genus_species", "latitude", "longitude", "elevation_of_collection")]),]

z = 1
while (z < length(populations$population_id) + 1) {
	i = 1
	while (i < length(unique_pairs$population_id) + 1) {
		if (is.na(unique_pairs$elevation_of_collection[i]) || is.na(populations$elevation_of_collection[z])) {
			same_pop <- (unique_pairs$latitude[i] == populations$latitude[z] & 
						 	unique_pairs$longitude[i] == populations$longitude[z] & 
						 	unique_pairs$genus_species[i] == populations$genus_species[z] &
						 	is.na(unique_pairs$elevation_of_collection[i]) & is.na(populations$elevation_of_collection[z]))
			same_loc <- (unique_pairs$latitude[i] == populations$latitude[z] & 
						 	unique_pairs$longitude[i] == populations$longitude[z] &
						 	is.na(unique_pairs$elevation_of_collection[i]) & is.na(populations$elevation_of_collection[z]))
		}
		else {
			same_pop <- (unique_pairs$latitude[i] == populations$latitude[z] & 
						 	unique_pairs$longitude[i] == populations$longitude[z] & 
						 	unique_pairs$genus_species[i] == populations$genus_species[z] & 
						 	unique_pairs$elevation_of_collection[i] == populations$elevation_of_collection[z])
			same_loc <- (unique_pairs$latitude[i] == populations$latitude[z] & 
						 	unique_pairs$longitude[i] == populations$longitude[z] &
						 	unique_pairs$elevation_of_collection[i] == populations$elevation_of_collection[z])
		}
		if (!same_pop & same_loc) {
			temperature_data$temps <- temperature_data[,i+1]
			pop_id <- paste(populations$population_id[z], populations$longitude[z], sep = "_")
			colnames(temperature_data)[length(temperature_data)]<- pop_id
			same_pop = FALSE
			same_loc = FALSE
			i = i+1
		}
		else {
			same_pop = FALSE
			same_loc = FALSE
			i = i+1
		}
	}
	z = z+1
}

## write temperature file to processed data 
write.csv(temperature_data, "./data-processed/intratherm-freshwater-temp-data-daily.csv", row.names = FALSE)
