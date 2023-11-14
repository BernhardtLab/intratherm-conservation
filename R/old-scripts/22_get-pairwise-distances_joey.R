## calculating pairwise population distances 
library(tidyverse)
library(utils)
library(geosphere)
library(cowplot)
theme_set(theme_cowplot())

## calcualte differences in distance and temperature between all populations of each species 
pop_difs <- initialize_pairwise_differences() 

## convert distance between to km 
pop_difs <- pop_difs %>%
	mutate(distance_km = distance/1000)

## add realm column 
realm <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv") %>%
	select(genus_species, realm_general2) %>%
	filter(!duplicated(genus_species))
pop_difs <- inner_join(realm, pop_difs, by = "genus_species")

write_csv(pop_difs, "data-processed/pop_difs.csv")

##plot all realms 
pop_difs %>% 
	ggplot(aes(x = distance_km, y = temp_difference)) + geom_point() +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)") 

## split by realm
pop_difs %>% 
	filter(realm_general2 == "Marine") %>%
	ggplot(aes(x = distance_km, y = temp_difference,col = realm_general2)) + geom_point() +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)") 

pop_difs %>% 
	filter(realm_general2 == "Terrestrial") %>%
	ggplot(aes(x = distance_km, y = temp_difference,col = realm_general2)) + geom_point() +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)") 

pop_difs %>% 
	filter(realm_general2 == "Freshwater") %>%
	ggplot(aes(x = distance_km, y = temp_difference,col = realm_general2)) + geom_point() +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)") 

pop_difs %>% 
	ggplot(aes(x = distance_km, y = temp_difference,col = realm_general2)) + geom_point() +
	facet_wrap(~ realm_general2, scales= "free") +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)")  + scale_x_log10()
ggsave("figures/differences-temps-distances.png", width = 12, height = 4)

## plot by elevation difference 
pop_difs %>% 
	filter(!is.na(elev_1) & !is.na(elev_2)) %>%
	ggplot(aes(x = abs(elev_2-elev_1), y = temp_difference,col = realm_general2)) + geom_point() +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance in elevation between populations (m)") 





#
###
#######
##############
##########################
####################################
###############################################
# FUNCTION TO INITILIZE THE DISTANCE DATAFRAME #
###############################################
## no input
## returns a dataframe 
initialize_distance_data <- function() {
	data <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")
	
	## subset to data that has both latitude and longitude 
	data <- data %>%
		filter(!is.na(latitude)) %>%
		filter(!is.na(longitude))
	
	## split into chunks of each species
	species_list <- split(data, data$genus_species)
	
	all_combinations <- data.frame(matrix(ncol = 4))
	colnames(all_combinations) <- c("lat_lon_1", "lat_lon_2", "distance", "genus_species")
	
	i = 1
	while(i < length(species_list) + 1) {
		species <- as.data.frame(species_list[i])
		colnames(species) <- colnames(data)
		
		## make new column lat_lon to use to find unique lat lon pairs
		unique_pops <- species %>%
			mutate(lat_lon = paste(latitude, longitude, sep = "_")) %>%
			filter(!duplicated(lat_lon)) %>%
			select(genus_species, latitude, longitude, lat_lon)
		
		if(nrow(unique_pops) > 1) {
			## find all unique combinations of lat_lon
			combinations <- combn(unique_pops$lat_lon, m=2, simplify = TRUE,)
			
			## split lat_lon string of the unique combinations so distance between the coordinates can be calculated
			## cooridnates must be in format c(longitude, latitude) so reverse column order
			split1 <- str_split_fixed(combinations[1,], n=3, pattern = "_")
			first_coord <- cbind(as.numeric(split1[,2]), as.numeric(split1[,1]))
			split2 <- str_split_fixed(combinations[2,], n=3, pattern = "_")
			second_coord <- cbind(as.numeric(split2[,2]), as.numeric(split2[,1]))
			
			## calculate distance between each unique lat lon pair 
			distance <- distHaversine(first_coord, second_coord)
			
			all <- as.data.frame(cbind(combinations[1,], combinations[2,], distance))
			all$genus_species <- unique_pops$genus_species[1]
			colnames(all)[1:2]<- c("lat_lon_1", "lat_lon_2")
			
			all_combinations <- rbind(all_combinations, all)
		}
		
		i = i + 1
		
	}
	
	all_combinations <- all_combinations[-1,]
	
	return (all_combinations)
}

#
###
#######
##############
##########################
####################################################
############################################################################
# FUNCTION TO GET MEAN YEARLY MAX TEMPS FROM TEMP DATA FOR ALL REALMS ######
############################################################################
## corrects for elevation for terrestrial species 
## returns intratherm with all unique populations and with column mean_yearly_max_temp
get_mean_yearly_max <- function() {
	intratherm <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")
	intratherm <- intratherm %>%
		filter(!is.na(latitude)) %>%
		filter(!is.na(longitude))
	
	
	##### Terrestrial species: -----------------------------------------------------
	terr_temps <- read_csv("~/Documents/too-big-for-github/intratherm-terrestrial-temps-tavg.csv") 
	
	terrestrial <- intratherm %>%
		filter(realm_general2 == "Terrestrial") %>%
		mutate(population_id = paste(population_id, longitude, sep = "_"))
	
	terrestrial <- terrestrial[!duplicated(terrestrial[,c("genus_species", "latitude", "longitude", "elevation_of_collection")]),]
	
	
	elev <- read.csv("./data-processed/intratherm-with-elev.csv")
	
	elev <- elev %>% 
		filter(realm_general2 == "Terrestrial") %>%
		mutate(population_id = paste(population_id, longitude, sep = "_"))
	
	elev <- elev[!duplicated(elev[,c("genus_species", "latitude", "longitude", "elevation_of_collection")]),]
	
	intra_long <- terr_temps %>% 
		mutate(date = as.character(date)) %>%
		separate(date, sep = 4, into = c("year", "decimal_year"), remove = FALSE) %>% 
		gather(key = population_id, value = temperature, 4:296) %>% 
		group_by(population_id, year) %>% 
		summarise(max_temp = max(temperature)) %>%
		ungroup() %>% 
		group_by(population_id) %>% 
		summarise(mean_yearly_max_temp = mean(max_temp))
	
	terrestrial <- left_join(terrestrial, intra_long) %>%
		select(-population_id, -elevation_of_collection)
	
	terrestrial <- left_join(terrestrial, elev) 
	
	## elevation correction:
	terrestrial <- terrestrial %>%
		mutate(elevation_of_collection = as.numeric(elevation_of_collection)) %>% 
		mutate(raster_mean = as.numeric(raster_mean)) %>% 
		mutate(mean_yearly_max_temp = mean_yearly_max_temp
			   + 5.5*((raster_mean - elevation_of_collection)/1000)) 
	
	terrestrial <- terrestrial %>%
		select(-raster_mean)
	
	
	##### Freshwater species: -----------------------------------------------------
	fresh_temps <- read_csv("./data-processed/intratherm-freshwater-temp-data-daily.csv")
	
	freshwater <- intratherm %>%
		filter(realm_general2 == "Freshwater") %>%
		mutate(population_id = paste(population_id, longitude, sep = "_"))
	
	freshwater <- freshwater[!duplicated(freshwater[,c("genus_species", "latitude", "longitude")]),]
	
	intra_long <- fresh_temps %>% 
		mutate(date = as.character(date)) %>%
		separate(date, sep = 4, into = c("year", "decimal_year"), remove = FALSE) %>% 
		gather(key = population_id, value = temperature, 4:385) %>% 
		group_by(population_id, year) %>% 
		summarise(max_temp = max(temperature)) %>%
		ungroup() %>% 
		group_by(population_id) %>% 
		summarise(mean_yearly_max_temp = mean(max_temp))
	
	freshwater <- left_join(freshwater, intra_long) 
	
	
	##### Marine species: -----------------------------------------------------
	marine_temps <- read_csv("./data-processed/intratherm-marine-temp-data.csv")
	
	marine <- intratherm %>%
		filter(realm_general2 == "Marine") %>%
		mutate(population_id = paste(population_id, longitude, sep = "_"))
	
	marine <- marine[!duplicated(marine[,c("genus_species", "latitude", "longitude")]),]
	
	intra_long <- marine_temps %>% 
		mutate(date = as.character(date)) %>%
		separate(date, sep = "-", into = c("year", "months", "days"), remove = FALSE) %>% 
		gather(key = population_id, value = temperature, 5:75) %>% 
		group_by(population_id, year) %>% 
		summarise(max_temp = max(temperature)) %>%
		ungroup() %>% 
		group_by(population_id) %>% 
		summarise(mean_yearly_max_temp = mean(max_temp))
	
	marine <- left_join(marine, intra_long) 
	
	
	#### combine all together: -----------------------------------------------------
	intratherm_maxtemp <- rbind(freshwater, marine, terrestrial)
	
	return(intratherm_maxtemp)
}

#
###
#######
##############
##########################
####################################
#####################################################################
# FUNCTION TO INITILIZE THE DISTANCE AND TEMP DIFFERENCE DATAFRAME #
#####################################################################
## no input
## returns a dataframe containing pairwise differences and temp differences between all populations of each species  
initialize_pairwise_differences <- function() {
	data = get_mean_yearly_max()
	
	## split into chunks of each species
	species_list <- split(data, data$genus_species)
	
	## prepare empty data frame to add pairwise differences to
	all_combinations <- data.frame(matrix(ncol = 9))
	colnames(all_combinations) <- c("genus_species", "lat_lon_1", "lat_lon_2", 
									"elev_1", "elev_2", "mean_yearly_max_temp1", 
									"mean_yearly_max_temp2","distance", "temp_difference")
	
	i = 1
	## go through all groups of species, calculating pairwise differences and adding them to the data frame 
	while(i < length(species_list) + 1) {
		
		## get next group of speices and add lat_lon column:
		species <- as.data.frame(species_list[i])
		colnames(species) <- colnames(data)
		unique_pops <- species %>%
			mutate(lat_lon = paste(latitude, longitude, sep = "_")) %>%
			filter(!duplicated(lat_lon)) %>%
			select(genus_species, latitude, longitude, elevation_of_collection, 
				   lat_lon, mean_yearly_max_temp) %>%
			mutate(lat_lon = as.character(lat_lon))
		
		## if the species has more than one population
		if(nrow(unique_pops) > 1) {
			
			## find all unique combinations of lat_lon
			combinations <- combn(unique_pops$lat_lon,m=2, simplify = TRUE,)
		
			## split lat_lon string so distance between the coordinates can be calculated
			## cooridnates must be in format c(longitude, latitude) so reverse column order
			split1 <- str_split_fixed(combinations[1,], n=3, pattern = "_")
			first_coord <- cbind(as.numeric(split1[,2]), as.numeric(split1[,1]))
			split2 <- str_split_fixed(combinations[2,], n=3, pattern = "_")
			second_coord <- cbind(as.numeric(split2[,2]), as.numeric(split2[,1]))
			
			## calculate distance between each unique coordinate pair 
			distance <- distHaversine(first_coord, second_coord)
			### calculate CTmax difference
			# distance_ctmax <- abs(ctmax20_1 - ctmax20_2)
			
			## combine all info in a dataframe 
			df1 <- data.frame(lat_lon = combinations[1,]) %>%
				mutate(lat_lon = as.character(lat_lon)) %>%
				left_join(., unique_pops, by = "lat_lon") %>%
				select(-latitude, -longitude) %>%
				rename(lat_lon_1 = lat_lon, elev_1 = elevation_of_collection,
					   mean_yearly_max_temp1 = mean_yearly_max_temp)
				
			df2 <- data.frame(lat_lon = combinations[2,]) %>%
				mutate(lat_lon = as.character(lat_lon), by = "lat_lon") %>%
				left_join(., unique_pops) %>%
				select(-genus_species, -latitude,-longitude) %>%
				rename(lat_lon_2 = lat_lon, elev_2 = elevation_of_collection,
					   mean_yearly_max_temp2 = mean_yearly_max_temp)
			
			df <- cbind(df1, df2) %>%
				mutate(distance = distance)
			
			df <- df  %>%
				mutate(temp_difference = abs(mean_yearly_max_temp1 - mean_yearly_max_temp2)) %>%
				select(genus_species, lat_lon_1, lat_lon_2, elev_1, elev_2, mean_yearly_max_temp1,
					   mean_yearly_max_temp2, distance, temp_difference)
			
			all_combinations <- rbind(all_combinations, df)
		}
		
		i = i + 1
		
	}
	
	all_combinations <- all_combinations[-1,]
	
	## now must calculate temp differences for terrestrial species at the same latitude and longitude but with different elevations 
	
	## subset to terrestrial species and break into species chunks 
	data <- data %>%
		filter(realm_general2 == "Terrestrial")
	data <- droplevels(data)
	terr_list <- split(data, data$genus_species)
	
	i = 1
	## go through all terrestrial species, check if any have the same lat_lon but different elevations 
	while(i < length(terr_list) + 1) {
	
		## get next group of speices and add lat_lon column:
		species <- as.data.frame(terr_list[i])
		colnames(species) <- colnames(data)
		species <- species %>%
			mutate(lat_lon = paste(latitude, longitude, sep = "_")) 
		
		## figure out if any have the same lat_lon and different elevation 
		is_duplicated <- species$lat_lon[which(duplicated(species$lat_lon))]
		same_coords <- species %>%
			filter(lat_lon %in% is_duplicated) 
		
		## if they do
		if(nrow(same_coords) > 2) {
			## split into chunks of the duplicate lat_lons 
			dups <- split(same_coords, same_coords$lat_lon)
			z = 1
			while (z < length(dups) + 1) {
				## go through each chunk
				dup <- as.data.frame(dups[z]) 
				names <- append(colnames(data), "lat_lon")
				colnames(dup) <- names
				dup <- dup %>% 
					select(genus_species, latitude, longitude, elevation_of_collection, 
						   lat_lon, mean_yearly_max_temp) 
				
				## find pairwise combinations of elevations, calculate temp differences and add them to the data frame
				combinations <- combn(dup$elevation_of_collection,m=2, simplify = TRUE,)
				
				df1 <- data.frame(elevation_of_collection = combinations[1,])%>%
					mutate(elevation_of_collection = elevation_of_collection) %>%
					left_join(., dup, by = "elevation_of_collection") %>%
					select(-latitude, -longitude) %>%
					rename(lat_lon_1 = lat_lon, elev_1 = elevation_of_collection,
						   mean_yearly_max_temp1 = mean_yearly_max_temp)
				
				df2 <- data.frame(elevation_of_collection = combinations[2,])%>%
					mutate(elevation_of_collection = elevation_of_collection) %>%
					left_join(., dup, by = "elevation_of_collection") %>%
					select(-latitude, -longitude, -genus_species) %>%
					rename(lat_lon_2 = lat_lon, elev_2 = elevation_of_collection,
						   mean_yearly_max_temp2 = mean_yearly_max_temp)
				
				df <- cbind(df1, df2) 
				df$distance = 0 ##since no lat_lon difference, assign distance between = 0
				
				df <- df  %>%
					mutate(temp_difference = abs(mean_yearly_max_temp1 - mean_yearly_max_temp2)) %>%
					select(genus_species, lat_lon_1, lat_lon_2, elev_1, elev_2, mean_yearly_max_temp1,
						   mean_yearly_max_temp2, distance, temp_difference)
				
				all_combinations <- rbind(all_combinations, df)
				z = z + 1
			}
		}
			
		i = i + 1
	}
	return (all_combinations)
}

#
###
#######
##############
##########################
####################################################
############################################################################
# FUNCTION TO PICK OUT TWO POPULATIONDS FROM THE LIST OF PAIRWISE DISTANCES
############################################################################
## takes two vectors of the format c(genus_species, latitude, longitude) representing the two populations to calculate the distance between and the distance dataframe 
distance_between <- function(population1, population2, all_combinations) {
	
	lat_lon1 = paste(population1[2], population1[3], sep = "_")
	lat_lon2 = paste(population2[2], population2[3], sep = "_")
	
	match = which(population1[1] == all_combinations$genus_species & population2[1] == all_combinations$genus_species & lat_lon1 == all_combinations$lat_lon_1 & lat_lon2 == all_combinations$lat_lon_2)
	
	if (length(match) == 0) {
		match = which(population1[1] == all_combinations$genus_species & population2[1] == all_combinations$genus_species & lat_lon2 == all_combinations$lat_lon_1 & lat_lon1 == all_combinations$lat_lon_2)
	}
	
	if (length(match) == 0) {
		print("Error: no match found for one of the populations.") 
		if (!population1[1] %in% all_combinations$genus_species) {
			print("There seems to be a problem with the second population's genus_species.")
		}
		if (!population2[1] %in% all_combinations$genus_species) {
			print("There seems to be a problem with the first population's genus_species.")
		}
		if (!lat_lon1 %in% all_combinations$lat_lon_1) {
			print("There seems to be a problem with the second population's coordinates.")
		}
		if (!lat_lon2 %in% all_combinations$lat_lon_2) {
			print("There seems to be a problem with the first population's coordinates.")
		}
		return()
	}
	else {
		dist <- as.numeric(as.character(all_combinations$distance[match]))
		
		return(dist)
	}
}


#
###
#######
##############
##########################
####################################################
############################################################################
# FUNCTION TO GET PAIRWISE DISTANCES OF SPECIES ############################
############################################################################
## takes a genus_species and the distance dataframe and returns all pairwise combinations of populations of that species 
populations_of_species <- function(species, all_combinations) {
	
	match = which(species == all_combinations$genus_species)
	
	if (length(match) == 0) {
		print("Error: no species match found.") 
		return()
	}
	else {
		output <- all_combinations[match,]
		
		return(output)
	}
}


