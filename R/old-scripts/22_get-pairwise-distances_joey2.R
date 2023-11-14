## calculating pairwise population distances 
library(tidyverse)
library(utils)
library(geosphere)

## calcualte differences in distance and temperature between all populations of each species 
pop_difs <- initialize_pairwise_differences_nichemapr()

## convert distance between to km 
pop_difs <- pop_difs %>%
	mutate(distance_km = distance/1000)

## add realm column 
realm <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv") %>%
	select(genus_species, realm_general2) %>%
	filter(!duplicated(genus_species))
pop_difs <- inner_join(realm, pop_difs, by = "genus_species")

##plot all realms 
pop_difs %>% 
	ggplot(aes(x = distance_km, y = temp_difference_topt)) + geom_point() +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)") 

pop_difs %>% 
	ggplot(aes(x = distance_km, y = experienced_temp_difference)) + geom_point() +
	ylab('Difference in experienced yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)") 

pop_difs %>% 
	ggplot(aes(x = temp_difference, y = experienced_temp_difference)) + geom_point() +
	ylab('Difference in experienced yearly monthly max temperatures (°C)') +
	xlab("Difference in yearly monthly max temperatures (°C)") 

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
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)") 

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
	
	## subset to data that has both latitude and longitude and split into chunks of each species
	species_list <- data %>%
		filter(!is.na(latitude)) %>%
		filter(!is.na(longitude)) %>%
		split(., data$genus_species)
	
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
	intratherm <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv") %>%
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
		select(-population_id, -elevation_of_collection) %>% 
		left_join(., elev) %>%
		mutate(elevation_of_collection = as.numeric(elevation_of_collection)) %>%  ## elevation correction:
		mutate(raster_mean = as.numeric(raster_mean)) %>% 
		mutate(mean_yearly_max_temp = mean_yearly_max_temp
			   + 5.5*((raster_mean - elevation_of_collection)/1000)) %>%
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
	terr_list <- data %>%
		filter(realm_general2 == "Terrestrial") %>%
		droplevels() %>%
	 	split(., data$genus_species)
	
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
####################################
#####################################################################
# FUNCTION TO INITILIZE THE DISTANCE AND TEMP DIFFERENCE DATAFRAME #
#####################################################################
## no input
## returns a dataframe containing pairwise differences in NicheMapR temperatures and distances for terrestrial species
initialize_pairwise_differences_nichemapr <- function() {
	topt <- read.delim("./data-processed/OperativeTemperatures.csv", sep = " ") %>%
		rename(intratherm_id = ID)
	intratherm <- read.csv("./data-processed/intratherm-with-elev.csv") %>% 
		select(intratherm_id, genus_species, population_id, class,
			   is.nocturnal, latitude, longitude, elevation_of_collection, realm_general2) %>%
		filter(realm_general2 == "Terrestrial") %>%
		droplevels()

	## make column of which topt temperature will be used: 
	## based on is_nocturnal
	## if is_nocturnal, use burrow t_opt temp 
	topt <- left_join(topt, intratherm) %>%
		mutate(t_opt = ifelse(is.nocturnal == "Y", TeBurrQ75, ## topt in burrow 
													 TeSunQ75)) ## topt in exposed area 
	
	## split into chunks of each species
	species_list <- split(topt, topt$genus_species)
	
	## prepare empty data frame to add pairwise differences to
	all_combinations <- data.frame(matrix(ncol = 9))
	colnames(all_combinations) <- c("genus_species", "lat_lon_1", "lat_lon_2", 
									"elev_1", "elev_2", "t_opt1", 
									"t_opt2","distance", "temp_difference_topt")
	
	i = 1
	## go through all groups of species, calculating pairwise differences and adding them to the data frame
	while(i < length(species_list) + 1) {
		
		## get next group of speices and add lat_lon column:
		species <- data.frame(species_list[i])
		colnames(species) <- colnames(topt)
		unique_pops <- species %>%
			mutate(lat_lon = paste(latitude, longitude, sep = "_")) %>%
			filter(!duplicated(lat_lon)) %>%
			select(genus_species, latitude, longitude, elevation_of_collection, 
				   lat_lon, t_opt)%>%
			mutate(lat_lon = as.character(lat_lon))
		
		## if the species has more than one populatopt
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
			
			## combine all info in a dataframe 
			df1 <- data.frame(lat_lon = combinations[1,]) %>%
				mutate(lat_lon = as.character(lat_lon)) %>%
				left_join(., unique_pops, by = "lat_lon") %>%
				select(-latitude, -longitude) %>%
				rename(lat_lon_1 = lat_lon, elev_1 = elevation_of_collection,
					   t_opt1 = t_opt)
			
			df2 <- data.frame(lat_lon = combinations[2,]) %>%
				mutate(lat_lon = as.character(lat_lon), by = "lat_lon") %>%
				left_join(., unique_pops) %>%
				select(-genus_species, -latitude,-longitude) %>%
				rename(lat_lon_2 = lat_lon, elev_2 = elevation_of_collection,
					   t_opt2 = t_opt)
			
			df <- cbind(df1, df2) %>%
				mutate(distance = distance)  %>%
				mutate(temp_difference_topt = abs(t_opt1 - t_opt2)) %>%
				select(genus_species, lat_lon_1, lat_lon_2, elev_1, elev_2, t_opt1,
					   t_opt2, distance, temp_difference_topt)
			
			all_combinations <- rbind(all_combinations, df)
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


#
###
#######
##############
##########################
####################################################
########################################################################################
# FUNCTION TO GET EXPERIENCED MEAN YEARLY MAX TEMPS FROM TEMP DATA FOR ALL REALMS ######
########################################################################################
## corrects for elevation for terrestrial species 
## returns intratherm with all unique populations and with column experienced_mean_yearly_max_temp
## "experienced" means mean_yearly_max_temp after temperatures in seasons when that species is away or inactive have been removed 
get_experienced_mean_yearly_max <- function() {

	intratherm <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv") %>%
		filter(!is.na(latitude)) %>%
		filter(!is.na(longitude))
	
	
	##### Terrestrial species: -----------------------------------------------------
	terr_temps <- read_csv("~/Documents/SUNDAY LAB/Intratherm/Data sheets/intratherm-terrestrial-temps-tavg.csv") 
	terr_experienced_temps <- set_temps_to_NA(terr_temps, realm = "Terrestrial")
	
	terrestrial <- intratherm %>%
		filter(realm_general2 == "Terrestrial") %>%
		mutate(population_id = paste(population_id, longitude, sep = "_"))
	
	terrestrial <- terrestrial[!duplicated(terrestrial[,c("genus_species", "latitude", "longitude", "elevation_of_collection")]),]
	
	
	elev <- read.csv("./data-processed/intratherm-with-elev.csv") %>% 
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
	
	intra_long_exp <- terr_experienced_temps %>% 
		mutate(date = as.character(date)) %>%
		separate(date, sep = 4, into = c("year", "decimal_year"), remove = FALSE) %>% 
		gather(key = population_id, value = temperature, 4:296) %>% 
		group_by(population_id, year) %>% 
		summarise(experienced_max_temp = max(temperature, na.rm = TRUE)) %>%
		ungroup() %>% 
		group_by(population_id) %>% 
		summarise(experienced_mean_yearly_max_temp = mean(experienced_max_temp, na.rm = TRUE)) %>%
		mutate(experienced_mean_yearly_max_temp = ifelse(is.infinite(experienced_mean_yearly_max_temp),
														 NA, experienced_mean_yearly_max_temp))
	
	terrestrial <- left_join(terrestrial, intra_long) %>%
		left_join(., intra_long_exp) %>%
		select(-population_id, -elevation_of_collection) %>%
		left_join(., elev) 
	
	## elevation correction:
	terrestrial <- terrestrial %>%
		mutate(elevation_of_collection = as.numeric(elevation_of_collection)) %>% 
		mutate(raster_mean = as.numeric(raster_mean)) %>% 
		mutate(experienced_mean_yearly_max_temp = experienced_mean_yearly_max_temp
			   + 5.5*((raster_mean - elevation_of_collection)/1000)) %>%
		mutate(mean_yearly_max_temp = mean_yearly_max_temp
			   + 5.5*((raster_mean - elevation_of_collection)/1000)) %>%
		select(-raster_mean)
	
	
	##### Freshwater species: -----------------------------------------------------
	fresh_temps <- read_csv("./data-processed/intratherm-freshwater-temp-data-daily.csv")
	
	fresh_experienced_temps <- set_temps_to_NA(fresh_temps, realm = "Freshwater")
	
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
	
	intra_long_exp <- fresh_experienced_temps %>% 
		mutate(date = as.character(date)) %>%
		separate(date, sep = 4, into = c("year", "decimal_year"), remove = FALSE) %>% 
		gather(key = population_id, value = temperature, 4:385) %>% 
		group_by(population_id, year) %>% 
		summarise(experienced_max_temp = max(temperature, na.rm = TRUE)) %>%
		ungroup() %>% 
		group_by(population_id) %>% 
		summarise(experienced_mean_yearly_max_temp = mean(experienced_max_temp, na.rm = TRUE))  %>%
		mutate(experienced_mean_yearly_max_temp = ifelse(is.infinite(experienced_mean_yearly_max_temp),
														 NA, experienced_mean_yearly_max_temp))

	freshwater <- left_join(freshwater, intra_long) %>%
		left_join(., intra_long_exp)
	
	
	##### Marine species: -----------------------------------------------------
	marine_temps <- read_csv("./data-processed/intratherm-marine-temp-data.csv")
	
	## cut off data from 1981 so begins at 1982: 
	marine_temps <- marine_temps[-1:-91,]
	
	marine_experienced_temps <- set_temps_to_NA(marine_temps, realm = "Terrestrial")
	
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
	
	intra_long_exp <- marine_experienced_temps %>% 
		mutate(date = as.character(date)) %>%
		separate(date, sep = "-", into = c("year", "months", "days"), remove = FALSE) %>% 
		gather(key = population_id, value = temperature, 5:75) %>% 
		group_by(population_id, year) %>% 
		summarise(experienced_max_temp = max(temperature, na.rm = TRUE)) %>%
		ungroup() %>% 
		group_by(population_id) %>% 
		summarise(experienced_mean_yearly_max_temp = mean(experienced_max_temp, na.rm = TRUE))  %>%
		mutate(experienced_mean_yearly_max_temp = ifelse(is.infinite(experienced_mean_yearly_max_temp),
														 NA, experienced_mean_yearly_max_temp))
	
	marine <- left_join(marine, intra_long) %>%
		left_join(., intra_long_exp)
	
	
	#### combine all together: -----------------------------------------------------
	intratherm_maxtemp <- rbind(freshwater, marine, terrestrial)
	
	return(intratherm_maxtemp)
}

initialize_pairwise_differences_experienced <- function() {
	data = get_experienced_mean_yearly_max()
	
	## split into chunks of each species
	species_list <- split(data, data$genus_species)
	
	## prepare empty data frame to add pairwise differences to
	all_combinations <- data.frame(matrix(ncol = 12))
	colnames(all_combinations) <- c("genus_species", "lat_lon_1", "lat_lon_2", 
									"elev_1", "elev_2", "mean_yearly_max_temp1", 
									"mean_yearly_max_temp2","experienced_mean_yearly_max_temp1", "experienced_mean_yearly_max_temp2", "distance", "temp_difference", "experienced_temp_difference")
	
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
				   lat_lon, mean_yearly_max_temp, experienced_mean_yearly_max_temp) %>%
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
			
			## combine all info in a dataframe 
			df1 <- data.frame(lat_lon = combinations[1,]) %>%
				mutate(lat_lon = as.character(lat_lon)) %>%
				left_join(., unique_pops, by = "lat_lon") %>%
				select(-latitude, -longitude) %>%
				rename(lat_lon_1 = lat_lon, elev_1 = elevation_of_collection,
					   mean_yearly_max_temp1 = mean_yearly_max_temp, 
					   experienced_mean_yearly_max_temp1 = experienced_mean_yearly_max_temp)
			
			df2 <- data.frame(lat_lon = combinations[2,]) %>%
				mutate(lat_lon = as.character(lat_lon), by = "lat_lon") %>%
				left_join(., unique_pops) %>%
				select(-genus_species, -latitude,-longitude) %>%
				rename(lat_lon_2 = lat_lon, elev_2 = elevation_of_collection,
					   mean_yearly_max_temp2 = mean_yearly_max_temp, 
					   experienced_mean_yearly_max_temp2 = experienced_mean_yearly_max_temp)
			
			df <- cbind(df1, df2) %>%
				mutate(distance = distance)
			
			df <- df  %>%
				mutate(temp_difference = abs(mean_yearly_max_temp1 - mean_yearly_max_temp2)) %>%
				mutate(experienced_temp_difference = abs(experienced_mean_yearly_max_temp1 - experienced_mean_yearly_max_temp2)) %>%
				select(genus_species, lat_lon_1, lat_lon_2, elev_1, elev_2, mean_yearly_max_temp1,
					   mean_yearly_max_temp2, experienced_mean_yearly_max_temp1, 
					   experienced_mean_yearly_max_temp2, distance, temp_difference, experienced_temp_difference)
			
			all_combinations <- rbind(all_combinations, df)
		}
		
		i = i + 1
		
	}
	
	all_combinations <- all_combinations[-1,]
	
	## now must calculate temp differences for terrestrial species at the same latitude and longitude but with different elevations 
	
	## subset to terrestrial species and break into species chunks 
	terr <- data %>%
		filter(realm_general2 == "Terrestrial") %>%
		droplevels()
	
	terr_list <- split(terr, terr$genus_species)
	
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
				names <- append(colnames(terr), "lat_lon")
				colnames(dup) <- names
				dup <- dup %>% 
					select(genus_species, latitude, longitude, elevation_of_collection, 
						   lat_lon, mean_yearly_max_temp, experienced_mean_yearly_max_temp) 
				
				## find pairwise combinations of elevations, calculate temp differences and add them to the data frame
				combinations <- combn(dup$elevation_of_collection,m=2, simplify = TRUE,)
				
				df1 <- data.frame(elevation_of_collection = combinations[1,])%>%
					mutate(elevation_of_collection = elevation_of_collection) %>%
					left_join(., dup, by = "elevation_of_collection") %>%
					select(-latitude, -longitude) %>%
					rename(lat_lon_1 = lat_lon, elev_1 = elevation_of_collection,
						   mean_yearly_max_temp1 = mean_yearly_max_temp, 
						   experienced_mean_yearly_max_temp1 = experienced_mean_yearly_max_temp)
				
				df2 <- data.frame(elevation_of_collection = combinations[2,])%>%
					mutate(elevation_of_collection = elevation_of_collection) %>%
					left_join(., dup, by = "elevation_of_collection") %>%
					select(-latitude, -longitude, -genus_species) %>%
					rename(lat_lon_2 = lat_lon, elev_2 = elevation_of_collection,
						   mean_yearly_max_temp2 = mean_yearly_max_temp, 
						   experienced_mean_yearly_max_temp2 = experienced_mean_yearly_max_temp)
				
				df <- cbind(df1, df2) 
				df$distance = 0 ##since no lat_lon difference, assign distance between = 0
				
				df <- df  %>%
					mutate(temp_difference = abs(mean_yearly_max_temp1 - mean_yearly_max_temp2)) %>%
					mutate(experienced_temp_difference = abs(experienced_mean_yearly_max_temp1 - experienced_mean_yearly_max_temp2)) %>%
					select(genus_species, lat_lon_1, lat_lon_2, elev_1, elev_2, mean_yearly_max_temp1,
						   mean_yearly_max_temp2, experienced_mean_yearly_max_temp1, 
						   experienced_mean_yearly_max_temp2, distance, temp_difference, experienced_temp_difference)
				
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
############################################
###########################################################################
# FUNCTION FOR SETTING TEMP DATA TO NA DURING SEASON WHEN AWAY/INACTIVE ###
###########################################################################
## takes a temp data frame with columns for each population's temp data and a realm argument specifying which dataset the temp data came from 
## column names must contain the population's genus_species name
## returns a new version of the temp_data with temps during seasons away and seasons inactive set to NA
set_temps_to_NA <- function(temp_data, realm) {
	
	intratherm <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")
	intratherm <- convert_seasons_to_numeric(intratherm)
	
	## get rid of date column
	new_temp_data <- data.frame(matrix(ncol = 1, nrow = nrow(temp_data)))
	new_temp_data[,1] <- temp_data[,1]
	colnames(new_temp_data) <- "date"
	temp_data <- temp_data[,-1]
	
	z = 1
	while (z < ncol(temp_data) + 1) {
		
		##figure out the species 
		column <- as.data.frame(temp_data[,z], nrow = 50000)
		species <- as.character(as.data.frame(str_split(colnames(temp_data)[z], pattern = "_"))[1,])
		
		species_rows <- intratherm[which(intratherm$genus_species == species),]
		
		swa_start <- species_rows$season_when_away_100km_start[1]
		swa_stop <- species_rows$season_when_away_100km_stop[1]
		sia_start <- species_rows$season_inactive_start[1]
		sia_stop <- species_rows$season_inactive_stop[1]
		sia_start2 <- species_rows$season_inactive_start2[1]
		sia_stop2 <- species_rows$season_inactive_stop2[1]
		
		## set all months to false by default 
		months <- c(jan <- FALSE, feb <- FALSE, mar <- FALSE, apr <- FALSE, may <- FALSE,
					jun <- FALSE, jul <- FALSE,	aug <- FALSE, sep <- FALSE, oct <- FALSE, 
					nov <- FALSE, dec <- FALSE)
		
		##set months when away/inactive to TRUE by calling initialize_months
		if (!swa_start == "") {
			away_start <- as.numeric(swa_start)
			away_stop <- as.numeric(swa_stop)
			
			months <- initialize_months(away_start, away_stop, months)
		}
		
		if (!sia_start == "") {
			away_start <- as.numeric(sia_start)
			away_stop <- as.numeric(sia_stop)
			
			months <- initialize_months(away_start, away_stop, months)
			if(!sia_start2 == "") {
				away_start <- as.numeric(sia_start2)
				away_stop <- as.numeric(sia_stop2)
				
				months <- initialize_months(away_start, away_stop, months)
			}
		}
		
		
		## set temps in column to NA 
		## different conditions for each realm since all have different start and stop dates for temp data
		if (realm == "Terrestrial") {
			max <- 2019
			rep <- 1930
			day_index <- 1 
		}
		else if (realm == "Marine") {
			## 1981-09-01 to 2020-04-04
			max = 2021
			rep = 1982
			day_index <- 1
		}
		else if (realm == "Freshwater") {
			max = 2002
			rep = 1958
			day_index <- 1
		}
		
		while (rep < max) {
			if (rep == 2019) {
				if (isTRUE(months[1])) {
					column[day_index:(day_index + 30),] <- NA
				}
				if (isTRUE(months[2])) {
					column[(day_index + 30):(day_index + 58),] <- NA
				}
				if (isTRUE(months[3])) {
					column[(day_index + 58):(day_index + 89),] <- NA
				}
				if (isTRUE(months[4])) {
					column[(day_index + 89):(day_index + 119),] <- NA
				}
				if (isTRUE(months[5])) {
					column[(day_index + 119):(day_index + 150),] <- NA
				}
				if (isTRUE(months[6])) {
					column[(day_index + 150):(day_index + 180),] <- NA
				}
				if (isTRUE(months[7])) {
					column[(day_index + 180):(day_index + 211),] <- NA
				}
				if (isTRUE(months[8])) {
					column[(day_index + 211):(day_index + 242),] <- NA
				}
				if (isTRUE(months[9])) {
					column[(day_index + 242):(day_index + 272),] <- NA
				}
				print(paste("Initialized ", rep, " months to NA - last year!", sep = ""))
				rep <- rep + 1
				day_index <- day_index + 365
			}
			else if (rep == 1900) {
				if (isTRUE(months[1])) {
					column[day_index:(day_index + 30),] <- NA
				}
				if (isTRUE(months[2])) {
					column[(day_index + 30):(day_index + 58),] <- NA
				}
				if (isTRUE(months[3])) {
					column[(day_index + 58):(day_index + 89),] <- NA
				}
				if (isTRUE(months[4])) {
					column[(day_index + 89):(day_index + 119),] <- NA
				}
				if (isTRUE(months[5])) {
					column[(day_index + 119):(day_index + 150),] <- NA
				}
				if (isTRUE(months[6])) {
					column[(day_index + 150):(day_index + 180),] <- NA
				}
				if (isTRUE(months[7])) {
					column[(day_index + 180):(day_index + 211),] <- NA
				}
				if (isTRUE(months[8])) {
					column[(day_index + 211):(day_index + 242),] <- NA
				}
				if (isTRUE(months[9])) {
					column[(day_index + 242):(day_index + 272),] <- NA
				}
				if (isTRUE(months[10])) {
					column[(day_index + 272):(day_index + 303),] <- NA
				}
				if (isTRUE(months[11])) {
					column[(day_index + 303):(day_index + 333),] <- NA
				}
				if (isTRUE(months[12])) {
					column[(day_index + 333):(day_index + 364),] <- NA
				}
				print(paste("Initialized ", rep, " months to NA (not a leap year)", sep = ""))
				rep <- rep + 1
				day_index <- day_index + 365
			}
			else if (rep %% 4 == 0) {
				if (isTRUE(months[1])) {
					column[day_index:(day_index + 30),] <- NA
				}
				if (isTRUE(months[2])) {
					column[(day_index + 30):(day_index + 59),] <- NA
				}
				if (isTRUE(months[3])) {
					column[(day_index + 59):(day_index + 90),] <- NA
				}
				if (isTRUE(months[4])) {
					column[(day_index + 90):(day_index + 120),] <- NA
				}
				if (isTRUE(months[5])) {
					column[(day_index + 120):(day_index + 151),] <- NA
				}
				if (isTRUE(months[6])) {
					column[(day_index + 151):(day_index + 181),] <- NA
				}
				if (isTRUE(months[7])) {
					column[(day_index + 181):(day_index + 212),] <- NA
				}
				if (isTRUE(months[8])) {
					column[(day_index + 212):(day_index + 243),] <- NA
				}
				if (isTRUE(months[9])) {
					column[(day_index + 243):(day_index + 273),] <- NA
				}
				if (isTRUE(months[10])) {
					column[(day_index + 273):(day_index + 304),] <- NA
				}
				if (isTRUE(months[11])) {
					column[(day_index + 304):(day_index + 334),] <- NA
				}
				if (isTRUE(months[12])) {
					column[(day_index + 334):(day_index + 365),] <- NA
				}
				print(paste("Initialized ", rep, " months to NA (leap year)", sep = ""))
				rep <- rep + 1
				day_index <- day_index + 366
			}
			else {
				if (isTRUE(months[1])) {
					column[day_index:(day_index + 30),] <- NA
				}
				if (isTRUE(months[2])) {
					column[(day_index + 30):(day_index + 58),] <- NA
				}
				if (isTRUE(months[3])) {
					column[(day_index + 58):(day_index + 89),] <- NA
				}
				if (isTRUE(months[4])) {
					column[(day_index + 89):(day_index + 119),] <- NA
				}
				if (isTRUE(months[5])) {
					column[(day_index + 119):(day_index + 150),] <- NA
				}
				if (isTRUE(months[6])) {
					column[(day_index + 150):(day_index + 180),] <- NA
				}
				if (isTRUE(months[7])) {
					column[(day_index + 180):(day_index + 211),] <- NA
				}
				if (isTRUE(months[8])) {
					column[(day_index + 211):(day_index + 242),] <- NA
				}
				if (isTRUE(months[9])) {
					column[(day_index + 242):(day_index + 272),] <- NA
				}
				if (isTRUE(months[10])) {
					column[(day_index + 272):(day_index + 303),] <- NA
				}
				if (isTRUE(months[11])) {
					column[(day_index + 303):(day_index + 333),] <- NA
				}
				if (isTRUE(months[12])) {
					column[(day_index + 333):(day_index + 364),] <- NA
				}
				print(paste("Initialized ", rep, " months to NA (not a leap year)", sep = ""))
				rep <- rep + 1
				day_index <- day_index + 365
			}
		}
		
		## make sure it is the right length:
		if(!nrow(column) == nrow(new_temp_data)) {
			column = column[-(nrow(new_temp_data)+1):-nrow(column),]
		}
		
		new_temp_data = cbind(new_temp_data, column)
		
		z = z + 1	
	}
	colnames(new_temp_data)[2:length(new_temp_data)] <- colnames(temp_data)
	return(new_temp_data)
	
}


## helper functions for set_temps_to_NA:
## ---------------------------------------------------------------

convert_seasons_to_numeric <- function (data) {
	
	data <- data %>%
		filter(!is.na(latitude)) %>%
		filter(!is.na(longitude))
	
	## if in southern hemisphere, mark:
	data <- data %>%
		mutate(is_in_south = ifelse(latitude < 0, "Y", "N"))
	
	
	## convert season_inactive and season_when_away_100km to start and end date numbers
	swa <- data$season_when_away_100km
	
	if (length(swa) < 1) {
		print("It seems like something is wrong with the column 'season_when_away_100km'")
		return()
	}
	
	split <- str_split_fixed(swa, pattern = "-", n = 2)
	split <- as.data.frame(split)
	split$V1 <- as.character(split$V1)
	split$V2 <- as.character(split$V2)
	split$is_in_south <- data$is_in_south
	
	
	## if is in south is true, flip the seasons:
	i = 1
	while (i < length(split$V1) + 1) {
		if(split$is_in_south[i] == 'Y') {
			if(split$V1[i] == "Summer") {
				split$V1[i] = "Winter"
			}
			else if(split$V1[i] == "Spring") {
				split$V1[i] = "Fall"
			}
			else if(split$V1[i] == "Winter") {
				split$V1[i] = "Summer"
			}
			else if(split$V1[i] == "Fall") {
				split$V1[i] = "Spring"
			}
			if (split$V2[i] == "Summer") {
				split$V2[i] = "Winter"
			}
			else if (split$V2[i] == "Spring") {
				split$V2[i] = "Fall"
			}
			else if (split$V2[i] == "Winter") {
				split$V2[i] = "Summer"
			}
			else if (split$V2[i] == "Fall") {
				split$V2[i] = "Spring"
			}
		}
		i = i + 1
	}
	
	split <- split %>% ## if only one month/season, put value in both columns
		mutate(V2 = if_else(V2=="", V1 , V2))
	
	season_when_away_100km_start <- split$V1 %>%
		str_replace(pattern = "Winter", replacement = "0.875") %>%
		str_replace(pattern = "Spring", replacement = "0.208") %>%
		str_replace(pattern = "Summer", replacement = "0.458") %>%
		str_replace(pattern = "Fall", replacement = "0.708") %>%
		str_replace(pattern = "Jan", replacement = "0.042") %>%
		str_replace(pattern = "Feb", replacement = "0.125") %>%
		str_replace(pattern = "Mar", replacement = "0.208") %>%
		str_replace(pattern = "Apr", replacement = "0.292") %>%
		str_replace(pattern = "May", replacement = "0.375") %>%
		str_replace(pattern = "Jun", replacement = "0.458") %>%
		str_replace(pattern = "Jul", replacement = "0.542") %>%
		str_replace(pattern = "Aug", replacement = "0.625") %>%
		str_replace(pattern = "Sep", replacement = "0.708") %>%
		str_replace(pattern = "Oct", replacement = "0.792") %>%
		str_replace(pattern = "Nov", replacement = "0.875") %>%
		str_replace(pattern = "Dec", replacement = "0.958") %>%
		str_replace(pattern = "kunk", replacement = "") %>%
		str_replace(pattern = "unk", replacement = "")
	
	season_when_away_100km_stop <- split$V2 %>%
		str_replace(pattern = "Winter", replacement = "0.125") %>%
		str_replace(pattern = "Spring", replacement = "0.375") %>%
		str_replace(pattern = "Summer", replacement = "0.625") %>%
		str_replace(pattern = "Fall", replacement = "0.792") %>%
		str_replace(pattern = "Jan", replacement = "0.042") %>%
		str_replace(pattern = "Feb", replacement = "0.125") %>%
		str_replace(pattern = "Mar", replacement = "0.208") %>%
		str_replace(pattern = "Apr", replacement = "0.292") %>%
		str_replace(pattern = "May", replacement = "0.375") %>%
		str_replace(pattern = "Jun", replacement = "0.458") %>%
		str_replace(pattern = "Jul", replacement = "0.542") %>%
		str_replace(pattern = "Aug", replacement = "0.625") %>%
		str_replace(pattern = "Sep", replacement = "0.708") %>%
		str_replace(pattern = "Oct", replacement = "0.792") %>%
		str_replace(pattern = "Nov", replacement = "0.875") %>%
		str_replace(pattern = "Dec", replacement = "0.958") %>%
		str_replace(pattern = "kunk", replacement = "") %>%
		str_replace(pattern = "unk", replacement = "")
	
	sia <- data$season_inactive
	
	if (length(sia) < 1) {
		print("It seems like something is wrong with the column 'season_inactive'")
		return()
	}
	
	split <- str_split_fixed(sia, pattern = "-", n = 2)
	split <- as.data.frame(split)
	split$V1 <- as.character(split$V1)
	split$V2 <- as.character(split$V2)
	split$is_in_south <- data$is_in_south
	
	## set ones with multiple sia to first 
	split$V1[which(sia == "Summer and Winter")] = "Summer"
	split$V1[which(sia == "Spring and Winter")] = "Spring"
	split$V1[which(sia == "Fall and Winter")] = "Fall"
	
	## if is in south is true, flip the seasons:
	i = 1
	while (i < length(split$V1) + 1) {
		if(split$is_in_south[i] == 'Y') {
			if(split$V1[i] == "Summer") {
				split$V1[i] = "Winter"
			}
			else if(split$V1[i] == "Spring") {
				split$V1[i] = "Fall"
			}
			else if(split$V1[i] == "Winter") {
				split$V1[i] = "Summer"
			}
			else if(split$V1[i] == "Fall") {
				split$V1[i] = "Spring"
			}
			if (split$V2[i] == "Summer") {
				split$V2[i] = "Winter"
			}
			else if (split$V2[i] == "Spring") {
				split$V2[i] = "Fall"
			}
			else if (split$V2[i] == "Winter") {
				split$V2[i] = "Summer"
			}
			else if (split$V2[i] == "Fall") {
				split$V2[i] = "Spring"
			}
		}
		i = i + 1
	}
	
	split <- split %>% ## if only one month/season, put value in both columns
		mutate(V2 = if_else(V2=="", V1 , V2))
	
	season_inactive_start <- split$V1 %>%
		str_replace(pattern = "Winter", replacement = "0.875") %>%
		str_replace(pattern = "Spring", replacement = "0.208") %>%
		str_replace(pattern = "Summer", replacement = "0.458") %>%
		str_replace(pattern = "Fall", replacement = "0.708") %>%
		str_replace(pattern = "Jan", replacement = "0.042") %>%
		str_replace(pattern = "Feb", replacement = "0.125") %>%
		str_replace(pattern = "Mar", replacement = "0.208") %>%
		str_replace(pattern = "Apr", replacement = "0.292") %>%
		str_replace(pattern = "May", replacement = "0.375") %>%
		str_replace(pattern = "Jun", replacement = "0.458") %>%
		str_replace(pattern = "Jul", replacement = "0.542") %>%
		str_replace(pattern = "Aug", replacement = "0.625") %>%
		str_replace(pattern = "Sep", replacement = "0.708") %>%
		str_replace(pattern = "Oct", replacement = "0.792") %>%
		str_replace(pattern = "Nov", replacement = "0.875") %>%
		str_replace(pattern = "Dec", replacement = "0.958") %>%
		str_replace(pattern = "kunk", replacement = "") %>%
		str_replace(pattern = "unk", replacement = "") %>%
		str_replace(pattern = "none", replacement = "")
	
	season_inactive_stop <- split$V2 %>%
		str_replace(pattern = "Winter", replacement = "0.125") %>%
		str_replace(pattern = "Spring", replacement = "0.375") %>%
		str_replace(pattern = "Summer", replacement = "0.625") %>%
		str_replace(pattern = "Fall", replacement = "0.792") %>%
		str_replace(pattern = "Jan", replacement = "0.042") %>%
		str_replace(pattern = "Feb", replacement = "0.125") %>%
		str_replace(pattern = "Mar", replacement = "0.208") %>%
		str_replace(pattern = "Apr", replacement = "0.292") %>%
		str_replace(pattern = "May", replacement = "0.375") %>%
		str_replace(pattern = "Jun", replacement = "0.458") %>%
		str_replace(pattern = "Jul", replacement = "0.542") %>%
		str_replace(pattern = "Aug", replacement = "0.625") %>%
		str_replace(pattern = "Sep", replacement = "0.708") %>%
		str_replace(pattern = "Oct", replacement = "0.792") %>%
		str_replace(pattern = "Nov", replacement = "0.875") %>%
		str_replace(pattern = "Dec", replacement = "0.958") %>%
		str_replace(pattern = "kunk", replacement = "") %>%
		str_replace(pattern = "unk", replacement = "") %>%
		str_replace(pattern = "none", replacement = "")
	
	## set second season when inactive:
	split$V1 <- ""
	split$V2 <- ""
	split$V1[which(sia == "Summer and Winter")] = "Winter"
	split$V1[which(sia == "Spring and Winter")] = "Winter"
	split$V1[which(sia == "Fall and Winter")] = "Winter"
	
	## if is in south is true, flip the seasons:
	i = 1
	while (i < length(split$V1) + 1) {
		if(split$is_in_south[i] == 'Y') {
			if(split$V1[i] == "Summer") {
				split$V1[i] = "Winter"
			}
			else if(split$V1[i] == "Spring") {
				split$V1[i] = "Fall"
			}
			else if(split$V1[i] == "Winter") {
				split$V1[i] = "Summer"
			}
			else if(split$V1[i] == "Fall") {
				split$V1[i] = "Spring"
			}
			if (split$V2[i] == "Summer") {
				split$V2[i] = "Winter"
			}
			else if (split$V2[i] == "Spring") {
				split$V2[i] = "Fall"
			}
			else if (split$V2[i] == "Winter") {
				split$V2[i] = "Summer"
			}
			else if (split$V2[i] == "Fall") {
				split$V2[i] = "Spring"
			}
		}
		i = i + 1
	}
	
	split <- split %>% ## if only one month/season, put value in both columns
		mutate(V2 = if_else(V2=="", V1 , V2))
	
	season_inactive_start2 <- split$V1 %>%
		str_replace(pattern = "Winter", replacement = "0.875")
	
	season_inactive_stop2 <- split$V2 %>%
		str_replace(pattern = "Winter", replacement = "0.125")
	
	data$season_when_away_100km_start <- season_when_away_100km_start
	data$season_when_away_100km_stop <- season_when_away_100km_stop
	data$season_inactive_start <- season_inactive_start
	data$season_inactive_stop <- season_inactive_stop
	data$season_inactive_start2 <- season_inactive_start2
	data$season_inactive_stop2 <- season_inactive_stop2
	
	return(data)
} 
## converts string season when away/inactive in intratherm to numeric season when away/inactive stop and start
## takes as an argument a data frame containing season_when_away_100km and season_inactive
## factors in these columns must be a single season or month (first three letters of month ex. Feb) or a range separated by a hyphen (ex. Spring-Winter)
## returns the same dataframe with new columns season_when_away_100km_start, season_when_away_100km_stop, season_inactive_start, season_inactive_stop, season_inactive_start2 and season_inactive_stop2

## ----------- DATE NUMBER CONVERSION ----------------
##		Seasons: each lasts 3 months 
##  		Spring (Mar, Apr, May) = 0.208 - 0.375
##			Summer (Jun, Jul, Aug) = 0.458 - 0.625
## 			Fall (Sep, Oct, Nov) = 0.708 - 0.792	
##			Winter (Dec, Jan, Feb) = 0.875 - 0.125
##
## 		MONTHS:
##		Jan = 0.042		Feb = 0.125		Mar = 0.208
##		Apr = 0.292		May = 0.375		Jun = 0.458
## 		Jul = 0.542		Aug = 0.625		Sep = 0.708
##		Oct = 0.792		Nov = 0.875		Dec = 0.958
##

initialize_months <- function(away_start, away_stop, months) {
	##jan start
	if(away_start == 0.042) {
		if (away_stop == 0.125){
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[1:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[1:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[1:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[1:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[1:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[1:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[1:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[1:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[1:12] = TRUE
		}
	}
	##feb start
	else if(away_start == 0.125) {
		if (away_stop == 0.208){
			months[2:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[2:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[2:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[2:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[2:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[2:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[2:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[2:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[2:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[2:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[2:12] = TRUE
			months[1] = TRUE
		}
	}
	## march start
	else if(away_start == 0.208) {
		if (away_stop == 0.292){
			months[3:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[3:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[3:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[3:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[3:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[3:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[3:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[3:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[3:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[3:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[3:12] = TRUE
			months[1:2] = TRUE
		}
	}
	## apr start
	else if(away_start == 0.292) {
		if (away_stop == 0.375){
			months[4:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[4:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[4:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[4:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[4:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[4:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[4:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[4:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[4:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[4:12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[4:12] = TRUE
			months[1:3] = TRUE
		}
	}
	## may start
	else if(away_start == 0.375) {
		if (away_stop == 0.458){
			months[5:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[5:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[5:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[5:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[5:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[5:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[5:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[5:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[5:12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[5:12] = TRUE
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[5:12] = TRUE
			months[1:4] = TRUE
		}
	}
	## june start
	else if(away_start == 0.458) {
		if (away_stop == 0.542){
			months[6:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[6:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[6:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[6:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[6:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[6:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[6:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[6:12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[6:12] = TRUE
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[6:12] = TRUE
			months[1:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[6:12] = TRUE
			months[1:5] = TRUE
		}
	}
	## july start
	else if(away_start == 0.542) {
		if (away_stop == 0.625){
			months[7:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[7:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[7:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[7:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[7:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[7:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[7:12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[7:12] = TRUE
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[7:12] = TRUE
			months[1:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[7:12] = TRUE
			months[1:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[7:12] = TRUE
			months[1:6] = TRUE
		}
	}
	## aug start
	else if(away_start == 0.625) {
		if (away_stop == 0.708){
			months[8:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[8:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[8:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[8:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[8:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[8:12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[8:12] = TRUE
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[8:12] = TRUE
			months[1:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[8:12] = TRUE
			months[1:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[8:12] = TRUE
			months[1:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[8:12] = TRUE
			months[1:7] = TRUE
		}
	}
	## sept start
	else if(away_start == 0.708) {
		if (away_stop == 0.792){
			months[9:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[9:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[9:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[9:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[9:12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[9:12] = TRUE
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[9:12] = TRUE
			months[1:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[9:12] = TRUE
			months[1:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[9:12] = TRUE
			months[1:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[9:12] = TRUE
			months[1:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[9:12] = TRUE
			months[1:8] = TRUE
		}
	}
	## oct start
	else if(away_start == 0.792) {
		if (away_stop == 0.875){
			months[10:11] = TRUE
		}
		else if (away_stop == 0.958){
			months[10:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[10:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[10:12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[10:12] = TRUE
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[10:12] = TRUE
			months[1:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[10:12] = TRUE
			months[1:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[10:12] = TRUE
			months[1:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[10:12] = TRUE
			months[1:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[10:12] = TRUE
			months[1:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[10:12] = TRUE
			months[1:9] = TRUE
		}
	}
	## nov start
	else if(away_start == 0.875) {
		if (away_stop == 0.958){
			months[11:12] = TRUE
		}
		else if (away_stop == 0.042){
			months[11:12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[11:12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[11:12] = TRUE
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[11:12] = TRUE
			months[1:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[11:12] = TRUE
			months[1:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[11:12] = TRUE
			months[1:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[11:12] = TRUE
			months[1:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[11:12] = TRUE
			months[1:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[11:12] = TRUE
			months[1:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[11:12] = TRUE
			months[1:10] = TRUE
		}
	}
	## dec start
	else if(away_start == 0.958) {
		if (away_stop == 0.042){
			months[12] = TRUE
			months[1] = TRUE
		}
		else if (away_stop == 0.125){
			months[12] = TRUE
			months[1:2] = TRUE
		}
		else if (away_stop == 0.208){
			months[12] = TRUE
			months[1:3] = TRUE
		}
		else if (away_stop == 0.292){
			months[12] = TRUE
			months[1:4] = TRUE
		}
		else if (away_stop == 0.375){
			months[12] = TRUE
			months[1:5] = TRUE
		}
		else if (away_stop == 0.458){
			months[12] = TRUE
			months[1:6] = TRUE
		}
		else if (away_stop == 0.542){
			months[12] = TRUE
			months[1:7] = TRUE
		}
		else if (away_stop == 0.625){
			months[12] = TRUE
			months[1:8] = TRUE
		}
		else if (away_stop == 0.708){
			months[12] = TRUE
			months[1:9] = TRUE
		}
		else if (away_stop == 0.792){
			months[12] = TRUE
			months[1:10] = TRUE
		}
		else if (away_stop == 0.875){
			months[12] = TRUE
			months[1:11] = TRUE
		}
	}
	return(months)
} 
## sets months when away to TRUE based on numeric stops and starts
