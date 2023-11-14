## BioTIME
## cleaning the BioTIME/intratherm species overlap data
library(tidyverse)
library(taxize)

intratherm <- read.csv("data-processed/intratherm-with-elev.csv", stringsAsFactors = FALSE)
biotime <- read.csv("data-raw/BioTIMEQuery02_04_2018.csv", stringsAsFactors = FALSE)

over_species <- intersect(unique(biotime$SPECIES), unique(intratherm$species))

ol_species <- biotime %>%
	filter(SPECIES %in% over_species)

names <- unique(ol_species$GENUS_SPECIES)
tsn_search <- get_tsn(as.character(names), accepted = FALSE) ## find tsn for each unique taxa
tsn_search <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/tsn_search_popdynam_biotime.rds")
tsns <- data.frame(tsn_search) %>%
	mutate(taxa = names)

found <- tsns %>%
	subset(match == "found") 

report <- lapply(found$ids, itis_acceptname)
report_df <- data.frame(matrix(unlist(report), nrow=914, byrow=T),stringsAsFactors=FALSE)
report_df <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/report_df_popdynam_biotime.rds")

found <- found %>%
	mutate(genus_species_corrected = report_df$X2)

merged_unique <- left_join(tsns, found) %>%
	mutate(taxa = as.character(taxa)) %>%
	mutate(genus_species = taxa) %>%
	select(genus_species, taxa, genus_species_corrected)

## if names found are not accepted names, then change to accepted name
i = 1
while (i < length(merged_unique$taxa)+1) {
	if (!is.na(merged_unique$genus_species_corrected[i])) {
		merged_unique$taxa[i] <- merged_unique$genus_species_corrected[i]
	}
	i = i+1
}

overlap <- intersect(unique(merged_unique$taxa), unique(intratherm$genus_species))

merged_unique <- merged_unique %>%
	filter(taxa %in% overlap) %>%
	filter(!duplicated(taxa))

over_biotime <- ol_species %>% 
	left_join(., merged_unique, by = c("GENUS_SPECIES" = "genus_species")) %>%
	rename(genus_species_intra = taxa) %>%
	select(-genus_species_corrected) %>%
	filter(GENUS_SPECIES %in% merged_unique$taxa) 

meta <- read.csv("data-raw/BioTIMEMetadata_02_04_2018.csv", stringsAsFactors = FALSE)

ol <- left_join(over_biotime, meta, by = "STUDY_ID") %>%
	filter(!str_detect(DATA_SOURCE, "Global Population Dynamics Database")) %>%
	arrange(STUDY_ID, SAMPLE_DESC, GENUS_SPECIES, YEAR) %>%
	select(STUDY_ID, SAMPLE_DESC, GENUS_SPECIES, YEAR, sum.allrawdata.ABUNDANCE, 
		   sum.allrawdata.BIOMASS, everything()) %>%
	mutate(GENUS_SPECIES = genus_species_intra) %>%  
	select(-X, -genus_species_intra) 

## fix latitude and longitude for study 57:
fiftysev <- ol %>%
	filter(STUDY_ID == 57) %>%
	mutate(lake = str_split_fixed(SAMPLE_DESC, pattern = "_", n = 6)[,4]) 

new_info <- data.frame(matrix(byrow = TRUE, nrow = 10, ncol = 5, 
							  c("AL", "Allequash", "46.038", "-89.621", "1981",
							    "BM", "Big Muskellunge", "46.021", "-89.612", "1981", 
							    "CR", "Crystal", "46.003", "-89.612", "1981", 
							    "SP", "Sparkling", "46.008", "-89.701", "1981", 
							    "TR", "Trout", "46.029", "-89.665", "1981", 
							    "TB", "Trout Bog", "46.041", "-89.686", "1981", 
							    "ME", "Mendota", "43.099", "-89.405", "1981", 
							    "MO", "Monona", "43.063", "-89.361", "1995", 
							    "WI", "Wingra", "43.053", "-89.425", "1995", 
							    "FI", "Fish", "43.287", "-89.652", "1995")))

colnames(new_info) <- c("lake", "lake_name", "lat", "lon", "start_year")

fiftysev <- left_join(fiftysev, new_info, by = "lake") %>%
	mutate(LATITUDE = lat, LONGITUDE = lon, START_YEAR = start_year) %>%
	select(-lat, -lon, -start_year, -lake, -lake_name) 

ol <- ol %>%
	filter(STUDY_ID != 57) %>%
	rbind(., fiftysev)

########################################################################################
## split studies into grid cells of 96km^2 individually and assign new GRID_ID (STUDY_ID = grid cell number)
library(sf)
library(sp)
library(rgdal)
library(rnaturalearth)

study_ids <- unique(ol$STUDY_ID)
i = 1
grid_ids <- data.frame(matrix(ncol = 6)) 
colnames(grid_ids) <- c("x", "y", "grid_id", "STUDY_ID", "latitude", "longitude")
UTMzones <- seq(from = -180, to = 179, by = 6)

while(i <  length(study_ids)+1) {
	study <- subset(ol, STUDY_ID == study_ids[i])
	
	## figure out UTM zone:
	## split the study into UTM zones and for each zone, perform the grid overlay
	zones <- sapply(as.numeric(as.character(study$LONGITUDE)), FUN = function (x) {
		which.min(abs(x - UTMzones))
		})
	
	study$zone <- zones
	study_split <- split(study, study$zone)
	z = 1
	while (z < length(study_split) + 1) {
		study_sub <- study_split[[z]]
		zone <- first(study_sub$zone)
		
		# map study site locations
		pts <- unique(data.frame(y = as.numeric(as.character(study_sub$LATITUDE)), 
								 x = as.numeric(as.character(study_sub$LONGITUDE))))
		coordinates(pts)= ~ x + y
		proj4string(pts)=CRS("+proj=longlat +datum=WGS84") # set it to lat-long from the World Geodesic System
		
		pts = spTransform(pts,CRS(paste("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs", sep = ""))) # transform so measurements can be in metres and accurate by using UTM zones 
		pts <- st_as_sf(pts)
		
		# create 96km hexagonal grid
		grid_96 <- st_make_grid(pts, cellsize = 6080, square = FALSE) %>% 
			st_sf(grid_id = paste(zone, 1:length(.), sep = "_"))
		
		# create labels for each grid_id
		grid_lab <- st_centroid(grid_96) %>% cbind(st_coordinates(.))
		
		new_coords <- grid_lab %>%
			as.data.frame() %>%
			select(X, Y, grid_id) 
		
		coordinates(new_coords)= ~ X + Y
		proj4string(new_coords)=CRS(paste("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs", sep = ""))
		
		new_coords <- new_coords %>%
			spTransform(., CRS("+proj=longlat +datum=WGS84")) %>%
			st_as_sf(.) %>%
			cbind(st_coordinates(.)) 
		
		st_geometry(new_coords) <- NULL
	
		new_coords <- new_coords %>%
			rename(latitude = Y, longitude = X) %>%
			select(latitude, longitude, grid_id)
		
		# view the sampled points and grid
		ggplot() +
			geom_sf(data = pts, color = 'red', size = 1.7) +
			geom_sf(data = grid_96, fill = 'transparent', lwd = 0.3) +
			geom_text(data = grid_lab, aes(x = X, y = Y, label = grid_id), size = 2) +
			coord_sf(datum = "WGS84", 
					 crs = CRS(paste("+proj=utm +zone=", zone, 
					 				" +datum=WGS84 +units=m +no_defs", sep = "")))  +
			labs(x = "") +
			labs(y = "")
		
		pts <- pts %>%
			st_join(grid_96, join = st_intersects) %>%
			as.data.frame 
		
		pts <- pts[!duplicated(pts$geometry),]
		
		pts <- cbind(unique(data.frame(y = study_sub$LATITUDE, x = study_sub$LONGITUDE)), pts) %>%
			mutate(STUDY_ID = study_ids[i]) %>%
			select(-geometry) %>%
			left_join(., new_coords, by = "grid_id")
		
		grid_ids <- rbind(grid_ids, pts[,1:6]) 
		
		z = z+1
	}
	
	i = i + 1
}

grid_ids <- grid_ids[-1,] %>%
	rename(LATITUDE = y, LONGITUDE = x) 

## combine with ol to create new column: 
new_ol <- left_join(ol, grid_ids, by = c("LATITUDE", "LONGITUDE", "STUDY_ID")) %>%
	mutate(GRID_ID = paste(STUDY_ID, grid_id, sep = "_")) %>%
	mutate(CENT_LAT = latitude, CENT_LONG = longitude) %>%
	select(GRID_ID, STUDY_ID, grid_id, everything()) %>%
	select(-grid_id, -latitude, -longitude)

## get rid of GRID_IDs with fewer than 5 years of sampling  
long_ones <- new_ol %>%
	group_by(GRID_ID) %>%
	mutate(ts_duration_years = length(unique(YEAR))) %>%
	select(ts_duration_years, GRID_ID) %>%
	ungroup() %>%
	filter(!duplicated(GRID_ID)) %>%
	filter(ts_duration_years >= 5) 

long_ones <- subset(new_ol, new_ol$GRID_ID %in% long_ones$GRID_ID)


########################################################################################
## make data frame of all sampled dates for each location so can add 0 abundance for days sampled but not found
sample_dates <- long_ones %>%
	group_by(GRID_ID) %>%
	arrange(GRID_ID, YEAR, MONTH, DAY) %>%
	select(GRID_ID, YEAR, MONTH, DAY) %>%
	distinct() %>%
	mutate(decimal_date = (MONTH*30+DAY)/365 + YEAR) %>% ## create decimal_date column for each sampling time
	mutate(decimal_date = ifelse(is.na(MONTH) & is.na(DAY), YEAR, decimal_date)) %>%
	filter(!is.na(decimal_date)) %>% 
	select(decimal_date, GRID_ID) %>%
	ungroup()

## create rows for absence data - ie. when a GRID_ID location was sampled on a date and a species was found over one of the years in that location, set abundance to zero in years where it was not reported as found

gridIDs <- long_ones %>% 
	mutate(decimal_date = (MONTH*30+DAY)/365 + YEAR) %>% ## create decimal_date column for each sampling time
	mutate(decimal_date = ifelse(is.na(MONTH) & is.na(DAY), YEAR, decimal_date)) %>%
	arrange(STUDY_ID, GRID_ID, GENUS_SPECIES, decimal_date, YEAR, MONTH, DAY) %>%
	unique() %>%
	split(., .$GRID_ID, .$GENUS_SPECIES) 

new_longones <- c()

## merge so each study has each combination of sampling time x species
i = 1
while(i < length(gridIDs) + 1) {
	species <- split(gridIDs[[i]], gridIDs[[i]]$GENUS_SPECIES)
	dates <- sample_dates %>%
		filter(GRID_ID == unique(species[[1]]$GRID_ID))
	
	z = 1
	while(z < length(species) + 1) {
		current_species <- left_join(dates, species[[z]],
									 by = c("decimal_date", "GRID_ID")) %>%
			fill(-DAY, -YEAR, -MONTH, -sum.allrawdata.ABUNDANCE, -sum.allrawdata.BIOMASS, .direction = "updown")
		
		new_longones <- rbind(new_longones, current_species)
		
		z = z + 1
	}
	i = i + 1
}

new_longones




