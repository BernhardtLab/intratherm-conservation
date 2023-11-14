

### initial analysis

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(geosphere)
library(broom)


intratherm <- read_csv("data-processed/intratherm-may-2020-squeaky-clean.csv") %>% 
	mutate(population_id = paste(population_id, longitude, sep = "_")) 

View(intratherm)
### how many populations do we have per species?

intratherm %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(genus_species) %>% 
	distinct(lat_long) %>% 
	tally() %>% 
	ggplot(aes(x = n)) + geom_histogram() +
	ylab("Frequency") + xlab("Number of distinct population locations") +
	scale_x_continuous(breaks = seq(1, 16, by = 1))
ggsave("figures/number-locations.png", width = 6, height = 4)

### from how many studies does each species have data from?

intratherm %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(genus_species) %>% 
	distinct(ref) %>% 
	tally() %>% 
	ggplot(aes(x = n)) + geom_histogram() +
	ylab("Frequency") + xlab("Number of studies per species") +
	scale_x_continuous(breaks = seq(1, 10, by = 1))
ggsave("figures/number-studies-per-species.png", width = 6, height = 4)


### how to calculate distance between two points?


# distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

### get all the the species that only have two populations

names(intratherm)

intra2 <- intratherm %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	select(elevation_of_collection, everything()) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(genus_species) %>% 
	select(genus_species, lat_long) %>% 
	distinct(genus_species, lat_long) %>% 
	tally() %>% 
	filter(n == 2)

intra3 <- intratherm %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	filter(genus_species %in% c(intra2$genus_species)) %>% 
	group_by(genus_species) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	distinct(lat_long, .keep_all = TRUE) %>% 
	select(genus_species, latitude, longitude)

intra_split <- intra3 %>% 
	split(.$genus_species) 



get_distance <- function(df){
	distance = distm(c(df[[1]]$longitude[1], df[[1]]$latitude[1]),
					 c(df[[1]]$longitude[2], df[[1]]$latitude[2]), fun = distHaversine)
	out <- data.frame(distance = distance[1])
	out
}

	

result <- data.frame() 
for (i in 1:length(intra_split)) {
	squared <- get_distance(intra_split[i])
	hold <- data.frame(species = intra_split[i][[1]]$genus_species[1], distance = squared)
	result <- bind_rows(result, hold)
}

distances <- result %>% 
	mutate(distance_km = distance/1000)
	

distances %>% 
	ggplot(aes(x = distance_km)) + geom_histogram() +
	ylab("Frequency") + xlab("Distance among populations (km)")
ggsave("figures/distances-km.png", width = 6, height = 4)


### read in temperature data
	
	intra_temps <- read_csv("~/Documents/too-big-for-github/intratherm-terrestrial-temps-tavg.csv") 
	
	
	intra_temps %>%
		select(contains("Anaxyrus")) %>% View
	
	intra_long <- intra_temps %>% 
		mutate(date = as.character(date)) %>%
		separate(date, sep = 4, into = c("year", "decimal_year"), remove = FALSE) %>% 
		gather(key = population_id, value = temperature, 4:299) %>% 
		group_by(population_id, year) %>% 
		summarise(max_temp = max(temperature)) %>% 
		ungroup() %>% 
		group_by(population_id) %>% 
		summarise(mean_max_temp = mean(max_temp))

	intra_long2 <- intra_long %>% 
		filter(!is.na(mean_max_temp))
	
	write_csv(intra_long, "data-processed/intratherm-temp-data-yearly-maxes-daily-avg.csv")
	
#### ok which of these are terrestrial? Only use the terrestrial species for now
	
	unique(intratherm$realm_general2)
	
	intra_temps <- intratherm %>% 
		filter(realm_general2 == "Terrestrial") %>% 
		select(genus_species, population_id, latitude, longitude) %>% 
		distinct(.) %>% 
		left_join(., intra_long2, by = c("population_id")) 
		
#### daily mean maximum temperature
	
	names(intratherm)
	
library(readxl)	
	intra_with_elev <- read_excel("data-processed/intratherm-with-elev.xlsx") %>% 
		mutate(population_id = paste(population_id, longitude, sep = "_")) %>% 
		select(intratherm_id, raster_mean, elevation_of_collection, realm_general2) %>% 
		filter(realm_general2 == "Terrestrial")
		
	
	intra_species <- intratherm %>% 
		select(intratherm_id, genus_species, latitude, longitude, population_id) %>% 
		distinct() %>% 
		left_join(., intra_with_elev)
	
	differences <- intra_long2 %>%
		left_join(., intra_species) %>% 
		# filter(!is.na(mean_max_temp)) %>% 
		# filter(genus_species %in% c(distances$species)) %>% 
		select(intratherm_id, genus_species, mean_max_temp, population_id, latitude, longitude, raster_mean, elevation_of_collection) %>% 
		left_join(., distances, by = c("genus_species" = "species")) %>% 
		mutate(elevation_of_collection = as.numeric(elevation_of_collection)) %>% 
		mutate(raster_mean = as.numeric(raster_mean)) %>% 
		mutate(temp_elev_corr = mean_max_temp + 5.5*((elevation_of_collection - raster_mean)/1000)) %>% 
		group_by(genus_species) %>%
		mutate(temp_diff = max(temp_elev_corr) - min(temp_elev_corr)) %>% 
		filter(temp_diff != 0) %>% 
		filter(!is.na(distance))
	
	realms <- intratherm %>% 
		select(genus_species, realm_general2) %>% 
		distinct()
	
	differences %>% 
		# distinct(temp_diff, .keep_all = TRUE) %>% 
		left_join(., realms) %>% 
		filter(realm_general2 == "Terrestrial") %>%
		ggplot(aes(x = distance_km, y = temp_diff, color = realm_general2)) + geom_point() +
		ylab("Max temperature difference") + xlab("Distance between populations (km)")
	ggsave("figures/temp_diffs_distances.png", width = 6, height = 4)
	

	
	
	
# Freshwater temperatures -------------------------------------------------

fw <- read_csv("data-processed/intratherm-freshwater-temp-data.csv")
intra_fw <- read_csv("data-processed/intratherm-may-2020-squeaky-clean.csv") %>% 
	mutate(population_id = paste(population_id, longitude, sep = "_")) %>% 
	filter(realm_general2 == "Freshwater")
	

fw2 <- fw %>% 
	gather(key = population_id, value = monthly_temp, 4:194) %>% 
	separate(date_number, into = c("year", "fraction")) %>% 
	group_by(population_id, year) %>% 
	summarise(max_yearly_temp = max(monthly_temp)) %>% 
	group_by(population_id) %>% 
	summarise(mean_yearly_max_temp = mean(max_yearly_temp))

intra_fw2 <- intra_fw %>% 
	left_join(., fw2)

	
fw3 <- fw2 %>% 
	separate(population_id, into = c("genus_species", "coordinates"), sep = "_", remove = FALSE) %>% 
	filter(genus_species %in% distances$species) %>% 
	left_join(., distances, by = c("genus_species" = "species")) %>% 
	group_by(genus_species) %>% 
	mutate(temp_difference = max(mean_yearly_max_temp) - min(mean_yearly_max_temp)) %>%
	filter(temp_difference != 0)


fw3 %>% 
	ggplot(aes(x = distance_km, y = temp_difference)) + geom_point() +
	ylab('Difference in yearly monthly max temperatures (°C)') +
	xlab("Distance between populations (km)")
ggsave("figures/temp-differences-freshwater.png", width = 6, height = 4)


### get all combinations
sample_list <- NULL
for (i in 1:nrow(ntbl_sub1) ) {
	output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
	output <- bind_rows(output, .id = "sample_id")
	subsample_size <- rep(i, nrow(output))
	output <- cbind(output, subsample_size)
	sample_list <- rbind(sample_list,output)
}



#### ok now try to get all pairwise distances
intratherm <- read_csv("data-processed/intratherm-with-elev.csv") %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	mutate(population_id2 = paste(genus_species, latitude, longitude, sep = "_"))

### find all species that have 2 or more populations
intra2 <- intratherm %>% 
	select(elevation_of_collection, everything()) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(genus_species) %>% 
	select(genus_species, lat_long) %>% 
	distinct(genus_species, lat_long) %>% 
	tally() %>% 
	filter(n > 1)

### find all the possible combinations of population pairs
find_all_combos <- function(df) {
	combos <- combn(x = df$intratherm_id, m = 2) %>% 
	t %>% 
	as.data.frame() %>% 
	rename(pop1 = V1,
		   pop2 = V2) 
	combos$genus_species <- unique(df$genus_species)
	combos$unique_combo <- paste(unique(df$genus_species), rownames(combos), sep = "_")
	return(combos)
}

## split intratherm by species
intra_split <- intratherm %>% 
	filter(genus_species %in% c(intra2$genus_species)) %>% 
	distinct(genus_species, lat_long, .keep_all = TRUE) %>% 
	split(.$genus_species)

### find population pairs
pop_combos <- intra_split %>% 
	map_df(find_all_combos) 

## use distm function to calculate distances in meters among populations
distances <- data.frame() 
for (i in 1:nrow(pop_combos)) {
	longitude1 <- intratherm$longitude[intratherm$intratherm_id == pop_combos$pop1[i]]
	latitude1 <- intratherm$latitude[intratherm$intratherm_id == pop_combos$pop1[i]]
	longitude2 <- intratherm$longitude[intratherm$intratherm_id == pop_combos$pop2[i]]
	latitude2 <- intratherm$latitude[intratherm$intratherm_id == pop_combos$pop2[i]]
	unique_combo <- pop_combos$unique_combo[i]
	genus_species <- pop_combos$genus_species[i]
	distance <- distm(x = c(longitude1, latitude1), y = c(longitude2, latitude2), fun = distHaversine)
	hold <- data.frame(pop1_long = longitude1, pop1_lat = latitude1,
					   pop2_long = longitude2, pop2_lat = latitude2,
					   unique_combo = unique_combo,
					   genus_species = genus_species,
					   distance = distance/1000)
	distances <- bind_rows(distances, hold)
}

### plot pairwise distances among all populations of a given species
distances %>% 
	ggplot(aes(x = distance)) + geom_histogram()



# Pop difs analysis -------------------------------------------------------
library(broom)

pop_difs1 <- read_csv("data-processed/pop_difs.csv")
realms <- read_csv("data-processed/pop_difs.csv") %>% 
	select(genus_species, realm_general2) %>% 
	distinct()
pop_difs <- read_csv("data-processed/initialize_pairwise_differences_experienced_and_topt.csv")
View(pop_difs)

pd2 <- pop_difs %>% 
	mutate(exp_temp_diff = ifelse(!is.na(temp_difference_topt), temp_difference_topt, experienced_temp_difference)) %>% 
	mutate(pop1 = paste(genus_species, lat_lon_1, sep = "_")) %>% 
	mutate(pop2 = paste(genus_species, lat_lon_2, sep = "_")) %>% 
	left_join(., realms, by = "genus_species")


## this is model we want to fit: 
### tmax.diffspecies_i ~ ARRspecies_i + tenvmax.diff + dispersal_distance + spatial_distance + dispersal_distance:spatial_distance + ARR:tenvmax.dif, random=species.
library(broom)

### get ARR for each species
intratherm <- read_csv("./data-processed/intratherm-may-2020-squeaky-clean.csv") %>% 
	mutate(population_id = paste(population_id, longitude, sep = "_")) %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) 

intratherm_elev <- read_csv("data-processed/intratherm-with-elev.csv") %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	mutate(population_id2 = paste(genus_species, latitude, longitude, sep = "_"))

multi_acc <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	tally() %>% 
	filter(n > 1)
	

arrs <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(population_id %in% c(multi_acc$population_id)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(realm_general2, genus_species, population_id, lat_long) %>% 
	do(tidy(lm(parameter_value~acclim_temp, data=.))) 

### look at the slopes
intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	# filter(population_id == "Lithobates sylvaticus_37.56_NA_-84.3") %>% 
	ggplot(aes(x = acclim_temp, y = parameter_value, group = population_id)) + 
	geom_smooth(method = "lm", se = FALSE, alpha = 0.5) + geom_point() +
	ylab("CTmax") + xlab("Acclimation temperature")
ggsave("figures/arr-slopes.png", width = 8, height = 6)

arr_slopes <- arrs %>% 
	filter(term != "(Intercept)") %>% ## ok fewer than half of the data have more than 2 acclimation temperatures
	select(genus_species, population_id, estimate) %>% 
	rename(slope = estimate) %>% 
	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") %>% ## this is the super high ARR, it's only got two data points
	filter(population_id != "Perca flavescens_42.08_NA_-81.34") ### this is the super low ARR

### plot the distribution of slopes

arr_slopes %>% 
	ggplot(aes(x = slope)) + geom_histogram() +
	geom_vline(xintercept = 0) + xlab("ARR (slope)")
ggsave("figures/ARR-slope-histogram.png", width = 8, height = 6)

intercepts <- arrs %>% 
	filter(term == "(Intercept)")  %>% 
	select(genus_species, population_id, estimate) %>% 
	rename(intercept = estimate) %>% 
	filter(population_id != "Perca flavescens_42.08_NA_-81.34") %>%  ### this is the super low ARR
	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") ## this is the super high ARR, it's only got two data points


slopes_int <- left_join(intercepts, arr_slopes)

ctmax_20 <- arrs %>% 
	select(genus_species, population_id, term, estimate) %>% 
	spread(key = term, value = estimate) %>% 
	rename(intercept = `(Intercept)`) %>% 
	mutate(ctmax_20 = acclim_temp*20 + intercept) %>% 
	filter(!is.na(ctmax_20)) 

arr_slopes %>% 
	ggplot(aes(x = slope)) + geom_histogram(bins = 70) +
	xlab("ARR slope") +
	facet_wrap( ~ realm_general2)
ggsave("figures/ARR-slopes-histogram.png", width = 8, height = 4)

arr_slopes2 <- arr_slopes %>% 
	group_by(genus_species) %>% 
	summarise(mean_arr = mean(slope))

dispersals <- intratherm %>% 
	select(genus_species, dispersal_distance_category) %>% 
	distinct() 


intra2 <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(population_id %in% c(multi_acc$population_id)) 

arr_model <- function(df) {
	lm(parameter_value~acclim_temp, data = df)
}


by_popid <- intra2 %>%
	group_by(realm_general2, genus_species, population_id) %>%
	nest()

by_popid2 <- by_popid %>%
	mutate(model = map(.x = data, .f = arr_model))


by_popid3 <- by_popid2 %>% 
	mutate(
		preds  = map2(.x = data, .y = model, .f = add_predictions),
		resids = map2(.x = data, .y = model, .f = add_residuals)
	)


preds <- unnest(data = by_popid3, preds)
resids <- unnest(data = by_popid3, resids)

preds %>%
	select(genus_species, population_id, acclim_temp, pred, parameter_value) %>% 
	ggplot(aes(acclim_temp, pred, group = population_id)) +
	geom_line(alpha = 0.35) 

resids %>%
	ggplot(aes(acclim_temp, resid, group = population_id)) +
	geom_line(alpha = 0.35)

#### ok now get the ctmax differences

View(pop_difs)

# pd2 <- pop_difs %>% 
# 	mutate(pop1 = paste(genus_species, lat_lon_1, sep = "_")) %>% 
# 	mutate(pop2 = paste(genus_species, lat_lon_2, sep = "_")) 

pop1_ctmax <- ctmax_20 %>% 
	mutate(pop1 = paste(genus_species, lat_long, sep = "_")) %>% 
	filter(pop1 %in% c(pd2$pop1)) %>% 
	ungroup() %>% 
	select(pop1, ctmax_20) %>% 
	rename(ctmax_pop1 = ctmax_20)

pop2_ctmax <- ctmax_20 %>% 
	mutate(pop2 = paste(genus_species, lat_long, sep = "_")) %>% 
	filter(pop2 %in% c(pd2$pop2)) %>% 
	ungroup() %>% 
	select(pop2, ctmax_20) %>% 
	rename(ctmax_pop2 = ctmax_20)
	
pd3 <- left_join(pd2, pop1_ctmax) %>% 
	left_join(., pop2_ctmax) 

pd4 <- pd3 %>% 
	filter(!is.na(ctmax_pop2)) %>% 
	filter(!is.na(ctmax_pop1)) %>% 
	mutate(diff_ctmax = abs(ctmax_pop1 - ctmax_pop2))

arr_slopes2 <- arr_slopes %>% 
	group_by(genus_species) %>% 
	summarise(mean_arr = mean(slope)) 

dispersals <- intratherm %>% 
	select(genus_species, dispersal_distance_category) %>% 
	distinct() 

pd5 <- left_join(pd4, arr_slopes2) %>% 
	left_join(., dispersals) 
# ctmax.diffspecies_i ~ ARRspecies_i + tenvmax.diff + dispersal_distance + spatial_distance + dispersal_distance:spatial_distance + ARR:tenvmax.dif, random=species.

mod <- lm(diff_ctmax ~ mean_arr + temp_difference + dispersal_distance_category  + mean_arr:temp_difference + realm_general2, data = pd5)
summary(mod)

library(visreg)

pd5 %>% View
	ggplot(aes(x = temp_difference, y = exp_temp_diff, color = realm_general2)) + geom_point() +
	ylab("Experienced temperature difference") + xlab("Environmental temperature difference")

pd5 %>% 
	ggplot(aes(x = distance, y = exp_temp_diff)) + geom_point() +
	scale_x_log10() +
	ylab("Experienced temperature difference") + xlab("Geographical distance")

pd5 %>% 
	ggplot(aes(x = distance, y = temp_difference)) + geom_point() +
	scale_x_log10() +
	ylab("Environmental temperature difference") + xlab("Geographical distance")


pd6 <- pd5 %>% 
	mutate(temp_difference = exp_temp_diff) %>% 
	select(diff_ctmax, mean_arr, temp_difference, dispersal_distance_category, distance, realm_general2) %>% 
	filter(dispersal_distance_category != "unk") %>% 
	mutate(dispersal_distance_category = as.numeric(factor(dispersal_distance_category, levels = c("0-1", "10-100", "100+")))) %>% 
	mutate(realm_general2 = as.numeric(as.factor(realm_general2)))

pd7 <- pd5 %>% 
	mutate(distance_km = distance / 1000) %>% 
	mutate(distance_km = ifelse(distance_km == 0, 1, distance_km)) %>% ## trying to avoid the issue of 0 distance, when elevations are different
	select(diff_ctmax, mean_arr, temp_difference, dispersal_distance_category, distance_km, realm_general2) %>% 
	filter(dispersal_distance_category != "unk") %>%
	mutate(distance_km = log(distance_km)) %>% 
	mutate(disperal_cat_numeric = case_when(dispersal_distance_category == "0-1" ~ "1",
											dispersal_distance_category == "1-10" ~ "10",
											dispersal_distance_category == "10-100" ~ "100",
											dispersal_distance_category == "100+" ~ "1000",
											TRUE ~ "other")) %>% 
	mutate(dispersal_cat_numeric = as.numeric(disperal_cat_numeric))

str(pd7)
unique(pd7$dispersal_distance_category)

mod <- lm(diff_ctmax ~ mean_arr + temp_difference + dispersal_distance_category +
		  	distance_km + dispersal_distance_category:distance_km + mean_arr:temp_difference, data = pd7)

mod <- lm(diff_ctmax ~ mean_arr + temp_difference + dispersal_cat_numeric +
		  	distance_km + log(dispersal_cat_numeric):distance_km + mean_arr:temp_difference, data = pd7)


mod <- lm(diff_ctmax ~ mean_arr + temp_difference + log(dispersal_cat_numeric) + mean_arr:temp_difference +
		  	log(dispersal_cat_numeric):distance_km:temp_difference, data = pd7)


summary(mod)


pd7 %>% 
	ggplot(aes(x = temp_difference)) + geom_histogram() +
	facet_wrap( ~ realm_general2) + xlab("Experienced temperature difference between populations") +
	ylab("Count")
ggsave("figures/experienced_temp_diffs_realm.png", width = 8, height = 6)


library(visreg)
summary(mod)
visreg(mod)


plot1 <- visreg(mod, "mean_arr", gg = TRUE, size = 4) +
	ylab("CTmax difference") + xlab("ARR") 

plot2 <- visreg(mod, "temp_difference", gg = TRUE, size = 4, by = "mean_arr") +
	ylab("CTmax difference") + xlab("Temperature difference") 

plot3 <- visreg(mod, "dispersal_cat_numeric", gg = TRUE, size = 4) +
	ylab("CTmax difference") + xlab("Dispersal distance") 

plot4 <- visreg(mod, "distance_km", gg = TRUE, size = 4, by = "dispersal_cat_numeric") +
	ylab("CTmax difference") + xlab("Geographical distance") 

# # plot5 <- visreg(mod, "realm_general2", gg = TRUE, size = 4) +
# 	ylab("CTmax difference") + xlab("Realm") 
plot6 <- visreg(mod, "distance_km", gg = TRUE, size = 4) +
	ylab("CTmax difference") + xlab("Geographical distance") 
plot7 <- visreg(mod, "temp_difference", gg = TRUE, size = 4) +
	ylab("CTmax difference") + xlab("Temperature difference") 



library(patchwork)
plot_all <- plot1 + plot2 + plot3 + plot6 + plot7 +
	plot_annotation(tag_levels = 'A') 


ggsave("figures/ctmax-partial-regressions-new_temps.png", plot = plot_all, width = 14, height = 10)



car::vif(mod)
library(car)
library(plyr)
?vif


cutoff=2
# Create function to sequentially drop the variable with the largest VIF until 
# all variables have VIF > cutoff
flag=TRUE
viftable=data.frame()
while(flag==TRUE) {
	vfit=vif(mod)
	viftable=rbind.fill(viftable,as.data.frame(t(vfit)))
	if(max(vfit)>cutoff) { mod=
		update(mod,as.formula(paste(".","~",".","-",names(which.max(vfit))))) }
	else { flag=FALSE } }
# Look at the final model
print(mod)
vif(mod)



pd5 %>% 
	ggplot(aes(x = diff_ctmax, y = temp_difference)) + geom_point() +
	ylab("Environmental temperature difference") + xlab("CTmax difference") 
ggsave("figures/env-temp-diff-ctmax-diff.png", width = 8, height = 6)


### ok try again now with new temperature data

temperatures <- read_csv("data-processed/initialize_pairwise_differences_experienced_and_topt.csv")
