
library(tidyverse)
library(broom)
library(janitor)
library(cowplot)
library(readxl)
library(stringr)


### New multipop data




intra <- read_csv("data-raw/Globtherm2_within_species_2018_12_17.csv") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "group")

so <- read_excel("data-raw/Globtherm2_within_species_SO.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "SO")

jb <- read_excel("data-raw/Globtherm2_within_species_JB.xlsx") %>% ### update Sept 2022 -- why isn't this file in the combined data below?
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "JB")

fl <- read_excel("data-raw/Globtherm2_within_species_FL.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "FL")

ab <- read_excel("data-raw/Globtherm2_within_species_AB.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "AB")

fv <- read_excel("data-raw/Globtherm2_FV_Test.xlsx") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = "_")) %>% 
	mutate_all(funs(as.character)) %>% 
	mutate(extractor = "FV")

all_mult <- bind_rows(so, ab, fl, intra, fv)
all_mult <- bind_rows(so, ab, fl, intra, fv, jb) ### update September 2022 to include the jb extracted data

write_csv(all_mult, "data-processed/team-intratherm-extracted.csv")


all_mult2 <- all_mult %>% 
	mutate(parameter_value = str_replace(parameter_value, "<", "")) %>% 
	mutate(parameter_value = as.numeric(parameter_value)) %>% 
	mutate(error_estimate = as.numeric(error_estimate)) %>% 
	mutate(lat_of_collection = as.numeric(lat_of_collection)) %>% 
	mutate(original_compilation = "intratherm_team") %>% 
	rename(latitude = lat_of_collection,
		   longitude = long_of_collection) %>% 
	mutate(latitude = as.numeric(latitude),
		   longitude = as.numeric(longitude)) %>% 
	rename(acclim_temp = pretreatment_temp) %>% 
	rename(acclim_time = pretreatment_duration) %>% 
	mutate(acclim_temp = as.numeric(acclim_temp)) %>% 
	mutate(ramping_rate = as.numeric(ramping_rate)) %>% 
	mutate(n_cat = NA) %>% 
	mutate(n_cat = ifelse(grepl(">", sample_size), sample_size, n_cat)) %>%
	mutate(n_cat = ifelse(grepl("<", sample_size), sample_size, n_cat)) %>%
	mutate(n_cat = ifelse(grepl("-", sample_size), sample_size, n_cat)) %>% 
	mutate(sample_size = ifelse(!grepl("[^0-9]", sample_size), sample_size, NA)) %>% 
	mutate(sample_size = as.numeric(sample_size))



### Update september 2022. We need to pull in all the reference info and DOIs for the globetherm data that are in intratherm

globe_refs <- read_excel("data-raw/globtherm-download-from-dryad/References_1_09_2017.xlsx")

all_mult3 <- left_join(all_mult2, globe_refs, by = c("ref" = "Data citation"))
write_csv(all_mult3, "data-processed/team-intratherm-extracted-2022.csv") ### ### update September 2022 to include the jb extracted data


unique(all_mult2$n_cat)

all_mult2 %>% 
	filter(sample_size == "15-Oct") %>% View


all_mult2 %>% 
	ggplot(aes(x = lat_of_collection, y = parameter_value, color = parameter_tmax_or_tmin)) + geom_point()



names(all_mult2)

rohr <- read_csv("data-processed/rohr_amphib_multi_pop.csv")

rohr2 <- rohr %>% 
	mutate(parameter_tmax_or_tmin = "tmax") %>% 
	rename(parameter_value = raw_ctm1) %>% 
	# select(genus1, species1, parameter_tmax_or_tmin, parameter_value, acclim_time, acclim_temp) %>% 
	rename(genus = genus1,
		   species = species1,
		   ramping_rate = heating_rate) %>% 
	mutate(original_compilation = "Rohr") %>% 
	mutate(acclim_time = as.character(acclim_time)) %>% 
	rename(life_stage = stage) %>% 
	rename(sample_size = n_numb) %>% 
	rename(realm_general = habitat) %>% 
	separate(record_species, into = c("record_number", "genus_species"), sep = "_") %>% 
	rename(metric_description = ep1_descrip) %>% 
	rename(ref = reference) %>% 
	rename(location_description = locality)



mult_species <- all_mult2 %>% 
	distinct(genus, species)

rohr_species <- rohr %>% 
	distinct(genus1, species1) %>% 
	rename(genus = genus1,
		   species = species1)

comte_species <- read_csv("data-processed/comte_fish_multi_pop.csv") %>% 
	clean_names() %>% 
	separate(species, into = c("genus", "species")) %>% 
	distinct(genus, species)


comte <- read_csv("data-processed/comte_fish_multi_pop.csv") %>% 
	clean_names() %>% 
	separate(species, into = c("genus", "species")) %>% 
	mutate(parameter_tmax_or_tmin = "tmax") %>% 
	rename(acclim_temp = temperature_of_acclimation_c) %>% 
	rename(acclim_time = length_acclimation_period_days,
		   ramping_rate = heating_rate_c_min) %>%
	# select(family, genus, species, latitude, longitude, thermal_limit_c,
	# 	   sd_thermal_limit, nindividuals, parameter_tmax_or_tmin,
	# 	   acclim_temp, acclim_time, acclimation, rate_acclimation) %>% 
	rename(parameter_value = thermal_limit_c) %>% 
	mutate(original_compilation = "Comte") %>% 
	mutate(acclim_time = as.character(acclim_time)) %>% 
	mutate(reference = as.character(reference)) %>% 
	rename(realm_general = realm_affinity) %>% 
	rename(sample_size = nindividuals) %>% 
	mutate(error_type = "SD") %>% 
	rename(error_estimate = sd_thermal_limit) %>% 
	rename(metric_description = endpoint) %>% 
	rename(ref = source) %>% 
	rename(location_description = location)
	



all_species <- bind_rows(mult_species, rohr_species, comte_species) %>% 
	distinct(genus, species)

write_csv(all_species, "data-processed/intratherm-species-list.csv")



# merge all the datasets --------------------------------------------------



combined_tmax <- bind_rows(all_mult2, rohr2, comte) %>% 
	filter(!is.na(parameter_value)) %>% 
	mutate(realm_general2 = case_when(realm_general == "aquatic" ~ "Aquatic",
									  realm_general == "marine" ~ "Marine",
									  realm_general == "marine littoral" ~ "Marine",
									  realm_general == "terrestrial" ~ "Terrestrial",
									  realm_general == "Arboreal" ~ "Terrestrial",
									  realm_general == "freshwater" ~ "Freshwater",
									  realm_general == "freshwater native" ~ "Freshwater",
									  TRUE ~ realm_general)) %>% 
	mutate(realm_general3 = case_when(realm_general2 %in% c("Aquatic", "Aquatic & terrestrial", "Freshwater", 'Marine') ~ "Aquatic",
									  realm_general2 == "Terrestrial" ~ "Terrestrial")) %>% 
	mutate(species = ifelse(species == "sylvatica", "sylvaticus", species)) %>% 
	mutate(genus_species = ifelse(is.na(genus_species), paste(genus, species, sep = " "), genus_species)) %>% 
	mutate(genus_species = str_replace(genus_species, "_", " ")) %>% 
	mutate(genus_species = str_replace(genus_species, "sylvatica", "sylvaticus"))

unique(combined_tmax$acclim_time)

comb_tmax2 <- combined_tmax %>% 
	mutate(acclim_time_original = acclim_time) %>% ## save a version of the acclim time as originally entered
	mutate(acclim_time_units = case_when(grepl("d", acclim_time) ~ "days",
										 grepl("w", acclim_time) ~ "weeks",
										 grepl("m", acclim_time) ~ "minutes",
										 grepl("h", acclim_time) ~ "hours",
										 TRUE ~ "days")) %>%
	mutate(acclim_time = str_replace(acclim_time, "overnight", "0.5")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "15-17d", "15")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "7-10d", "7")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "12-14", "12")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "10-14", "10")) %>%
	mutate(acclim_time = str_replace(acclim_time, "8-10w", "8")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "57-63d", "57")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "30-51d", "51")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "58-70d", "57")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "2-4m", "2")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "w", "")) %>% 
	mutate(acclim_time = str_replace(acclim_time, "h", "")) %>%
	mutate(acclim_time = str_replace(acclim_time, "d", "")) %>%
	mutate(acclim_time = str_replace(acclim_time, "[A-Za-z]", "")) %>% 
	mutate(acclim_time = as.numeric(acclim_time)) %>% 
	mutate(acclim_time = ifelse(acclim_time_units == "weeks", acclim_time*7, acclim_time)) %>% 
	mutate(acclim_time = ifelse(acclim_time_units == "hours", acclim_time/24, acclim_time)) %>% 
	mutate(acclim_time = ifelse(acclim_time_units == "minutes", acclim_time/1440, acclim_time)) %>% 
	select(genus_species, genus, species, latitude, longitude, location_description, realm_general3, acclim_temp, acclim_time, parameter_value, parameter_tmax_or_tmin, everything())
	

unique(comb_tmax2$acclim_time)

write_csv(comb_tmax2, "data-processed/combined-thermal-limits.csv") ### update September 8 2022. I think this is the most complete raw intratherm data

comb_tmax2 <- read_csv("data-processed/combined-thermal-limits.csv")

mult_pop_comb <- comb_tmax2 %>% 
	distinct(genus_species, latitude, longitude, elevation_of_collection, .keep_all = TRUE) %>% 
	group_by(genus_species) %>% 
	tally() %>% 
	filter(n > 1) %>% 
	select(genus_species)

mult_acclim_comb <- comb_tmax2 %>% 
	distinct(genus_species, acclim_temp, .keep_all = TRUE) %>% 
	group_by(genus_species) %>% 
	tally() %>% 
	filter(n > 1) %>% 
	select(genus_species)

cadillac <- data.frame(genus_species = c(intersect(mult_pop_comb$genus_species, mult_acclim_comb$genus_species)))
cadillac <- cadillac %>% 
	mutate(genus_species = as.character(genus_species))

extras_no_acclimation <- setdiff(combined3$genus_species, cadillac$genus_species)
extras_no_acclimation <- data.frame(genus_species = c(extras_no_acclimation))	

combined3 <- comb_tmax2 %>% 
	filter(genus_species %in% c(mult_pop_comb$genus_species)) %>% 
	select(genus_species, latitude, longitude, everything()) %>% 
	mutate(population_id = paste(genus_species, latitude, sep = "_")) %>% 
	select(genus_species, population_id, acclim_temp, everything())

write_csv(combined3, "data-processed/intratherm-multi-pop.csv")

combined5 <- comb_tmax2 %>% 
	filter(genus_species %in% c(mult_acclim_comb$genus_species)) %>% 
	select(genus_species, latitude, longitude, everything()) %>% 
	mutate(population_id = paste(genus_species, latitude, sep = "_")) %>% 
	select(genus_species, population_id, acclim_temp, everything())

write_csv(combined5, "data-processed/intratherm-multi-acclim.csv")


### now this has multiple acclimation temps
combined4 <- comb_tmax2 %>% 
	filter(genus_species %in% c(cadillac$genus_species)) %>% 
	select(genus_species, latitude, longitude, everything()) %>% 
	mutate(population_id = paste(genus_species, latitude, sep = "_")) %>% 
	select(genus_species, population_id, acclim_temp, everything())

write_csv(combined4, "data-processed/intratherm-multi-pop-multi-acclim.csv")

unique(combined4$genus_species)
unique(comb_tmax2$genus_species)
intratherm_species <- combined4 %>% 
	distinct(genus_species, .keep_all = TRUE) %>% 
	select(genus_species, phylum, class, order, family, life_stage)

intratherm_classes <- read_csv("data-processed/intratherm-classes.csv") %>% 
	filter(!is.na(class)) %>% 
	distinct(query, class) %>% 
	rename(genus_species = query)

intratherm_species2 <- left_join(intratherm_species, intratherm_classes)

write_csv(intratherm_species, "data-processed/intratherm-species.csv")

intratherm_species %>% 
	distinct(phylum) %>% View


intratherm_species %>% 
	filter(phylum == "Streptophyta") %>% View

locations <- combined_tmax %>% 
	select(genus_species, latitude, longitude) %>% 
	distinct(genus_species, latitude, longitude) %>% 
	filter(!is.na(latitude))

write_csv(locations, "data-processed/intratherm-locations.csv")


combined_tmax %>% 
	filter(is.na(latitude)) %>% View

combined_tmax %>% 
	filter(grepl("Bates", ref)) %>% View

# clean up combined dataset -----------------------------------------------


combined_tmax %>% 
	group_by(genus, species, metric_type, original_compilation, latitude, longitude, parameter_tmax_or_tmin) %>% 
	tally() %>% 
	filter(n >1) %>% View


combined_tmax %>% 
	ggplot(aes(x = latitude, y = parameter_value, color = realm_general3)) + geom_point() +
	ylab("Thermal limit (°C)") + xlab("Latitude") + facet_grid(realm_general3 ~ parameter_tmax_or_tmin)

# mult_pop_comb <- comb_tmax2 %>% 
# 	distinct(genus_species, latitude, longitude, elevation_of_collection, .keep_all = TRUE) %>% 
# 	group_by(genus_species) %>% 
# 	tally() %>% 
# 	filter(n > 1) %>% 
# 	select(genus_species)
# 
# many_pop_comb <- comb_tmax2 %>% 
# 	distinct(genus_species, latitude, longitude, elevation_of_collection, .keep_all = TRUE) %>% 
# 	group_by(genus_species) %>% 
# 	tally() %>% 
# 	filter(n > 2) %>% 
# 	select(genus_species)

# 
# combined_tmax %>% 
# 	filter(genus_species == "Ambystoma macrodactylum") %>% View

combined_tmax %>% 
	filter(parameter_tmax_or_tmin == "tmax") %>% 
	filter(genus_species %in% c(mult_pop_comb$genus_species)) %>% 
	ggplot(aes(x = latitude, y = parameter_value)) + geom_point() +
	ylab("Thermal limit (°C)") + xlab("Latitude") + facet_wrap( ~ genus_species, scales = "free")
ggsave("figures/all_lims.png", width = 49, height = 30, limitsize = FALSE)




combined3 %>% 
	filter(parameter_tmax_or_tmin == "tmax") %>% 
	ggplot(aes(x = latitude, y = parameter_value, color = realm_general3)) + geom_point() +
	ylab("Thermal limit (°C)") + xlab("Latitude")


combined3 %>% 
	# filter(parameter_tmax_or_tmin == "tmax") %>% 
	group_by(population_id) %>% 
	ggplot(aes(x = acclim_temp, y = parameter_value, color = latitude)) + geom_point() +
	ylab("Thermal limit (°C)") + xlab("Acclimation temperature") + facet_wrap( ~ parameter_tmax_or_tmin, scales = "free") +
	geom_smooth(method = "lm", se = FALSE) +
	theme(legend.position = "none")
	ggsave("figures/ARR-all.png", width = 10, height = 8)
	
	combined3 %>% 
		# filter(genus_species == "Lithobates berlandieri") %>% 
		group_by(population_id) %>% 
		filter(parameter_tmax_or_tmin == "tmax") %>% 
		filter(genus_species %in% c(many_pop_comb$genus_species)) %>% 
		filter(!is.na(acclim_temp)) %>% 
		mutate(lat2 = round(abs(latitude), digits = 0)) %>% 
		# filter(grepl("punctatus", genus_species)) %>% 
		ggplot(aes(x = acclim_temp, y = parameter_value, color = factor(lat2))) + geom_point(aes(color = factor(lat2))) +
		ylab("Thermal limit (°C)") + xlab("Acclimation temperature") +
		# facet_wrap( ~ genus_species, scales = "free") +
		geom_smooth(method = "lm", se = FALSE) +
		geom_point(aes(color = factor(lat2))) +
		# theme(legend.position = "none") +
		scale_color_viridis_d(name = "Absolute latitude", option = "magma", direction = -1, begin = 0.2)
	ggsave("figures/arr-many-pop-abs-lat-colour.png", width = 12, height = 8)


	combined3 %>%
		filter(parameter_tmax_or_tmin == "tmax") %>% 
		filter(genus_species %in% c(many_pop_comb$genus_species)) %>% 
		filter(!is.na(acclim_temp)) %>% 
		mutate(lat2 = round(abs(latitude), digits = 0)) %>% 
		group_by(population_id, latitude) %>% 
		do(tidy(lm(parameter_value ~ acclim_temp, data = .), conf.int = TRUE)) %>% 
		filter(term == "acclim_temp", estimate > 0) %>% 
		ggplot(aes(x = abs(latitude), y = estimate)) + geom_point() +
		ylim(0, 1.5) +ylab("ARR") + xlab("Absolute latitude")
	ggsave("figures/arr-latitude.png", width = 6, height = 4)
	
	
	
	
	combined3 %>% 
		filter(!is.na(acclim_temp)) %>%
		filter(parameter_tmax_or_tmin == "tmax") %>% 
		ggplot(aes(x = latitude, y = parameter_value, color = acclim_temp)) + geom_point() +
		ylab("Thermal limit (°C)") + xlab("Latitude") +
		facet_wrap( ~ parameter_tmax_or_tmin, scales = "free") +
		scale_color_viridis_c(name = "Acclimation temperature (°C)")
	ggsave("figures/ct-max-latitude.png", width = 10, height = 4)
	
	combined3 %>% 
		filter(is.na(latitude)) %>% View

	
combined3 %>% 
	# filter(is.na(acclim_temp)) %>% 
	filter(ref == "Fangue_&_Bennet_2003") %>% 
	select(extractor, everything()) %>% View

### ARR is the slope of the relationship between CTmax and acclimation temp, PRR is the slope of the 
### relationship between temperature at collection site and CTmax
