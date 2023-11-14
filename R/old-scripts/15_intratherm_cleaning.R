library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


### Data cleaning etc.

## this is the latest datafile sent by Nikki (April 30 2020)

intratherm <- read_csv("data-raw/intratherm-merged-nikkis-traits-clean.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_")) %>% 
	arrange(genus_species) %>% 
	mutate(intratherm_id = as.numeric(rownames(.))) %>% 
	select(intratherm_id, extractor, ref, original_compilation, genus_species, everything()) 

write_csv(intratherm, "data-processed/intratherm-may-2020.csv")


intratherm$intratherm_id <- rownames(intratherm)

names(intratherm)[grepl("stage", names(intratherm))] ## find the list of columns that have 'general'

unique(intratherm$dispersal_distance_category)



intratherm %>% 
	filter(is.na(longitude)) %>% View
	mutate(dispersal_distance_category = ifelse(dispersal_distance_category == "10-Jan", "1-10", dispersal_distance_category))

	
	
intratherm <- read_delim("data-raw/intratherm-may-2020-nikki.txt", delim = "\t")
intratherm <- read_csv("data-raw/intratherm-may-2020-nikki.csv") %>% 
	select(1:95)


### realm
intratherm %>% 
	# filter(is.na(realm_general2)) %>% ## 249 rows missing
	ggplot(aes(x = realm_general2)) + geom_histogram(stat = "count") +
	xlab("Realm")
ggsave("figures/intratherm-realm.png", width = 12, height = 6)
ggsave("figures/intratherm-realm.png", width = 8, height = 4)

### tmax tmin
intratherm %>% 
	filter(is.na(parameter_tmax_or_tmin)) %>% ## 0 missing
	ggplot(aes(x = parameter_tmax_or_tmin)) + geom_histogram(stat = "count")
ggsave("figures/intratherm-parameter-tmax-tmin.png", width = 12, height = 6)


### metric type
intratherm %>% 
	# filter(is.na(metric_type)) %>% no rows missing
	ggplot(aes(x = metric_type)) + geom_histogram(stat = "count")
ggsave("figures/metric_type.png", width = 12, height = 6)


### metric description
intratherm %>% 
	# filter(is.na(metric_description)) %>% ## 51 rows missing
	ggplot(aes(x = metric_description)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/metric_description.png", width = 12, height = 12)


### ramping rate
intratherm %>% 
	# filter(is.na(ramping_rate)) %>% ## 729 rows missing
	ggplot(aes(x = ramping_rate)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/ramping_rate.png", width = 12, height = 12)

### life stage
intratherm %>% 
	# filter(is.na(life_stage.x)) %>% ## 909 rows missing
	ggplot(aes(x = life_stage.x)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/life_stage-x.png", width = 12, height = 12)


### acclim temp -- there are some ranges in here, take mean?
intratherm %>% 
	# select(contains("acclim")) %>% View
	# filter(is.na(acclim_temp)) %>% ## 34 rows missing
	ggplot(aes(x = acclim_temp)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/acclim_temp.png", width = 20, height = 12)

### acclim time
intratherm %>% 
	# select(contains("acclim")) %>% View
	# filter(is.na(acclim_time)) %>% ## 1000 rows missing
	ggplot(aes(x = acclim_time)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/acclim_time.png", width = 12, height = 12)

unique(intratherm$acclim_time)

### age_maturity_days_female
intratherm %>% 
	# select(contains("age")) %>% View
	# filter(is.na(age_maturity_days_female)) %>% ## 0 rows missing
	ggplot(aes(x = age_maturity_days_female)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/age_maturity_days_female.png", width = 12, height = 12)


### age_maturity_days_male
intratherm %>% 
	# select(contains("age")) %>% View
	# filter(is.na(age_maturity_days_male)) %>% ## 0 rows missing
	ggplot(aes(x = age_maturity_days_male)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/age_maturity_days_male.png", width = 12, height = 12)

### dispersal -- Fix 10-jan here!
intratherm %>% 
	# select(contains("dispersal")) %>% View
	# filter(is.na(dispersal_distance_category)) %>% ## 0 rows missing
	ggplot(aes(x = dispersal_distance_category)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/dispersal_distance_category.png", width = 12, height = 12)
ggsave("figures/dispersal_distance_category.png", width = 6, height = 4)

### season inactive
intratherm %>% 
	# select(contains("dispersal")) %>% View
	# filter(is.na(season_inactive)) %>%  ## 0 rows missing
	ggplot(aes(x = season_inactive)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/season_inactive.png", width = 12, height = 12)

### migratory - fix one weird line contains migrate extensively
intratherm %>% 
	# select(contains("dispersal")) %>% View
	filter(!grepl("migrate extensively", migratory)) %>%   ## 0 rows missing
	ggplot(aes(x = migratory)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/migratory.png", width = 12, height = 12)


### lifespan
intratherm %>% 
	# select(contains("dispersal")) %>% View
	# filter(is.na(lifespan_days)) %>%   ## 0 rows missing
	ggplot(aes(x = lifespan_days)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/lifespan-days.png", width = 12, height = 12)


### latitude
intratherm %>% 
	# select(contains("dispersal")) %>% View
	# filter(is.na(latitude)) %>%   ## 91 rows missing
	ggplot(aes(x = latitude)) + geom_density() +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/latitude.png", width = 12, height = 12)

### latitude
intratherm %>% 
	# select(contains("dispersal")) %>% View
	# filter(is.na(longitude)) %>% ## 95 rows missing
	ggplot(aes(x = longitude)) + geom_density() +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/longitude.png", width = 12, height = 12)



### body size

intratherm %>% 
	# select(contains("maximum")) %>% View
	# filter(is.na(maximum_body_size_svl_hbl_cm)) %>% ## 0 rows missing
	ggplot(aes(x = maximum_body_size_svl_hbl_cm)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/body_size.png", width = 20, height = 12)


intratherm2 <- intratherm %>% 
	mutate(hatchery_origin = ifelse(grepl("hatchery", location_description), "hatchery_origin", "non-hatchery")) %>% 
	select(hatchery_origin, everything()) %>% View
