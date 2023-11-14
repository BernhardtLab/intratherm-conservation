



library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(broom)


intratherm <- read_csv("data-processed/intratherm-cadillac-limits-traits-location-updated.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))


intratherm <- read_csv("data-raw/intratherm-merged-nikkis-traits.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))

model <- intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	group_by(population_id) %>% 
	do(tidy(lm(parameter_value~acclim_temp, data=.))) 

model %>% View

### fixed effects model


mod2 <- intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(!is.na(ramping_rate)) %>%
	# group_by(population_id) %>% 
	do(tidy(lm(parameter_value ~ acclim_temp*population_id + acclim_temp*ramping_rate, data=.))) 


View(mod2)

main_effect_acclimation_temp <- mod2 %>% 
	filter(term == "acclim_temp") 

intratherm %>% 
	group_by(genus_species) %>%
	filter(genus_species == "Cyclorana brevipes") %>% View
	count() %>% View
	filter(parameter_tmax_or_tmin=="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	filter(!is.na(ramping_rate)) %>%
	do(tidy(lm(parameter_value ~ population_id*acclim_temp, data=.))) %>% 
	View
	
	
intra2 <- intratherm %>% 
	mutate(metric_description = ifelse(is.na(metric_description), metric_type, metric_description))

intra3 <- intra2 %>% 
	group_by(genus_species, acclim_temp) %>%
	count() %>% 
	filter(n > 2) 

intra4 <- intra2 %>% 
	filter(genus_species %in% c(intra3$genus_species)) %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	filter(!is.na(ramping_rate)) %>%
	group_by(genus_species, acclim_temp) %>%
	count()

### ok still getting duplicates
first_cols <- names(intra2)[1:10]

intra5 <-  intra2 %>% 
	filter(genus_species %in% c(intra3$genus_species)) %>% 
	filter(genus_species %in% c(intra4$genus_species)) %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	# filter(!is.na(ramping_rate)) %>% 
	ungroup() %>% 
	distinct(.keep_all = TRUE) %>% 
	group_by(genus_species, acclim_temp, population_id) %>% 
	distinct(parameter_value, latitude, .keep_all = TRUE) %>% 
	count() %>% 
	filter(n>1)

### this doesn't make sense, because we don't need multiple pops within latitude
# intra6 <- intra2 %>% 
# 	filter(genus_species %in% c(intra5$genus_species)) %>% 
# 	group_by(genus_species, acclim_temp, latitude) %>% 
# 	count() %>% View
# 	filter(n > 2)

intra7 <- intra2 %>% 
	filter(genus_species %in% c(intra3$genus_species)) %>% 
	filter(genus_species %in% c(intra4$genus_species)) %>%
	filter(genus_species %in% c(intra5$genus_species)) %>% 
	# filter(genus_species %in% c(intra6$genus_species)) %>% View
	# filter(genus_species == "Ameiurus natalis") %>% 
	group_by(genus_species, acclim_temp) %>% 
	count() %>% 
	filter(n > 2) %>% 
	ungroup() %>% 
	mutate(genus_species = as.character(genus_species)) %>% 
	unite(genus_species, acclim_temp, col = genus_acclim, sep = "_", remove = FALSE) 

intra7 %>% 
	filter(genus_species == "Acris crepitans") %>% View


intra_comp <- intra2 %>% 
	unite(genus_species, acclim_temp, col = genus_acclim, sep = "_", remove = FALSE) %>% 
	filter(genus_acclim %in% c(unique(intra7$genus_acclim))) %>% 
	filter(!is.na(acclim_temp)) %>% 
	select(genus_species, population_id, acclim_temp, parameter_value, metric_description) %>% 
	filter(complete.cases(.)) %>% 
	ungroup() %>% 
	group_by(genus_species, population_id, metric_description) %>% 
	mutate(n = n_distinct(acclim_temp)) %>% 
	filter(n > 1) %>% 
	group_by(genus_species, acclim_temp, metric_description) %>% 
	mutate(n2 = n_distinct(population_id)) %>%  
	filter(n2 > 1)


estimates_pop <- intra_comp %>% 
	group_by(genus_species, metric_description) %>% 
	do(tidy(lm(parameter_value ~ population_id + acclim_temp, data=.))) %>% 
	filter(grepl("population", term)) 

estimates_pop %>% 
	ggplot(aes(x = p.value)) + geom_histogram() +
	geom_vline(xintercept = 0.05)
ggsave("figures/population_effect.pdf", width = 6, height = 4)
	
estimates_pop %>% 
	mutate(significant_pop = ifelse(p.value < 0.05, "yes", "no")) %>% 
	group_by(significant_pop) %>% 
	count()

mod3 <- intra_comp %>% 
		filter(genus_species == "Agosia chrysogaster") %>% 
		# filter(metric_description == "righting_ability") %>% 
		lm(parameter_value ~ population_id + acclim_temp, data = .) 

summary(mod3)
	
# fit over each set of groupings
fits <- intra_comp %>%
	group_by(., genus_species, acclim_temp) %>%
	nest() %>%
	mutate(fit = purrr::map(data, ~ lm(parameter_value ~ population_id, data = .x)))


summary(fits$fit[[1]])

info <- fits %>% View
	filter(!is.na(fit)) %>% 
	unnest(fit %>% map(glance))

# get params
params <- fits %>% 
	unnest(fit %>% map(tidy))
	

intra2 %>% 
	filter(genus_species == "Rhinichthys atratulus") %>% View

mod1 <- intra2 %>% 
	filter(genus_species == "Abudefduf saxatilis") %>% 
	distinct(parameter_value, latitude, .keep_all = TRUE) %>% 
	lm(parameter_value ~ population_id + acclim_temp, data=.))

library(visreg)

visreg(mod1)
summary(mod1)
