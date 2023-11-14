#Goals: 
#1 using all of the cadillac data, extract a value of "local differentiation" for each species
#2 using all of the cadillac data, extract a value of "acclimation response" for each species

library(tidyverse)
library(cowplot)
library(broom)

intratherm<-read_csv("data-processed/intratherm-cadillac-limits-traits.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))

intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>%
	ggplot(aes(x = acclim_temp, y = parameter_value, color = latitude)) + 
	geom_point() + 
	ylab("CTmax") + xlab("Acclimation")

model <- intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	group_by(population_id) %>% 
	do(tidy(lm(parameter_value~acclim_temp, data=.))) 

### fixed effects model


mod2 <- intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(!is.na(ramping_rate)) %>%
	# group_by(population_id) %>% 
	do(tidy(lm(parameter_value ~ acclim_temp*population_id + acclim_temp*ramping_rate, data=.))) 


main_effect_acclimation_temp <- mod2 %>% 
	filter(term == "acclim_temp") 





temperatures <- read_csv("data-raw/IntTh.dailymax.AVGTEMP.csv")

temp_long <- temperatures %>%
	gather(key = day, value = temperature, 4:368) %>%
	group_by(latitude, longitude) %>%
	summarise(sd_temperature = sd(temperature),
			  range_temperature = max(temperature)-min(temperature),
			  mean_temperature = mean(temperature),
			  interquantile_temp = quantile(temperature, probs=0.95)-quantile(temperature, probs=0.05))

intratherm_temps <- left_join(intratherm, temp_long, by = c("latitude", "longitude"))

#now ask how the slope (acclimation) is predicted by CV of temperature:
intratherm_acclimation <- model %>%
	filter(term=="acclim_temp") %>%
	left_join(.,  intratherm_temps, by = "population_id") %>% 
	rename(acclimation_slope=estimate, acclimation.sd=std.error) %>% 
	filter(!is.na(mean_temperature))

intratherm_acclimation %>%
	# filter(genus_species %in% c(two_populations$genus_species)) %>% 
	ungroup() %>% 
	ggplot(aes(x = sd_temperature, y = acclimation_slope, group = genus_species,
			   color = genus_species)) + 
	geom_point() + 
	geom_errorbar(aes(ymax = acclimation_slope + acclimation.sd,
					  ymin = acclimation_slope - acclimation.sd)) +
	geom_smooth(method=lm, se = FALSE) +
	theme(legend.position = "none") + facet_wrap( ~ realm_general3)



sd_slopes <- intratherm_acclimation %>% 
	distinct(population_id, acclimation_slope, .keep_all = TRUE) %>% 
	group_by(genus_species) %>% 
	do(tidy(lm(acclimation_slope ~ sd_temperature, data=.)))

sd_slopes %>% 
	filter(term == "sd_temperature") %>% 
	ggplot(aes(x = estimate)) + geom_density() + xlim(-3, 3) +geom_vline(xintercept = 0)
	

intratherm_acclimation %>% 
	group_by(population_id) %>% 
	mutate(acclimation_temp_range = max(acclim_temp) - min(acclim_temp)) %>%
	select(acclimation_temp_range, everything()) %>% 
	filter(acclimation_slope > 1) %>% View
	ggplot(aes(x = acclimation_slope, y = acclimation.sd)) + geom_point()
	distinct(population_id, acclimation_slope, .keep_all = TRUE) %>% 
	ggplot(aes(x = acclimation_slope)) + geom_density() + xlim(-3, 3) +geom_vline(xintercept = 0)
	

subset <- intratherm_acclimation %>% 
	filter(genus_species == "Agosia chrysogaster") %>% 
	distinct(acclimation_slope, .keep_all = TRUE) 

mod1 <- lm(acclimation_slope ~ sd_temperature, data = subset)
summary(mod1)

subset %>% 
	distinct(acclimation_slope, .keep_all = TRUE) %>% 
	select(genus_species, sd_temperature, everything()) %>% View
	ggplot(aes(x = sd_temperature, y = acclimation_slope)) + geom_point() +
	geom_smooth(method = "lm")

two_populations <- intratherm_acclimation %>%
	group_by(genus_species) %>%
	tally() %>% 
	filter(n == 2)


intratherm_acclimation %>%
	group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	ungroup() %>%
	do(glance(lm(acclimation_slope~interquantile_temp, data=.)))
#later ask how the differentiation (intercept) is predicted by differences in the temperatures:


	
#intercept diff
