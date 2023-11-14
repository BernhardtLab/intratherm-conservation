#Goals: 
#1 using all of the cadillac data, extract a value of "local differentiation" for each species
#2 using all of the cadillac data, extract a value of "acclimation response" for each species

library(tidyverse)
library(cowplot)
library(broom)
theme_set(theme_cowplot())

intratherm <- read_csv("data-processed/intratherm-cadillac-limits-traits-location-updated.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))

str_replace(column1, " ", "")

missing_location <- intratherm %>% 
	filter(is.na(longitude)) %>% 
	select(ref, genus_species, latitude, longitude) 

write_csv(missing_location, "data-processed/missing-locations-intratherm.csv")

intratherm %>% 
	filter(is.na(latitude)) %>% 
	distinct(genus_species) %>% tally()
	

intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>%
	group_by(population_id) %>%
	ggplot(aes(x = acclim_temp, y = parameter_value, color = population_id)) + 
	geom_point() + geom_line() + theme(legend.position="none") +
	facet_grid(~genus_species) +
	ylab("CTmax") + xlab("Acclimation")

model <- intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	group_by(population_id) %>% 
	do(tidy(lm(parameter_value~acclim_temp, data=.)))
View(model)

#read in temperature, summarize data per grid, and join with main dataframe
temperatures <- read_csv("data-raw/IntTh.dailymax.AVGTEMP.csv")

temp_long <- temperatures %>%
	gather(key = day, value = temperature, 4:368) %>%
	group_by(latitude, longitude) %>%
	summarise(sd_temperature = sd(temperature),
			  range_temperature = max(temperature)-min(temperature),
			  mean_temperature = mean(temperature),
			  interquantile_temp = quantile(temperature, probs=0.95)-quantile(temperature, probs=0.05))

intratherm_temps <- left_join(intratherm, temp_long, by = c("latitude", "longitude"))

#now ask how the slope (acclimation) is predicted by variation of temperature:
intratherm_acclimation <- model %>% filter(term=="acclim_temp") %>%
	left_join(.,  intratherm_temps, by = "population_id") %>% 
	rename(acclimation_slope=estimate, acclimation.sd=std.error)

#latitude
intratherm_acclimation %>%
	group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	ggplot(aes(x = abs(latitude), y = acclimation_slope, color = ramping_rate)) + 
	geom_point() + 
	geom_errorbar(aes(ymax=acclimation_slope+acclimation.sd, ymin=acclimation_slope-acclimation.sd)) +
	geom_smooth(method=lm)

#does latitude have higher sd?
intratherm_acclimation %>%
	group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	ggplot(aes(x = latitude, y = sd_temperature, color = abs(latitude))) + 
	geom_point()


#sd_temperature
intratherm_acclimation %>%
	group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	ggplot(aes(x = sd_temperature, y = acclimation_slope, color = abs(latitude))) + 
	geom_point() + 
	geom_errorbar(aes(ymax=acclimation_slope+acclimation.sd, ymin=acclimation_slope-acclimation.sd)) +
	geom_smooth(method=lm)


#interquantile_temp
intratherm_acclimation %>%
	group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	# filter(!is.na(ramping_rate)) %>% 
	mutate(ramping_category = case_when(ramping_rate >= 1 ~ "fast",
										ramping_rate < 1 ~ "slow")) %>% 
	group_by(realm_general3) %>% 
	ggplot(aes(x = sd_temperature, y = acclimation_slope, color = abs(latitude))) + 
	geom_point() + 
	geom_errorbar(aes(ymax=acclimation_slope+acclimation.sd, ymin=acclimation_slope-acclimation.sd)) +
	geom_smooth(method=lm, color = "black") + 
	# facet_grid (ramping_category ~ realm_general3) +
	ylim(0, 1) +
	ylab("Plasticity (ARR slope)") + xlab("SD of temperature") +
	scale_color_viridis_c()
ggsave("figures/plasticity-sd-temp.png", width = 8, height = 6)


intratherm_acclimation %>% 
group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	# filter(!is.na(ramping_rate)) %>% 
	mutate(ramping_category = case_when(ramping_rate >= 1 ~ "fast",
										ramping_rate < 1 ~ "slow")) %>% 
	ggplot(aes(x = sd_temperature, y = acclimation_slope, color = genus_species)) + 
	geom_point() + 
	geom_errorbar(aes(ymax=acclimation_slope+acclimation.sd, ymin=acclimation_slope-acclimation.sd)) +
	geom_smooth(method=lm, se = FALSE) + facet_grid (~ realm_general3) + ylim(0, 1) +
	theme(legend.position = "none")



names(intratherm_acclimation)

#fit a model
intratherm_acclimation %>%
	group_by(genus_species) %>%
	filter(acclimation_slope<25) %>%
	filter(acclimation_slope<1,
		   acclimation_slope>0) %>%
	ungroup() %>%
	do(tidy(lm(acclimation_slope~sd_temperature+ramping_rate, data=.)))
#later ask how the differentiation (intercept) is predicted by differences in the temperatures:



#Now ask how differentiated the populations are 
#by asking how different the heights of the curve are at some mediam acclimation temperature
#(i.e. differences in intercepts but not at zero)
intratherm_acclimation <- model %>% 
	filter(term=="acclim_temp") %>%
	left_join(.,  intratherm_temps, by = "population_id") %>% 
	rename(acclimation_slope=estimate, acclimation.sd=std.error)

library(modelr)

#make a new 
newdata <- intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species) %>% 
	summarise(median_acc=median(acclim_temp)) %>% 
	select(median_acc, genus_species)

View(newdata)
library(janitor)


models <- intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	do(tidy(lm(parameter_value ~ acclim_temp, data=.))) 

all_models <- left_join(models, newdata) %>% 
	select(genus_species, population_id, term, estimate, median_acc) %>% 
	spread(key = term, value = estimate) %>% 
	filter(!is.na(acclim_temp)) %>%
	clean_names() %>% 
	mutate(ctmax_at_medianT = median_acc*acclim_temp + intercept) %>% 
	rename(slope = acclim_temp)

all_models2 <- left_join(all_models, intratherm, by = c("population_id", "genus_species"))

all_models3 <- all_models2 %>% 
	group_by(genus_species) %>% 
	mutate(max_diff_ctmax = max(ctmax_at_medianT) - min(ctmax_at_medianT))

library(plotrix)

all_models3 %>% 
	group_by(realm_general3) %>% 
	summarise_each(funs(mean, std.error), max_diff_ctmax) %>% 
	ggplot(aes(x = realm_general3, y = mean)) + geom_point() + 
	geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.1, color = "purple") +
	geom_jitter(aes(x = realm_general3, y = max_diff_ctmax), alpha = 0.1, data = all_models3, width = 0.1) +
	geom_point(color = "purple") + ylab("Max difference in CTmax") + xlab("Realm")



data_frame <- intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	group_by(population_id) 



model_aug<-intratherm %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	group_by(population_id) %>% 
	do(augment(lm(parameter_value~acclim_temp, data=.), newdata=newdata$median_acc))
length(newdata$median_acc)
dim(model)
