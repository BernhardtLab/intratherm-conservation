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
	group_by(population_id) %>%
	ggplot(aes(x = acclim_temp, y = parameter_value, color = population_id)) + 
	geom_point() + geom_line() + theme(legend.position="none") +
	facet_grid(~genus_species) +
	ylab("CTmax") + xlab("Acclimation")

model<-intratherm %>%
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
			  interquantile_temp = quantile(temperature, probs=0.95)-quantile(temperature, probs=0.05),
			  upper_quantile_temperature = quantile(temperature, probs=0.95))

intratherm_temps <- left_join(intratherm, temp_long, by = c("latitude", "longitude"))

#now ask how the slope (acclimation) is predicted by variation of temperature:
intratherm_acclimation<-model %>% filter(term=="acclim_temp") %>%
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
	ggplot(aes(x = sd_temperature, y = acclimation_slope, color = abs(latitude))) + 
	geom_point() + 
	geom_errorbar(aes(ymax=acclimation_slope+acclimation.sd, ymin=acclimation_slope-acclimation.sd)) +
	geom_smooth(method=lm) + facet_grid (ramping_category ~ realm_general3) + ylim(0, 1)


intratherm_acclimation %>% View
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
intratherm_acclimation<-model %>% 
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
	rename(slope=acclim_temp)

all_models2 <- left_join(all_models, intratherm, by=c("population_id", "genus_species"))

all_models3 <- all_models2 %>%
	group_by(population_id) %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	slice(1)

write_csv(all_models2, "data-processed/traits_differ_acclim_temp.csv")

library(geosphere)
distHaversine(c(0,0),c(90,90))
for (i in 1:length(unique(all_models2$genus_species))){
	each_species<-all_models3 %>% 
		filter(genus_species==unique(all_models2$genus_species)[4])
	c(dist(each_species$ctmax_at_medianT, method = "euclidean"))
	c(dist(each_species$latitude, each_species$longitude,  method = "euclidean")
#needs to go long then lat
#needs to not have any NAs
	c(distHaversine(c(each_species$longitude[1], each_species$latitude[1]), 
					c(each_species$longitude[2], each_species$latitude[2]),
					c(each_species$longitude[3], each_species$latitude[3]))) 
	  
	c(distHaversine(each_species$longitude[which(!is.na(each_species$longitude))], 
					each_species$latitude[which(!is.na(each_species$latitude))])

	  distHaversine(c(-100, -10), c(-100, -80), c(80, NA))
	  