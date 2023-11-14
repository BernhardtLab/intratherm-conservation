
library(cowplot)
library(tidyverse)
library(broom)

intratherm <- read_csv("data-processed/intratherm-cadillac-limits-traits.csv") %>% 
	mutate(population_id = paste(genus_species, latitude, sep = "_"))

temperatures <- read_csv("data-raw/IntTh.dailymax.AVGTEMP.csv")



temp_long <- temperatures %>% 
	tidyr::gather(key = day, value = temperature, 4:368) %>%
	group_by(latitude, longitude) %>% 
	summarise(sd_temperature = sd(temperature))

intratherm_temps <- left_join(intratherm, temp_long, by = c("latitude", "longitude")) %>% 
	select(sd_temperature, everything())

dim(temperatures)
intratherm %>%
	filter(parameter_tmax_or_tmin == "tmax") %>% 
	ggplot(aes(x = acclim_temp, y = parameter_value)) + geom_point()



intratherm %>% 
	filter(parameter_tmax_or_tmin == "tmax") %>% 
	group_by(population_id) %>% 
	do(tidy(lm(parameter_value ~ acclim_temp, data = .), conf.int = TRUE)) %>% View



intratherm %>% 
	filter(parameter_tmax_or_tmin == "tmax") %>% 
	group_by(population_id) %>% 
	do(tidy(lm(parameter_value ~ acclim_temp, data = .), conf.int = TRUE)) %>% View
