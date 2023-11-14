

library(tidyverse)
library(broom)
library(janitor)
library(cowplot)

rohr <- read_csv("data-processed/rohr_amphib_multi_pop.csv")
comte <- read_csv("data-processed/comte_fish_multi_pop.csv") %>% 
	clean_names()

rohr3 <- rohr %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) 

rohr3 %>% 
	ggplot(aes(x = bioclim5, y = raw_ctm1, color = genus_species)) + 
	theme(legend.position = "none") + geom_smooth(method = "lm", se = FALSE) +
	ylab("CTmax") + xlab("Habitat temperature")

names(rohr3)

#model PRR with acclimation and methodology
PRR_model<-rohr3 %>%
	group_by(genus_species) %>%
	filter(max(bioclim5)-min(bioclim5)>5) %>%
	ungroup() %>%
	do(tidy(lm(raw_ctm1 ~ bioclim5*genus_species+acclim_temp*log_acclim_time +
			   	log((raw_ctm1 - acclim_temp)/heating_rate), data = .)))

bioclim_slope<-filter(PRR_model, term=="bioclim5")$estimate

PRR_model %>% 
	filter(grepl("bioclim5:", term)) %>% 
	ggplot(aes(x = estimate + bioclim_slope)) + geom_histogram() 
		   

#model ARR with environmental temp and methodology
ARR_model<-rohr3 %>%
	group_by(genus_species) %>%
	filter(max(acclim_temp)-min(acclim_temp)>5) %>%
	ungroup() %>%
	do(tidy(lm(raw_ctm1 ~ acclim_temp*genus_species + bioclim5 , data = .)))

bioclim_slope<-filter(PRR_model, term=="bioclim5")$estimate

PRR_model %>% 
	filter(grepl("bioclim5:", term)) %>% 
	ggplot(aes(x = estimate + bioclim_slope)) + geom_histogram() 




