### getting phenotypic plasticity


library(tidyverse)
library(cowplot)


rohr <- read_csv("data-processed/rohr_amphib_multi_pop.csv")


rohr3 <- rohr %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) 


cv <- rohr3 %>% 
	group_by(genus_species, lat_long) %>% 
	summarise_each(funs(sd, mean, var),raw_ctm1) %>% 
	filter(!is.na(raw_ctm1_sd)) %>% 
	mutate(cv = raw_ctm1_sd/raw_ctm1_mean) 

rcv <- left_join(rohr3, cv, by = c("lat_long", "genus_species"))

rcv %>% 
	ggplot(aes(x = latitude, y = cv)) + geom_point() +
	ylab("Coefficient of variation") + xlab("Latitude")
ggsave("figures/cv_amphibian.pdf", width = 6, height = 5)

rcv %>% 
	ggplot(aes(x = abs(latitude), y = cv)) + geom_point() +
	ylab("Coefficient of variation") + xlab("Absolute latitude")
ggsave("figures/cv_amphibian.pdf", width = 6, height = 5)

rcv %>% 
	ggplot(aes(x = latitude, y = raw_ctm1_var)) + geom_point() +
	ylab("Variance") + xlab("Latitude")
ggsave("figures/variance_amphibian.pdf", width = 6, height = 5)
