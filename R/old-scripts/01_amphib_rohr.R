

library(tidyverse)
library(janitor)
library(viridis)
library(cowplot)


amphib <- read_csv("data-raw/amphib-rohr.csv") %>% 
	clean_names()


am2 <- amphib %>% 
	unite(col = genus_species, sep = " ", remove = FALSE, genus3, species3) %>% 
	unite(col = record_species, sep = "_", remove = FALSE, record, genus_species) 

mult_pop <- am2 %>% 
	distinct(genus_species, latitude, longitude) %>% 
	group_by(genus_species) %>% 
	tally() %>%
	filter(n > 1) %>% 
	select(genus_species)



amphib_multi_pop <- am2 %>% 
	filter(genus_species %in% mult_pop$genus_species) 

write_csv(amphib_multi_pop, "data-processed/rohr_amphib_multi_pop.csv")


amphib_multi_pop <- read_csv("data-processed/rohr_amphib_multi_pop.csv")

amphib_multi_pop %>% 
	filter(genus_species == "Litoria rothii") %>% View
	ggplot(aes(x = abs_lat, y = raw_ctm1)) + geom_point() +
	geom_smooth(method = "lm", color = "black") +
	theme(legend.position = "none") + facet_wrap(~ genus_species, scales = "free") +
	ylab("CTmax") + xlab("Absolute latitude")
ggsave("figures/amphib_multipop.pdf", width = 14, height = 14)
