library(tidyverse)
library(janitor)
library(viridis)
library(cowplot)


comte <- read_csv("data-raw/comte-all.csv")


length(unique(comte$Species))

mult_pop <- comte %>% 
	distinct(Species, Latitude, Longitude) %>% 
	group_by(Species) %>% 
	tally() %>%
	filter(n > 1) %>% 
	select(Species)


comte_multi_pop <- comte %>% 
	filter(Species %in% mult_pop$Species) 

write_csv(comte_multi_pop, "data-processed/comte_fish_multi_pop.csv")


c1 <- comte_multi_pop %>% 
	clean_names() %>% 
	mutate(abs_lat = abs(latitude)) %>% 
	filter(!is.na(latitude))

c1 %>% 
	ggplot(aes(x = latitude, y = thermal_limit_c, group = species)) + geom_point() +
	geom_smooth(method = "lm", color = "black") +
	theme(legend.position = "none") + 
	facet_wrap(~ species, scales = "free") +
	ylab("CTmax") + xlab("Latitude")
ggsave("figures/comte_latitude.pdf", width = 30, height = 14)

comte_clean <- comte %>% 
	clean_names() 


comte_clean %>% 
	filter(heating_rate_c_min < 5) %>% 
	ggplot(aes(x = heating_rate_c_min)) + geom_histogram() +
	xlab("Heating rate (°C per min)")
	

c2 <- comte_multi_pop %>% 
	clean_names() %>% 
	mutate(abs_lat = abs(latitude))


