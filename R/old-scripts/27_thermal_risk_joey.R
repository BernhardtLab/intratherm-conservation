library(tidyverse)

##### estimating thermal risk
theme_set(theme_cowplot())
library(cowplot)

pops <- read_csv("~/Documents/too-big-for-github/population-dynamics-with-temp-ts.csv")


intervals <- pops %>% 
	filter(!is.na(abundance)) %>%
	group_by(population_id) %>% 
	mutate(time_interval = date - lag(date)) 


intervals %>% 
	filter(time_interval < 2.1) %>%
	filter(abundance > 0) %>% 
	ggplot(aes(x = time_interval)) + geom_histogram() + xlab("Time interval between sampling points")
ggsave("figures/time-invervals.png", width = 8, height = 6)


int2 <- intervals %>% 
	filter(time_interval == 1) %>% View
	filter(abundance > 0)

int3 <- head(int2, n = 1000)
	
int3 %>% 
	filter(grepl("bellare", population_id)) %>% 
ggplot(aes(x = date, y = abundance, group = population_id)) + geom_point() +
	facet_wrap( ~ population_id)


intratherm <- read_csv("data-processed/intratherm-with-elev.csv") %>%
	mutate(population_id = paste(population_id, longitude, sep = "_"))

multi_acc <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	tally() %>%   
	filter(n > 1)


pops <- bind_rows(pops) %>%
	mutate(genus_species = str_split_fixed(population_id, pattern = "_", n = 3)[,1]) %>%
	mutate(acclim_temp = temperature) %>%
	## mutate(acclim_temp = lag(temperature, n = 14)) %>%
	filter(!is.na(acclim_temp))


pops %>% 
	group_by(population_id) %>% 
	count() %>% View

tpops <- pops %>% 
	filter(grepl("Alosa pseudoharengus_45.9166666666667_-66.866", population_id)) %>%
	# filter(!is.na(abundance)) %>% 
	mutate(fake_temp = temperature + 30)


intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>%
	filter(grepl("Alosa pseudoharengus", population_id)) %>% View
