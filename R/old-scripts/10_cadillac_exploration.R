
library(tidyverse)
library(janitor)

#### cadillac dataset messing around

cadillac <- read_csv("data-processed/intratherm-multi-pop-multi-acclim.csv")
unique(cadillac$genus_species)

traits <- read_csv("data-raw/intratherm-traits.csv") %>% 
	clean_names() %>% 
	mutate(genus_species = paste(genus, species, sep = " "))

	
cadillac_locations <- cadillac %>% 
	select(genus_species, latitude, longitude) %>% 
	distinct(genus_species, latitude, longitude)

write_csv(cadillac_locations, "data-processed/intratherm_cadillac_locations.csv")


all_cadillac <- left_join(cadillac, traits, by = "genus_species") 
unique(all_cadillac$genus_species)

write_csv(all_cadillac, "data-processed/intratherm-cadillac-limits-traits.csv")
