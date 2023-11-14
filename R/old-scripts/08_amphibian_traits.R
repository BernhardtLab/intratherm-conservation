
### traits data

library(readxl)
library(janitor)

globetherm_traits <- read_csv("data-raw/globtherm_traits_collated_180617.csv") %>% 
	clean_names()


### amphibian data


amphib <- read_csv("data-raw/AmphiBIO_v1/AmphiBIO_v1.csv") %>% 
	clean_names()


species_list <- read_csv("data-processed/intratherm-species.csv") 

amphibian_traits <- amphib %>% 
	filter(species %in% c(species_list$genus_species)) %>% 
	select(1:5, 23:29)

write_csv(amphibian_traits, "data-processed/amphibian-traits.csv")
