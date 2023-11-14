

### figuring out how many locations are still missing

library(tidyverse)
library(readxl)

locations <- read_xlsx("data-raw/missing-locations-intratherm.xlsx") %>% 
	mutate(longitude = ifelse(longitude ==  "NA", NA, longitude)) %>% 
	mutate(latitude = ifelse(latitude ==  "NA", NA, latitude))
	


locations %>% 
	filter(is.na(longitude)) %>%
	select(ref, location_description, everything()) %>% View


locations %>% 
	filter(is.na(longitude)) %>%
	select(ref, location_description, everything()) %>% 
	distinct(ref)



intratherm <- read_csv("data-processed/intratherm-cadillac-limits-traits.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))

unique(intratherm$genus_species)
intratherm_complete_locations <- intratherm %>% 
	filter(!is.na(longitude))

locations_new <- locations %>% 
	filter(!is.na(longitude))


n_loc_new <- names(locations_new)
n_intratherm <- names(intratherm)


intratherm2 <- bind_rows(intratherm, intratherm_complete_locations)

write_csv(intratherm2, "data-processed/intratherm-cadillac-limits-traits-location-updated.csv")


intratherm2 %>% 
	filter(is.na(acclim_temp)) %>% View


