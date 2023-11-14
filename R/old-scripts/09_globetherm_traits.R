

### globetherm traits

library(tidyverse)
library(janitor)

gtraits <- read_csv("data-raw/globtherm_traits_collated_180617.csv") %>% 
	clean_names()


globetherm_species <- gtraits %>% 
	unite(genus_species, genus, species, sep = " ") %>% 
	distinct(genus_species) %>% 
	filter(!is.na(genus_species))


intratherm_species <- read_csv("data-processed/intratherm-species.csv") 

gspecies <- globetherm_species$genus_species
ispecies <- intratherm$genus_species

overlapping_species <- intersect(gspecies, ispecies)
non_overlapping_species <- setdiff(ispecies, gspecies)



gtraits2 <- gtraits %>% 
	mutate(genus_species = paste(genus, species, sep = " ")) 

intratherm_traits <- left_join(intratherm_species, gtraits2)

intratherm_traits2 <- left_join(intratherm_traits, ages2) %>% 
	arrange(class) %>% 
	select(genus_species, everything()) %>% 
	select(-genus) %>% 
	select(-species) %>% 
	separate(genus_species, into = c("genus", "species")) %>% 
	select(1:7, female_maturity_days, everything())

write_csv(intratherm_traits, "data-processed/intratherm_traits.csv")
write_excel_csv(intratherm_traits2, "data-processed/intratherm_traits.csv")

intratherm1 <- read_csv("data-processed/intratherm_traits.csv") %>% 
	mutate(genus_species = paste(genus, species, sep = " "))
unique(intratherm1$genus_species)

intratherm2 <- read_csv("data-processed/intratherm-cadillac-limits-traits-location-updated.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))

unique(intratherm2$genus_species)

library(fuzzyjoin)

fjoined <- gspecies %>%
	stringdist_inner_join(ispecies, by = "genus_species", max_dist = 1, distance_col = "distance")
		   

fjoined <- stringdist_left_join(intratherm, globetherm_species, by = c(genus_species = "genus_species"), max_dist = 2)



ispecies_sub <- ispecies[3]

library(taxize)


classification(ispecies_sub, db = 'itis')
classes <- tax_name(query = ispecies, get = "class", db = "itis")
classes2 <- tax_name(query = ispecies, get = "class", db = "both")

write_csv(classes2, "data-processed/intratherm-classes.csv")


### figure out which range data

ranges <- read_csv("data-raw/globtherm_range_list.csv") %>% 
	clean_names() %>% 
	mutate(range_status = ifelse(action %in% c("remove", NA), "range data missing", "we have range data"))


ranges2 <- ranges %>% 
	select(species_name_queried_gbif, range_status) %>% 
	filter(!is.na(species_name_queried_gbif))


ranges3 <- left_join(intratherm, ranges2, by = c("genus_species" = "species_name_queried_gbif"))

write_csv(ranges3, "data-processed/range-status-intratherm.csv")


ranges3 %>% 
	group_by(range_status) %>% tally()



ages <- read_excel("data-raw/sp_age.xlsx")

ages2 <- ages %>% 
	mutate(species_names = str_replace(species_names, "_", " ")) %>% 
	rename(genus_species = species_names) %>% 
	clean_names() %>% 
	select(genus_species, female_maturity_days)
