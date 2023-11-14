## for prepping trait database for publication 
## fixing citations and cleaning up NAs, unks, etc.
library(tidyverse)
library(dplyr)
library(taxize)

citations <- read.csv("./data-raw/intratherm-traits_references.csv") %>%
	.[1:13]

## edit the 'reference' of this document only - use the 'in_text' as a key to link to citations in above ^ 
key <- read.csv("./data-raw/traits_references-key.csv") %>%
	.[,1:3]


## combine with traits
traits <- read.csv("~/Documents/intratherm-traits_June-2020.csv") 

replace_imputed <- function(original, imputed){
	
	namestoChange <- namestoChange <- colnames(imputed)
	
	for(i in 1:length(namestoChange)){
		original[namestoChange[i]] <- imputed[namestoChange[i]]
	}
	return(original)
	
}

merged <- replace_imputed(traits, citations)
	
	
## make sure all 'unks' that have references can be removed 
traits %>%
	filter(age_maturity_days_female == "unk") %>%
	dplyr::select(genus, species, age_maturity_days_female, age_maturity_days_female_reference) %>% View

traits %>%
	filter(age_maturity_days_male == "unk") %>%
	dplyr::select(genus, species, age_maturity_days_male, age_maturity_days_male_reference) %>% View

traits %>%
	filter(dispersal_distance_category == "unk") %>%
	dplyr::select(genus, species, dispersal_distance_category, logic_source_for_dispersal_distance) %>% View

traits %>%
	filter(maximum_body_size_svl_hbl_cm == "unk") %>%
	dplyr::select(genus, species, maximum_body_size_svl_hbl_cm, source_for_maximum_body_size) %>% View

traits %>%
	filter(migratory == "unk") %>%
	dplyr::select(genus, species, migratory, source_logic_for_migration_info) %>% View

traits %>%
	filter(season_inactive == "unk") %>%
	dplyr::select(genus, species, season_inactive, logic_source_for_season_inactive) %>% View

traits %>%
	filter(is.na(season_when_away_10km...migratory.only.)) %>%
	filter(is.na(season_when_away_100km...migratory.only.)) %>%
	dplyr::select(genus, species, season_when_away_10km...migratory.only., 
				  season_when_away_100km...migratory.only.,
				  logic_source_season_when_away) %>% View

traits %>%
	filter(Home.range.size_Km2 == "unk") %>%
	dplyr::select(genus, species, Home.range.size_Km2, logic.source_for_Home.range.size) %>% View

traits %>%
	filter(lifespan_days == "unk") %>%
	dplyr::select(genus, species, lifespan_days, lifespan_days_reference) %>% View
## some contain averages - maybe keep? talk to JJJ


## getting rid of unnecessary references:
traits <- traits %>%
	mutate(age_maturity_days_female_reference = 
		   	ifelse(age_maturity_days_female == "unk", "", 
		   		   as.character(age_maturity_days_female_reference))) %>%
	mutate(age_maturity_days_male_reference = 
		   	ifelse(age_maturity_days_male == "unk", "",
		   		   as.character(age_maturity_days_male_reference))) %>%
	mutate(source_logic_for_migration_info = 
		   	ifelse(migratory == "unk", "",
		   		   as.character(source_logic_for_migration_info))) %>%
	mutate(logic_source_season_when_away = 
		   	ifelse(is.na(season_when_away_10km...migratory.only.) & 
		   		   	is.na(season_when_away_100km...migratory.only.), "",
		   		   as.character(logic_source_season_when_away))) %>%
	mutate(logic.source_for_Home.range.size = 
		   	ifelse(Home.range.size_Km2 == "unk", "",
		   		   as.character(logic.source_for_Home.range.size))) %>%
	mutate(logic_source_for_season_inactive = 
		   	ifelse(season_inactive == "unk", "",
		   		   as.character(logic_source_for_season_inactive))) %>%
	mutate(logic_source_for_dispersal_distance = 
		   	ifelse(dispersal_distance_category == "unk", "",
		   		   as.character(logic_source_for_dispersal_distance))) %>%
	mutate(lifespan_days_reference = 
		   	ifelse(lifespan_days == "unk", "",
		   		   as.character(lifespan_days_reference)))
	

merged <- merged %>%
	mutate(age_maturity_days_female_reference = 
		   	ifelse(age_maturity_days_female == "unk", "", 
		   		   as.character(age_maturity_days_female_reference))) %>%
	mutate(age_maturity_days_male_reference = 
		   	ifelse(age_maturity_days_male == "unk", "",
		   		   as.character(age_maturity_days_male_reference))) %>%
	mutate(source_logic_for_migration_info = 
		   	ifelse(migratory == "unk", "",
		   		   as.character(source_logic_for_migration_info))) %>%
	mutate(logic_source_season_when_away = 
		   	ifelse(is.na(season_when_away_10km...migratory.only.) & 
		   		   	is.na(season_when_away_100km...migratory.only.), "",
		   		   as.character(logic_source_season_when_away))) %>%
	mutate(logic.source_for_Home.range.size = 
		   	ifelse(Home.range.size_Km2 == "unk", "",
		   		   as.character(logic.source_for_Home.range.size))) %>%
	mutate(logic_source_for_season_inactive = 
		   	ifelse(season_inactive == "unk", "",
		   		   as.character(logic_source_for_season_inactive))) %>%
	mutate(logic_source_for_dispersal_distance = 
		   	ifelse(dispersal_distance_category == "unk", "",
		   		   as.character(logic_source_for_dispersal_distance))) %>%
	mutate(lifespan_days_reference = 
		   	ifelse(lifespan_days == "unk", "",
		   		   as.character(lifespan_days_reference)))


## make sure all 'kunks' have refs
traits %>%
	filter(Home.range.size_Km2 == "kunk") %>%
	dplyr::select(genus, species, Home.range.size_Km2, logic.source_for_Home.range.size) %>% View

traits %>%
	filter(lifespan_days == "kunk") %>%
	dplyr::select(genus, species, lifespan_days, lifespan_days_reference) %>% View

##remove "none" and "unk" values for season_inactive unless referenced 
merged <- merged %>%
	mutate(season_inactive = 
		   	ifelse(season_inactive == "none" & logic_source_for_season_inactive == "", "", 
		   		   as.character(season_inactive))) %>%
	mutate(season_inactive = 
		   	ifelse(season_inactive == "unk" & logic_source_for_season_inactive == "", "", 
		   		   as.character(season_inactive))) 


## run taxize to allow merging with squeaky clean data:
taxa <- data.frame(taxa = paste(merged$genus, merged$species)) ## create dataframe of names to check

syns <- unique(taxa)
tsn_search <- get_tsn(as.character(syns$taxa), accepted = FALSE) ## find tsn for each unique taxa
tsn_search <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/tsn_search_traits.rds")
tsns <- data.frame(tsn_search)
tsns$taxa <- syns$taxa
syns <- tsns

found <- syns %>%
	subset(match == "found") 

report <- lapply(found$ids, itis_acceptname)
report_df <- data.frame(matrix(unlist(report), nrow=269, byrow=T),stringsAsFactors=FALSE)
report_df <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/report_df_traits.rds")

found <- found %>%
	mutate(genus_species_corrected = report_df$X2)

## merge short unique list to long list of all taxa
merged_unique <- left_join(syns, found)
merged_unique <- left_join(taxa, merged_unique)
merged_unique$taxa <- as.character(merged_unique$taxa)

## if names found are not accepted names, then change to accepted name
i = 1
while (i < length(merged_unique$taxa)+1) {
	if (!is.na(merged_unique$genus_species_corrected[i])) {
		merged_unique$taxa[i] <- merged_unique$genus_species_corrected[i]
	}
	i = i+1
}

## create new genus and species columns, correct original dataset
split <- str_split_fixed(merged_unique$taxa, pattern = " ", n = 2)
merged_unique$genus <- split[,1]
merged_unique$species <- split[,2]

## update the database :)
merged <- merged %>%
	mutate(genus = merged_unique$genus) %>%
	mutate(species = merged_unique$species) 

## get rid of columns no longer accurate or necessary
merged <- merged %>%
	dplyr::select(-no_acclimation, -family, -order, -phylum, -class, -life_stage, -Notes) %>%
	.[-2,]

## get rid of duplicate species now 
merged <- unique(merged)


write.csv(merged, "./data-processed/intratherm-traits-clean-citations.csv", row.names = FALSE)
