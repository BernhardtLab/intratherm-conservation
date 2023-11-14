## getting diurnal/nocturnal traits for amphibians from AmphiBIO
library(tidyverse)
library(taxize)

## read in AmphiBIO
amphibio <- read.csv("./AmphiBIO_v1/AmphiBIO_v1.csv")

## subset intratherm to only amphibians 
amphibs <- read.csv("./data-processed/intratherm-with-elev.csv") %>%
	subset(class == "Amphibia") 

## get species list to look for 
amphib_species <- unique(amphibs$genus_species) %>%
	droplevels()

## how many are in the database? 
found <- amphib_species[which(amphib_species %in% amphibio$Species)] %>%
	droplevels()
length(found)
amphib_species <- amphib_species[-which(amphib_species %in% found)]
amphibs_notfound <- amphib_species[-which(amphib_species %in% found)]

## make a subset of amphibio to only species we have in intratherm and only noc trait:
amphibio <- amphibio %>%
	subset(Species %in% found) %>%
	select(Species, Noc) %>%
	rename(genus_species = Species) %>%
	mutate(is.nocturnal = ifelse(is.na(Noc), "N", "Y")) %>%
	select(-Noc)

## merge back to other amphibians 
amphib_species <- data.frame(amphib_species) %>%
	rename(genus_species = amphib_species) %>%
	mutate(is.nocturnal = as.character("NA"))

amphib_species <- rbind(amphibio, amphib_species)

amphibs <- left_join(amphibs, amphib_species) 

non_amphibs <- read.csv("./data-processed/intratherm-with-elev.csv") %>%
	subset(class != "Amphibia") 

non_amphibs$is.nocturnal <- as.character("NA")
all_species <- rbind(non_amphibs, amphibs)



## see if synonyms in amphibio:
amphibs <- read.csv("./data-processed/intratherm-with-elev.csv") %>%
	subset(class == "Amphibia") 
amphib_species <- unique(amphibs$genus_species) %>%
	droplevels()
amphibio <- read.csv("./AmphiBIO_v1/AmphiBIO_v1.csv")

potential_syn <- amphib_species[which(!amphib_species %in% amphibio$Species)] 

sub <- amphibs %>%
	filter(genus_species %in% potential_syn)

## potential synonyms:
syns <- unique(paste(sub$genus_from_study, sub$species_from_study, sep = " "))
found <- syns[which(syns %in% amphibio$Species)]

## make a subset of amphibio to only species we have in intratherm and only noc trait:
amphibio <- amphibio %>%
	subset(Species %in% found) %>%
	select(Species, Noc) 

split <- str_split_fixed(amphibio$Species, pattern = " ", n = 2)
amphibio <- amphibio %>%
	mutate(genus_from_study = split[,1]) %>%
	mutate(species_from_study = split[,2]) %>%
	mutate(genus_species = unique(sub$genus_species[which(sub$genus_from_study %in% split[,1] & sub$species_from_study %in% split[,2])]))

amphibio <- amphibio %>%
	mutate(is.nocturnal = ifelse(is.na(Noc), "N", "Y")) %>%
	select(-Noc,-Species, -genus_from_study, -species_from_study)

## merge back to other amphibians 
syns_only <- all_species %>%
	filter(genus_species %in% amphibio$genus_species) %>%
	select(-is.nocturnal)

syns_only <- left_join(syns_only, amphibio)

no_syns <- all_species %>%
	filter(!genus_species %in% amphibio$genus_species) 


## final data with added nocturnal column: 
all_species <- rbind(no_syns, syns_only)

## catch missed ones manually:
taxa <- c("Litoria aurea", "Litoria brevipes", "Litoria phyllochroa", "Litoria chloris", "Litoria lesueurii") ## create dataframe of names to check for synonyms we might have
taxa <- data.frame(taxa)

syns <- unique(taxa)
tsn_search <- get_tsn(as.character(syns$taxa), accepted = FALSE) ## find tsn for each unique taxa
tsns <- data.frame(tsn_search)
tsns$taxa <- syns$taxa
syns <- tsns

found <- syns %>%
	subset(match == "found") 

report <- lapply(found$ids, itis_acceptname)
report_df <- data.frame(matrix(unlist(report), nrow=5, byrow=T),stringsAsFactors=FALSE)

## manually change confirmed synonyms:
amphibio <- read.csv("./AmphiBIO_v1/AmphiBIO_v1.csv")
all_species$is.nocturnal[which(all_species$genus_species == "Ranoidea aurea")] = ifelse(is.na(amphibio$Noc[which(amphibio$Species == "Litoria aurea")]), "N", "Y")
all_species$is.nocturnal[which(all_species$genus_species == "Ranoidea brevipes")] = ifelse(is.na(amphibio$Noc[which(amphibio$Species == "Litoria brevipes")]), "N", "Y")
all_species$is.nocturnal[which(all_species$genus_species == "Ranoidea phyllochroa")] = ifelse(is.na(amphibio$Noc[which(amphibio$Species == "Litoria phyllochroa")]), "N", "Y")
all_species$is.nocturnal[which(all_species$genus_species == "Ranoidea chloris")] = ifelse(is.na(amphibio$Noc[which(amphibio$Species == "Litoria chloris")]), "N", "Y")
all_species$is.nocturnal[which(all_species$genus_species == "Ranoidea lesueurii")] = ifelse(is.na(amphibio$Noc[which(amphibio$Species == "Litoria lesueurii")]), "N", "Y")


## create template to fill in traits for through searching:
terrestrial <- all_species %>%
	filter(realm_general2 == "Terrestrial")

template <- terrestrial %>%
	select(genus_species, is.nocturnal) %>%
	unique()

template <- template %>%
	mutate(is.nocturnal_source = ifelse(is.nocturnal == "NA", "", "AmphiBIO"))

## write to fill to be filled out manually:
write.csv(template, "./intratherm_nocturnal-traits.csv", row.names = FALSE)


## import filled in file and merge back to intratherm:
nocturnal_traits <- read.csv("./data-raw/intratherm_nocturnal-traits-complete.csv")

terrestrial <- terrestrial %>%
	select(-is.nocturnal, -is.nocturnal_source)

merged <- left_join(terrestrial, nocturnal_traits)

non_terrestrial <- read.csv("./data-processed/intratherm-with-elev.csv") %>%
	filter(!realm_general2 == "Terrestrial") %>%
	mutate(is.nocturnal = "NA") %>%
	mutate(is.nocturnal_source = "NA")

all <- rbind(non_terrestrial, merged)


## write to file:
write.csv(all, "./data-processed/intratherm-with-elev.csv", row.names = FALSE)
