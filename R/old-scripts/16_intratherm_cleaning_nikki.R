## data cleaning ##
## load packages 
library(taxize)
library(tidyverse)

## note: taxize code first used on may 2020 version of database, again used on precleaning database
data <- read.csv("./data-raw/intratherm-may-2020-precleaning.csv")



## taxize all data so genus and species are correct
###################################################
## note on taxize synonyms: acceptedname with return value NA means name given was accepted name, do nothing
taxa <- data.frame(taxa = data$genus_species) ## create dataframe of names to check


syns <- unique(taxa)
tsn_search <- get_tsn(as.character(syns$taxa), accepted = FALSE) ## find tsn for each unique taxa
tsn_search <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/tsn_search.rds")
tsns <- data.frame(tsn_search)
tsns$taxa <- syns$taxa
syns <- tsns

found <- syns %>%
  subset(match == "found") 
  
report <- lapply(found$ids, itis_acceptname)
report_df <- data.frame(matrix(unlist(report), nrow=209, byrow=T),stringsAsFactors=FALSE)
report_df <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/report_df.rds")

found <- found %>%
  mutate(genus_species_corrected = report_df$X2)

## merge short unique list to long list of all taxa
merged_unique <- left_join(syns, found)
merged <- left_join(taxa, merged_unique)

  
## if names found are not accepted names, then change to accepted name
i = 1
while (i < length(merged$taxa)) {
  if (!is.na(merged$genus_species_corrected[i])) {
    merged$taxa[i] <- merged$genus_species_corrected[i]
  }
  i = i+1
}

## create new genus and species columns, correct original dataset
split <- str_split_fixed(merged$taxa, pattern = " ", n = 2)
merged$genus <- split[,1]
merged$species <- split[,2]

## update the database :)
data <- data %>%
  ungroup()%>%
  mutate(genus = merged$genus) %>%
  mutate(species = merged$species) %>%
  mutate(genus_species = merged$taxa) 




## taxize all data to update higher taxonomy columns 
####################################################
higher_tax <- tax_name(as.character(unique(data$genus_species)), get = c("phylum","class","order","family"))
higher_tax <- higher_tax[, -1]
colnames(higher_tax)[1] <- "genus_species"
higher_tax <- readRDS("~/Documents/SUNDAY LAB/Intratherm/Data sheets/higher_tax.rds")

data <- data %>%
  select(-phylum.x,-order,-order.x,-family,-family.x,-class,-class.x) %>% ## get rid of old taxonomy
  left_join(., higher_tax) %>% ## merge based on genus_species
  select(intratherm_id, genus_species, genus, species, phylum, class, 
         order, family, everything())  ## move columns around


## for all taxa where higher taxonomy not found: data filled in manually using a google search 




## change 'original_compilation' for entries by Nikki with field missing into intratherm_team 
#############################################################################################
data <- data %>%
  mutate(extractor) %>% 
  group_by(extractor) %>% 
  mutate(flag = grepl(extractor, pattern = "Nikki")) ## flag entries input by Nikki 

## if flagged as Nikki's and if original compilation is missing, write original compilation as intratherm_team

i = 1
while(i < length(data$original_compilation)) {
  if(data$flag[i] == TRUE && is.na(data$original_compilation[i])) {
    data$original_compilation[i] <- 'intratherm_team' 
 }
  i = i + 1
}

## remove flag
data <- data %>%
 select(-flag) 




## clean realm_general2
#######################
## categories allowed: freshwater, terrestrial and marine based on the adult lifestage 
## investigate NA
unique(data$realm_general2)

## flag entries with value Aquatic & terrestrial
data <- data %>%
  mutate(flag = grepl(realm_general2, pattern = "Aquatic & terrestrial")) 

  
## inspected all flagged, all are amphibians or missing, since spend adult life on land change to terrestrial 
## otherwise mark as missing taxonomy 
data_flagged <- data %>%
  filter(flag == TRUE) %>% 
  mutate(realm_general2 = ifelse(grepl(class, pattern = "Amphibia"), "Terrestrial", "missing higher taxonomy")) 

##change flagged data points to proper realm in database by removing flagged and reading 
data <- data %>%
  filter(flag == FALSE) %>%
  rbind(., data_flagged) %>%
  select(-flag)


unique(data$realm_general2)

## flag entries with value Aquatic
data <- data %>%
  mutate(flag = grepl(realm_general2, pattern = "Aquatic")) 

## inspected all flagged, either Amphibia or Teleosti
data_flagged <- data %>%
  filter(flag == TRUE) 

## change Amphibia to Terrestrial 
data_amphibs <- data_flagged %>%
  filter(class == "Amphibia") %>% 
  mutate(realm_general2 = "Terrestrial") 

data <- data %>%
  mutate(flag = ((grepl(realm_general2, pattern = "Aquatic")) & (grepl(class, pattern = "Amphibia")))) %>% ##flag only amphibians
  filter(flag == FALSE) 

data <- rbind(data, data_amphibs) %>% 
  select(-flag)


## reflag Aquatic and inspect
data <- data %>%
  mutate(flag = grepl(realm_general2, pattern = "Aquatic"))

data_flagged <- data %>%
  filter(flag == TRUE) 

## composed of family Cyprinidae and Zoarcidae, Cypr are freshwater and Zoarc are anadromous but spend most time in ocean 
unique(data_flagged$family)

## change Cyprinidae to freshwater 
data_cypr <- data_flagged %>%
  filter(family == "Cyprinidae") %>% 
  mutate(realm_general2 = "Freshwater") 

## remove Cpryn and reput with new realm
data <- data %>%
  mutate(flag = ((grepl(realm_general2, pattern = "Aquatic")) & (grepl(family, pattern = "Cyprinidae")))) %>% ##flag only Cypr
  filter(flag == FALSE) %>%
  rbind(., data_cypr) %>% 
  select(-flag)



## change Zoarcidae to marine 
data_zoar <- data_flagged %>%
  filter(family == "Zoarcidae") %>% 
  mutate(realm_general2 = "Marine") 

data <- data %>%
  mutate(flag = ((grepl(realm_general2, pattern = "Aquatic")) & (grepl(family, pattern = "Zoarcidae")))) %>% ##flag only Zoar with aquatic
  filter(flag == FALSE) %>% ## remove
  rbind(., data_zoar) %>%
  select(-flag)



## make sure all is well and no aquatic are left: 
data %>%
  subset(realm_general2 == "Aquatic") %>% View 


## change terrestrial to Terrestrial, marine to Marine and freshwater to Freshwater 
unique(data$realm_general2)

data <- data %>%
  mutate(is_f = ifelse(is.na(realm_general2), "FALSE", 
                       if_else(str_detect(realm_general2, "freshwater"), "TRUE", "FALSE"))) 

data_f <- data %>%
  filter(is_f == TRUE) %>%
  mutate(realm_general2 = "Freshwater")

data <- data %>%
  filter(is_f == FALSE) %>%
  rbind(., data_f) %>%
  select(-is_f)



unique(data$realm_general2)

data <- data %>%
  mutate(is_t = ifelse(is.na(realm_general2), "FALSE", 
                       if_else(str_detect(realm_general2, "terrestrial"), "TRUE", "FALSE"))) 

data_t <- data %>%
  filter(is_t == TRUE) %>%
  mutate(realm_general2 = "Terrestrial")

data <- data %>%
  filter(is_t == FALSE)%>%
  rbind(., data_t) %>%
  select(-is_t)


unique(data$realm_general2)

data <- data %>%
  mutate(is_m = ifelse(is.na(realm_general2), "FALSE", 
                       if_else(str_detect(realm_general2, "marine"), "TRUE", "FALSE"))) 

data_m <- data %>%
  filter(is_m == TRUE) %>%
  mutate(realm_general2 = "Marine")

data <- data %>%
  filter(is_m == FALSE)%>%
  rbind(., data_m) %>%
  select(-is_m)










## clean life_stage.x 
######################
unique(data$life_stage.x)

## fix adults
data <- data %>%
  mutate(is_adult = ifelse(is.na(life_stage.x), "FALSE", if_else(str_detect(life_stage.x, "juvenile|sub|Juvenile|Sub|juveniles"), 
                                                                 "FALSE", ifelse(str_detect(life_stage.x, "adult|Adult|adults"), 
                                                                                 "TRUE", "FALSE")))) 
data_adults <- data %>%
  filter(is_adult == TRUE) %>%
  mutate(life_stage.x = "Adult")

data <- data %>%
  filter(is_adult == FALSE) %>%
  rbind(., data_adults) %>%
  select(-is_adult)


unique(data$life_stage.x)

## fix juveniles
data <- data %>%
  mutate(is_juv = ifelse(is.na(life_stage.x), "FALSE", 
                         if_else(str_detect(life_stage.x, "sub-adult"), "TRUE", 
                         ifelse(str_detect(life_stage.x, "adult|Adult"), "FALSE", 
                         ifelse(str_detect(life_stage.x, "juvenile|Juvenile|sub-adult|fingerling|elver|fry"), "TRUE", "FALSE"))))) 
  

data_juv <- data %>%
  filter(is_juv == TRUE) %>%
  mutate(life_stage.x = "Juvenile")

data <- data %>%
  filter(is_juv == FALSE) %>%
  rbind(., data_juv) %>%
  select(-is_juv)


unique(data$life_stage.x)


## fix larvae
data <- data %>%
  mutate(is_larvae = ifelse(is.na(life_stage.x), "FALSE", 
                         if_else(str_detect(life_stage.x, "larva"), "TRUE", "FALSE"))) 

data_larvae <- data %>%
  filter(is_larvae == TRUE) %>%
  mutate(life_stage.x = "Larva")

data <- data %>%
  filter(is_larvae == FALSE)%>%
  rbind(., data_larvae) %>%
  select(-is_larvae)


unique(data$life_stage.x)


## fix juveniles and adults to "Juvenile and adult"
data <- data %>%
  mutate(is_mixed = ifelse(is.na(life_stage.x), "FALSE", 
                            if_else(str_detect(life_stage.x, "juveniles adult|juvenile and"), "TRUE", "FALSE"))) 

data_mixed <- data %>%
  filter(is_mixed == TRUE) %>%
  mutate(life_stage.x = "Juvenile and adult")

data <- data %>%
  filter(is_mixed == FALSE) %>%
  rbind(., data_mixed) %>%
  select(-is_mixed)

unique(data$life_stage.x)


## fix embryos
data <- data %>%
  mutate(is_emb = ifelse(is.na(life_stage.x), "FALSE", 
                           if_else(str_detect(life_stage.x, "embryo"), "TRUE", "FALSE"))) 

data_emb <- data %>%
  filter(is_emb == TRUE) %>%
  mutate(life_stage.x = "Embryo")

data <- data %>%
  filter(is_emb == FALSE) %>%
  rbind(., data_emb) %>%
  select(-is_emb)


unique(data$life_stage.x)


## fix unknown
data <- data %>%
  mutate(is_unk = ifelse(is.na(life_stage.x), "FALSE", 
                         if_else(str_detect(life_stage.x, "unknown"), "TRUE", "FALSE"))) 

data_unk <- data %>%
  filter(is_unk == TRUE) %>%
  mutate(life_stage.x = "Unknown")

data <- data %>%
  filter(is_unk == FALSE) %>%
  rbind(., data_unk) %>%
  select(-is_unk)


unique(data$life_stage.x)






## fix dispersal distance january 10 issue 
##########################################
unique(data$dispersal_distance_category)

data$dispersal_distance_category <- as.character(data$dispersal_distance_category)

data <- data %>%
  mutate(is_Jan10 = ifelse(is.na(dispersal_distance_category), "FALSE", 
                         ifelse(str_detect(dispersal_distance_category, "10-Jan"), "TRUE", "FALSE"))) 

data_jan10 <- data %>%
  filter(is_Jan10 == TRUE) %>%
  mutate(dispersal_distance_category = "1-10")

data <- data %>%
  filter(is_Jan10 == FALSE) %>%
  rbind(., data_jan10) %>%
  select(-is_Jan10)







## fix ranges in acclim_temp column by taking average of values
###############################################################
levels(data$acclim_temp)

data_sub <- data %>%
  mutate(has_hyphen = ifelse(str_detect(acclim_temp, "-"), "TRUE", "FALSE")) %>% ## detect ones with hyphen
  filter(has_hyphen == TRUE) ##subset to data with hyphen 

split <- str_split_fixed(data_sub$acclim_temp, pattern = "-", n = 2) %>%
  data.frame(.) %>%
  mutate(is_empty = ifelse(str_detect(X1, ""), (ifelse(str_detect(X2, ""), "FALSE", "TRUE")), "TRUE")) ## detect ones that are   empty to make sure no hypthens represented negative values 

data_sub$has_hyphen[which(split$is_empty == TRUE)] <- FALSE ##unflag ones with hyphen not surrounded by two things

split <- split %>% 
  filter(is_empty == FALSE) %>%
  mutate(avg = (as.numeric(as.character(X1)) + as.numeric(as.character(X2))) / 2) ## make new column representing average

data_sub <- data_sub %>%
  mutate(acclim_temp = ifelse((has_hyphen == TRUE), split$avg, as.numeric(as.character(data_sub$acclim_temp)))) %>%
  mutate(acclim_temp = as.factor(acclim_temp))

data <- data %>%
  mutate(has_hyphen = ifelse(str_detect(acclim_temp, "-"), "TRUE", "FALSE")) 

get_rid <- which(data$has_hyphen == TRUE)
data <- data[-get_rid,] ## remove rows where has_hyphen is TRUE

merged <- rbind(data, data_sub) ## add back hyphenated with new values 

data <- merged %>%
  select(-has_hyphen)





## clean elevation of collection
unique(data$elevation_of_collection)
## appears that some are ranges and some may be in ft vs m - fix later if needed 

## fix 20-May
data$elevation_of_collection <- as.character(data$elevation_of_collection)

data <- data %>%
  mutate(is_May20 = ifelse(is.na(elevation_of_collection), "FALSE", 
                           ifelse(str_detect(elevation_of_collection, "20-May"), "TRUE", "FALSE"))) 

data_may20 <- data %>%
  filter(is_May20 == TRUE) %>%
  mutate(elevation_of_collection = "5-20")

data <- data %>%
  filter(is_May20 == FALSE) %>%
  rbind(., data_may20) %>%
  select(-is_May20)





## update population id 
########################
## new population id: includes latitude AND elevation as unique identifier 
data <- data %>%
  mutate(population_id = paste(genus_species, latitude, elevation_of_collection, sep = "_")) 






## remove all non-wild hatchery containing rows in loc description 
##################################################################
##subset to hatchery data 
data_hatch <- data %>%
  mutate(is_hatchery = ifelse(str_detect(location_description, "hatchery|Hatchery"), "TRUE", "FALSE")) %>%
  filter(is_hatchery == TRUE) %>%
  select(-is_hatchery) %>%
  mutate(keep = ifelse(str_detect(location_description, "wild|Wild"), "TRUE", "FALSE")) %>%  ## flag wild populations as keep in subset
  filter(keep == TRUE) %>%
  select(-keep)

## remove all hatchery data from data, re-add ones flagged keep
hatchery <- which(str_detect(data$location_description, "hatchery|Hatchery"))
data <- data[-hatchery,] %>%
  rbind(., data_hatch)






## remove columns we do not need right now: 
bad_columns <- c("realm_general3", 
                 "realm_specific", 
                 "realm_general", 
                 "n_cat", 
                 "acclim_time_life_120", 
                 "log_acclim_time", 
                 "safety", 
                 "log_elevation", 
                 "elev_min", 
                 "iucn_2104risk", 
                 "rate_acclimation_c_day", 
                 "weight_g", 
                 "trophic_position", 
                 "source_logic_for_trophic_position", 
                 "dispersal_type_walking_may_need_to_be_reworded", 
                 "logic_source_for_dispersal_type",
                 "cold_season_dormancy",
                 "highland_specialist",
                 "source_logic_for_cold_season_dormancy", 
                 "source_logic_for_highland_specialist",
                 "no_acclimation", 
                 "life_stage",
                 "dispersal_distance2_category",
                 "logic_source_for_dispersal_distance2_category")

data <- data[, !(names(data) %in% bad_columns)]



## get rid of non-existent species Hyla alpina
##############################################
data <- data %>%
  filter(genus_species != "Hyla alpina")


######################################
## added all missing taxonomy manually
missing_tax <- data %>%
  filter(is.na(phylum)) 

data <- data %>% ##remove all rows missing phylums
  mutate(is_missing = ifelse(is.na(phylum), "TRUE", "FALSE")) %>%
  filter(is_missing == FALSE)

unique(missing_tax$genus_species)

planz <- missing_tax %>%
  filter(genus_species == "Planiliza subviridis") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Actinopterygii") %>%
  mutate(order = "Mugiliformes") %>%
  mutate(family = "Mugilidae") 

palir <- missing_tax %>%
  filter(genus_species == "Palirhoeus eatoni") %>%
  mutate(phylum = "Arthropoda") %>%
  mutate(class = "Insecta") %>%
  mutate(order = "Coleoptera") %>%
  mutate(family = "Curculionidae") 

taky <- missing_tax %>%
  filter(genus_species == "Takydromus hsuehshanensis") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Reptilia") %>%
  mutate(order = "Squamata") %>%
  mutate(family = "Laertidae") 

isch <- missing_tax %>%
  filter(genus_species == "Ischnura elegans") %>%
  mutate(phylum = "Arthropoda") %>%
  mutate(class = "Insecta") %>%
  mutate(order = "Odonata") %>%
  mutate(family = "Coenagrionidae") 

pleuro <- missing_tax %>%
  filter(genus_species == "Pleurodema thaul") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Amphibia") %>%
  mutate(order = "Anura") %>%
  mutate(family = "Leptodactylidae") 

rhyn <- missing_tax %>%
  filter(genus_species == "Rhynchocypris oxycephalus") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Actinopterygii") %>%
  mutate(order = "Cypriniformes") %>%
  mutate(family = "Cyprinidae") 

gymno <- missing_tax %>%
  filter(genus_species == "Gymnocephalus cernua") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Actinopterygii") %>%
  mutate(order = "Perciformes") %>%
  mutate(family = "Percidae") 

gloss <- missing_tax %>%
  filter(genus_species == "Glossina pallidipes") %>%
  mutate(phylum = "Arthropoda") %>%
  mutate(class = "Insecta") %>%
  mutate(order = "Diptera") %>%
  mutate(family = "Glossinidae") 
			
litho <- missing_tax %>%
  filter(genus_species == "Lithobates catesbeiana") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Amphibia") %>%
  mutate(order = "Anura") %>%
  mutate(family = "Ranidae") 

squa <- missing_tax %>%
  filter(genus_species == "Squalius cephalus") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Actinopterygii") %>%
  mutate(order = "Cypriniformes") %>%
  mutate(family = "Cyprinidae") 

lamp <- missing_tax %>%
  filter(genus_species == "Lampropholis coggeri") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Reptilia") %>%
  mutate(order = "Squamata") %>%
  mutate(family = "Scincidae") 

tem <- missing_tax %>%
  filter(genus_species == "Temnothorax curvispinosus") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Reptilia") %>%
  mutate(order = "Squamata") %>%
  mutate(family = "Scincidae") 

ranoid <- missing_tax %>%
  filter(genus_species == "Ranoidea caerulea") %>%
  mutate(phylum = "Chordata") %>%
  mutate(class = "Amphibia") %>%
  mutate(order = "Anura") %>%
  mutate(family = "Pelodryadidae") 

data <- data %>%
  select(-is_missing) %>%
  rbind(., planz, palir, taky, isch, pleuro, rhyn, 
              gymno, gloss, litho, squa, lamp, tem, ranoid) 
  




## make sure no missing realms for ones that had no higher taxonomy 
unique(data$realm_general2)

## change realm to terresrtrial since amphibian
## flag entries with value Aquatic
data <- data %>%
  mutate(flag = grepl(realm_general2, pattern = "missing higher taxonomy")) 

## inspected all flagged, either Amphibia or Teleosti
data_flagged <- data %>%
  filter(flag == TRUE)

## change Amphibia to Terrestrial 
data_amphibs <- data_flagged %>%
  mutate(realm_general2 = "Terrestrial") 

data <- data %>%
  filter(flag == FALSE)%>%
  rbind(., data_amphibs) %>%
  select(-flag)






## May 15: now need elevation data, so revisiting cleaning the column:
#####################################################################
## went back to studies to make sure elevation was in m and as reported in studies for all entries 
## if in ft, changed value in precleaning data and then reran code to update squeaky clean
unique(data$elevation_of_collection)


## fix ranges 
##############
data_sub <- data %>%
  mutate(has_hyphen = ifelse(str_detect(elevation_of_collection, "-"), "TRUE", "FALSE")) %>% ## detect ones with hyphen
  filter(has_hyphen == TRUE) ##subset to data with hyphen 

split <- str_split_fixed(data_sub$elevation_of_collection, pattern = "-", n = 2)%>%
  data.frame() %>% 
  mutate(avg = (as.numeric(as.character(X1)) + as.numeric(as.character(X2))) / 2) ## make new column representing average

data_sub <- data_sub %>%
  mutate(elevation_of_collection = ifelse((has_hyphen == TRUE), split$avg, as.numeric(as.character(data_sub$elevation_of_collection)))) %>%
  mutate(elevation_of_collection = as.factor(elevation_of_collection))

data <- data %>%
  mutate(has_hyphen = ifelse(str_detect(elevation_of_collection, "-"), "TRUE", "FALSE")) 

get_rid <- which(data$has_hyphen == TRUE)
data <- data[-get_rid,] ## remove rows where has_hyphen is TRUE

merged <- rbind(data, data_sub) ## add back hyphenated with new values 

data <- merged %>%
  select(-has_hyphen)






## May 18: 
## found a location labelled "fish farm" that should be removed since not wild 
## remove all farmed data from data
farm <- which(str_detect(data$location_description, "farm|Farm"))
data <- data[-farm,]




## while getting freshwater data, found some freshwater species living in estuaries that should be labelled marine 
## change realm_general2 to marine for these species 
intratherm_ids_marine <- c(313,321,766,1792,1797,1164,657,1240,1791,968,949,1228)
marine_sub <- data[-1:-2898,]

i <- 1
while (i < length(intratherm_ids_marine) + 1) {
  pop <- intratherm_ids_marine[i]
  row <- data[which(data$intratherm_id == pop),]
  
  if(length(row$latitude) == 0) {
    i = i + 1
  }
  
  else {
    ## find all population members - same lat, long, elev
    pop_members <- data %>%
      filter(latitude == row$latitude & longitude == row$longitude) 
    
    marine_sub <- rbind(marine_sub, pop_members) ## add to subset
    data <- data[!data$intratherm_id %in% pop_members$intratherm_id,] ## remove from dataset
    
    i <- i + 1
  }
}

marine_sub$realm_general2 <- "Marine" ##change general realm to marine 

data <- rbind(data, marine_sub)



## while getting marine data, found one species labelled marine that might have data in freshwater:
## change realm_general2 to freshwater for these species 
intratherm_ids_fresh <- c(2837)

fresh_sub <- data[-1:-2898,]

i <- 1
while (i < length(intratherm_ids_fresh) + 1) {
  pop <- intratherm_ids_fresh[i]
  row <- data[which(data$intratherm_id == pop),]
  
  if(length(row$latitude) == 0) {
    i = i + 1
  }
  
  else {
    ## find all population members - same lat, long, elev
    pop_members <- data %>%
      filter(latitude == row$latitude & longitude == row$longitude) 
    
    fresh_sub <- rbind(fresh_sub, pop_members) ## add to subset
    data <- data[!data$intratherm_id %in% pop_members$intratherm_id,] ## remove from dataset
    
    i <- i + 1
  }
}

fresh_sub$realm_general2 <- "Freshwater" ##change general realm to marine 
data <- rbind(data, fresh_sub)



## May 22:
## Madras Atomic Power Station Kalpakkam India and Central Institute of Fisheries Education Mumbai India not wild locations 
## remove all 
non_wild <- which(str_detect(data$location_description, "Education")) %>%
  append(which(str_detect(data$location_description, "Atomic")))

data <- data[-non_wild,]


##updating traits and clean references:
genus_col <- data$genus
species_col <- data$species

traits <- read.csv("./data-processed/intratherm-traits-clean-citations.csv") %>%
  dplyr::select(-dispersal_distance2_category, -logic_source_for_dispersal_distance2)
  
data <- data %>%
  ungroup() %>%
  select(-colnames(traits))%>%
  mutate(genus = genus_col, species = species_col)%>%
  left_join(., traits)


## cleaning up season_when_away100km+ and season_when_inactive
## change names of columns to simplify:
colnames(data)[which(names(data) == "season_when_away_100km...migratory.only.")] <- "season_when_away_100km"
colnames(data)[which(names(data) == "season_when_away_10km...migratory.only.")] <- "season_when_away_10km"

## clean up 100km:
swa <- data$season_when_away_100km
unique(swa)

swa <- str_replace(swa, pattern = "October/Novermber/December/January/February", replacement = "Oct-Feb") %>%
  str_replace(pattern = "spring-fall", replacement = "Spring-Fall") %>%
  str_replace(pattern = "fall/winter", replacement = "Fall-Winter") %>%
  str_replace(pattern = "March-April", replacement = "Mar-Apr") %>%
  str_replace(pattern = "August/September/October", replacement = "Aug-Oct") %>%
  str_replace(pattern = "summer", replacement = "Summer") %>%
  str_replace(pattern = "fall", replacement = "Fall") %>%
  str_replace(pattern = "autumn/winter", replacement = "Fall-Winter") %>%
  str_replace(pattern = "April-Aug/Sept", replacement = "Apr-Sep") %>%
  str_replace(pattern = "spring", replacement = "Spring") %>%
  str_replace(pattern = "late Spring/early Summer", replacement = "Spring-Summer") %>%
  str_replace(pattern = "3 months around April/May", replacement = "Apr-Jun") %>%
  str_replace(pattern = "late winter-early Spring", replacement = "Winter-Spring") 

unique(swa)

data$season_when_away_100km <- swa



## clean up 10km:
swa <- data$season_when_away_10km
unique(swa)

swa <- str_replace(swa, pattern = "spring-fall", replacement = "Spring-Fall") %>%
  str_replace(pattern = "winter(including Nov and Dec)", replacement = "Winter") %>%
  str_replace(pattern = "August/September/October", replacement = "Aug-Oct") %>%
  str_replace(pattern = "March-April", replacement = "Mar-Apr") %>%
  str_replace(pattern = "fall/winter", replacement = "Fall-Winter") %>%
  str_replace(pattern = "late spring/early summer", replacement = "Spring-Summer") %>%
  str_replace(pattern = "October/Novermber/December/January/February", replacement = "Oct-Feb") %>%
  str_replace(pattern = "spring", replacement = "Spring") %>%
  str_replace(pattern = "May-October", replacement = "May-Oct") %>%
  str_replace(pattern = "fall", replacement = "Fall") %>%
  str_replace(pattern = "winter", replacement = "Winter") %>%
  str_replace(pattern = "summer", replacement = "Summer") %>%
  str_replace(pattern = "autumn/winter", replacement = "Fall-Winter") %>%
  str_replace(pattern = "autumn/Winter", replacement = "Fall-Winter") %>%
  str_replace(pattern = "February/March/April", replacement = "Feb-Apr") %>%
  str_replace(pattern = "late winter-early spring", replacement = "Winter-Spring") %>%
  str_replace(pattern = "April-Aug/Sept", replacement = "Apr-Sep") %>%
  str_replace(pattern = "July-September", replacement = "Jul-Sep") %>%
  str_replace(pattern = "August/September/October/November/December/January/February/March/April", replacement = "Aug-Apr") %>%
  str_replace(pattern = "April/May/June", replacement = "Apr-Jun") %>%
  str_replace(pattern = "late Winter-early Spring", replacement = "Winter-Spring") %>%
  str_replace(pattern = "\\([^()]{0,}\\)", replacement = "") %>%
  str_replace(pattern = "3 months around April/May", replacement = "Apr-Jun") %>%
  str_replace(pattern = "Aug-Oct/November/December/January/Feb-Apr", replacement = "Aug-Apr") %>%
  str_replace(pattern = "Spring\\+ two months", replacement = "Spring-Summer")
  
unique(swa)
data$season_when_away_10km <- swa

## clean up season inactive:
## let dry == summer, hot == summer
sia <- data$season_inactive
unique(sia)

sia <- str_replace(sia, pattern = "hot dry", replacement = "Summer") %>%
  str_replace(pattern = "Oct - Mar", replacement = "Oct-Mar") %>%
  str_replace(pattern = "summer/dry season", replacement = "Summer") %>%
  str_replace(pattern = "winter", replacement = "Winter") %>%
  str_replace(pattern = "Oct - Apr", replacement = "Oct-Apr") %>%
  str_replace(pattern = "June - Oct", replacement = "Jun-Oct") %>%
  str_replace(pattern = "fall-winter", replacement = "Fall-Winter") %>%
  str_replace(pattern = "spring\\+ Winter", replacement = "Spring and Winter") %>%
  str_replace(pattern = "hot", replacement = "Summer") %>%
  str_replace(pattern = "dry", replacement = "Summer") %>%
  str_replace(pattern = "summer", replacement = "Summer") %>%
  str_replace(pattern = "winter\\+hot", replacement = "Winter and Summer") %>%
  str_replace(pattern = "winter\\+dry", replacement = "Winter and Summer") %>%
  str_replace(pattern = "fall\\-Winter", replacement = "Fall and Winter") %>%
  str_replace(pattern = "Winter\\+Summer", replacement = "Summer and Winter") %>%
  str_replace(pattern = "none ", replacement = "") %>%
  str_replace(pattern = "none", replacement = "") %>%
  str_replace(pattern = "Winter\\?", replacement = "Winter") 

unique (sia)
data$season_inactive <- sia


## found more non-wild fish including in descriptions "Central Institute of Fisheries Education Mumbai India" and "Madras Atomic Power Station Kalpakkam India"



## formatting references 
unique(data$ref)

## fix special characters
data <- data %>%
  mutate(ref = iconv(ref, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  mutate(ref = str_replace_all(ref, pattern = "\\^a\\^_\\^ao/oo ", replacement = "í")) %>%
  mutate(ref = str_replace_all(ref, pattern = "\\^a\\^_\\^A\\^0", replacement = "á")) %>%
  mutate(ref = str_replace_all(ref, pattern = "\\^a\\^_\\^a\\^'", replacement = "ö")) %>%
  mutate(ref = str_replace_all(ref, pattern = "\\^a\\^_\\^A", replacement = "ü"))



## investigate suspicious locations including word "Supplier"
##############################################################
supplier <- which(str_detect(data$location_description, "supplier")) %>%
  append(which(str_detect(data$location_description, "Supplier"))) %>%
  data[.,]

## notes: 
##      - Spina et al. does not mention whether population is originally wild - remove
##      - Geise & Linsenmair does not specify supplier or provide distinct location 
##      - Seibel, 1970 information on supplier mentioned cannot be found 
##      - Mahoney & Hutchison supplier has mixed populations 
##      - Sherman et al. mentions non-specific supplier 
##      - Brattstrom 1962, "presumably obtained in Wisconsin" 
##      - Maness & Hutchison, Zetts Fish Farm and Hatcheries has no info on whether wild population or not
##          but Rana berlandieri collected from Sinaloa Mexico
##          24.997770, -107.24825 - assumed loc
##      - Ritchart & Hutchison, information on supplier Kons Scientific Supply cannot be found
##      - Erskine & Hutchison mentions non-specific supplier 
## conclusion: only usable data is Rana berlandier from Maness & Hutchison 

## change loc of usable data in database:
data$location_description <- as.character(data$location_description)
data$location_description[which(data$intratherm_id == 1431)] <- "hatchery where wild population was collected from Sinaloa, Mexico"
data$latitude[which(data$intratherm_id == 1431)] <- 24.997770
data$longitude[which(data$intratherm_id == 1431)] <- -107.24825

## get rid of all mentioning supplier now:
data <- data %>%
  filter(!str_detect(location_description, "supplier")) %>%
  filter(!str_detect(location_description, "Supplier")) 

## write out to file
write.csv(data, "./data-processed/intratherm-may-2020-squeaky-clean.csv", row.names = FALSE)

## got temp data HERE
## ran elevation script HERE
## ran nocturnal script HERE