

### global population dynamics database

devtools::install_github("ropensci/rgpdd")


library(rgpdd)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


taxa <- gpdd_taxon
gdata <- gpdd_data
main <- gpdd_main
location <- gpdd_location
names(main)

setdiff(unique(gdata$MainID), unique(taxa$TaxonID))

length(unique(gdata$MainID))
length(unique(taxa$TaxonID))

# ggplot(dplyr::filter(gpdd_data, MainID %in% 1:10)) +
# 	geom_line(aes(SeriesStep, Population, col=MainID, group=MainID))


intratherm <- read_csv("data-processed/intratherm-cadillac-limits-traits-location-updated.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))
intratherm <- read_csv("data-raw/intratherm-merged-nikkis-traits.csv") %>%
	mutate(population_id = paste(genus_species, latitude, sep = "_"))


overlap <- intersect(unique(intratherm$genus_species), unique(unique(taxa$TaxonName)))
overlap
length(overlap)


overlap_intra <- intratherm %>% 
	filter(genus_species %in% overlap)

overlap_taxa <- taxa %>% 
	filter(TaxonName %in% overlap)

overlap_main <- main %>% 
	filter(TaxonID %in% overlap_taxa$TaxonID)

overlap_gdata <- gdata %>% 
	filter(MainID %in% overlap_main$MainID)





ol <- left_join(overlap_gdata, overlap_main) %>% 
	left_join(., overlap_taxa, by  = "TaxonID") %>% 
	left_join(., overlap_intra, by = c("TaxonName" = "genus_species")) %>% 
	left_join(., location)


names(ol)

ol %>% 
	mutate(unique_population = paste(TaxonName, MainID, sep = "_")) %>% 
	select(unique_population, everything()) %>% 
	ggplot(aes(x = SeriesStep, y = Population, group = unique_population, color = TaxonName)) + geom_line() +
	facet_wrap( ~ unique_population, scales = "free")
ggsave("figures/gpdd-taxon.pdf", width = 20, height = 12)
ggsave("figures/gpdd.png", width = 14, height = 12)

write_csv(ol, "data-processed/intratherm-gpdd.csv")

unique(ol$TaxonName)

sub <- ol %>% 
	filter(MainID == 1822) %>%
	select(location_description, ExactName, Country, everything()) %>% 
	rename(intratherm_location = location_description) %>% 
	rename(GPDD_location = ExactName)

unique(sub$location_description)


library(rlpi)
??rlpi

file.copy(from=system.file("extdata", "example_data.zip", package = "rlpi"), to=getwd())
unzip("example_data.zip")
Nearc_lpi <- LPIMain("example_data/terrestrial_class_nearctic_infile.txt", use_weightings = 1, VERBOSE=FALSE)


# Remove NAs (trailing years with no data)
Nearc_lpi <- Nearc_lpi[complete.cases(Nearc_lpi), ]
# This produces a simple plot, but we can use ggplot_lpi to produce a nicer version
ggplot_lpi(Nearc_lpi, ylims=c(0, 2))


Nearc_mams_lpi <- LPIMain("example_data/T_Nearctic_mammalia_infile.txt", VERBOSE=FALSE)

# Remove NAs (trailing years with no data)
Nearc_mams_lpi <- Nearc_mams_lpi[complete.cases(Nearc_mams_lpi), ]
# Nicer plot
ggplot_lpi(Nearc_mams_lpi, ylims=c(0, 2))


library(tidyverse)
lpi <- read_csv("data-raw/LPI_LPR2016data_public.csv") %>% 
	mutate(genus_species = str_replace(Binomial, "_", " "))

over <- intersect(unique(lpi$genus_species), unique(intratherm$genus_species))

lpi_over <- lpi %>% 
	filter(genus_species %in% over) %>% 
	gather(key = year, value = abundance, "1950":"2015") %>% 
	mutate(abundance = ifelse(abundance == "NULL", NA, abundance)) %>% 
	mutate(abundance = as.numeric(abundance)) %>% 
	mutate(year = as.numeric(year))

unique(lpi_over$genus_species)
write_csv(lpi_over, "data-processed/lpi-intratherm-overlap.csv")


lpi_over %>%  
	filter(Binomial == "Alosa_pseudoharengus") %>%
	ggplot(aes(x = year, y = abundance, group = ID)) + geom_line() +
	facet_wrap( ~ Location)
ggsave("figures/lpi-examples-alewife.pdf", width = 10, height = 10)
ggsave("figures/lpi-examples-alewife.png", width = 10, height = 10)

alewife_locations_intra <- intratherm %>% 
	filter(genus_species == "Alosa pseudoharengus")

alewife_locations_intra2 <- unique(alewife_locations_intra$location_description)
alewife_locations_lpi <- lpi_over %>%  
	filter(Binomial == "Alosa_pseudoharengus")

alewife_locations_lpi2 <- unique(alewife_locations_lpi$Location)


### now trying compadre


library(Rcompadre)


str(comadre)

unique(comadre$SpeciesAccepted)

over_com <- intersect(unique(comadre$SpeciesAccepted), unique(intratherm$genus_species))

over_com


trout <- subset(comadre, SpeciesAccepted == "Oncorhynchus clarkii")


trout

library(rredlist)
rl_sp_citation('Oncorhynchus clarkii')

rl_search("Oncorhynchus clarkii")
rredlist::rl_use_iucn()
rredlist::rl_use_iucn()


iucn_species <- read_csv("~/Documents/redlist_species_data_d/assessments.csv")



over_iucn <- intersect(unique(iucn_species$scientificName), unique(intratherm$genus_species))
length(unique(over_iucn))
length(unique(intratherm$genus_species))

iucn_species_overlap <- iucn_species %>% 
	filter(scientificName %in% c(unique(intratherm$genus_species)))

write_csv(iucn_species_overlap, "data-processed/intratherm-species-redlist.csv")
	
iucn_species_overlap %>% 
	ggplot(aes(x = redlistCategory)) + geom_histogram(stat = "count", fill = "cadetblue") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/redlist-categories.pdf", width = 6, height = 4)
ggsave("figures/redlist-categories.png", width = 8, height = 8)

iucn_species %>% 
	filter(scientificName %in% c(unique(intratherm$genus_species))) %>% 
	ggplot(aes(x = populationTrend)) + geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 90))
ggsave("figures/redlist-population-trends.png", width = 8, height = 8)
