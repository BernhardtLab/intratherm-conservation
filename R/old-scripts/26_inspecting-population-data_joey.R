### getting some info about populaton dynamics data:
library(tidyverse)

biotime <- read.csv("~/Documents/too-big-for-github/intratherm-biotime_nikki.csv")%>%
	arrange(STUDY_ID) ## studies have unique STUDY_ID
biotime_with_absence <- read.csv("~/Documents/too-big-for-github/intratherm-biotime-with-absence_nikki.csv") %>%
	arrange(STUDY_ID) ## studies have unique STUDY_ID
gpdd <- read_delim("~/Documents/too-big-for-github/intratherm-gpdd_nikki.txt", delim = ",") %>%
	arrange(MainID) ## studies have unique MainID
lpi <- read_csv("data-processed/lpi-intratherm-overlap_nikki.csv") %>%
	arrange(ID) ## studies have unique ID


biotime %>%
	mutate(pop_trend = paste(STUDY_ID, GENUS_SPECIES)) %>%
	ggplot(., aes(x = decimal_date, 
				  y = log(sum.allrawdata.ABUNDANCE),
				  col = pop_trend)) + 
	geom_line() +
	theme(legend.position = "none")

biotime %>%
	mutate(pop_trend = paste(STUDY_ID, GENUS_SPECIES)) %>%
	ggplot(., aes(x = decimal_date, 
				  y = log(sum.allrawdata.BIOMASS),
				  col = pop_trend)) + 
	geom_line() +
	theme(legend.position = "none")

biotime_with_absence %>%
	mutate(pop_trend = paste(STUDY_ID, GENUS_SPECIES)) %>%
	filter(sample_type == "abundance") %>%
	ggplot(., aes(x = decimal_date, 
				  y = log(sum.allrawdata.ABUNDANCE),
				  col = pop_trend)) + 
	geom_line() +
	theme(legend.position = "none")

str(biotime_with_absence)
unique(biotime$ABUNDANCE_TYPE)

biotime_with_absence %>%
	mutate(pop_trend = paste(STUDY_ID, GENUS_SPECIES)) %>%
	filter(sample_type == "biomass") %>%
	ggplot(., aes(x = decimal_date, 
				  y = log(sum.allrawdata.BIOMASS),
				  col = pop_trend)) + 
	geom_line() +
	theme(legend.position = "none")

gpdd %>%
	mutate(pop_trend = paste(MainID, TaxonName)) %>%
	ggplot(., aes(x = SampleYear, 
				  y = log(Population),
				  col = pop_trend)) + 
	geom_line() +
	theme(legend.position = "none")


lpi %>%
	mutate(pop_trend = paste(ID, Binomial)) %>%
	ggplot(., aes(x = year, 
				  y = log(abundance), col = pop_trend)) + 
	geom_line() +
	theme(legend.position = "none")



## see time series length
count_lpi <- lpi %>%
	filter(!is.na(abundance)) %>%
	count(ID, Binomial) %>%
	rename(study_id = ID, genus_species = Binomial) %>%
	mutate(source = "LPI")

count_gpdd <- gpdd %>%
	count(MainID, TaxonName) %>%
	rename(study_id = MainID, genus_species = TaxonName)%>%
	mutate(source = "GPDD")

count_biotime <- biotime_with_absence %>%
	count(STUDY_ID, GENUS_SPECIES) %>%
	rename(study_id = STUDY_ID, genus_species = GENUS_SPECIES) %>%
	mutate(source = "Biotime")

distribution <- rbind(count_lpi, count_gpdd, count_biotime)

ggplot(distribution, aes(x = )) + geom_histogram()
