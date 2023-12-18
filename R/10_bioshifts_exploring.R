### looking at bioshifts data

library(tidyverse)
library(janitor)


bioshifts <- read_csv("data-raw/bioshifts/Bioshifts/BioShifts.csv") %>% 
  clean_names()


bioshifts %>% 
  filter(gradient == "Latitudinal") %>%
  ggplot(aes(x = shift_r)) + geom_histogram()

### pulling in acclitherm data
acclitherm <- read_csv("data-processed/acclitherm_after-matching.csv")
acclitherm_or <- read_csv("data-processed/acclitherm.csv")

gpdd <- read_csv("data-processed/population-ts/gpdd_acclitherm-spp-with-temps.csv")

str(gpdd)
names(gpdd)

ps <- readRDS("~/Documents/too-big-for-github/population-time-series-with-temps-and-acclim-temps.rds")
ps3 <- readRDS("~/Documents/too-big-for-github/population-time-series-with-temps_thermal-data.rds")

ps2 <- bind_rows(ps)  

ps_slice <- ps3 %>% 
  filter(!is.na(abundance))

ps2_slice <- ps2 %>% 
  filter(!is.na(abundance)) 

write_csv(ps2_slice, "data-processed/modelled-ctmax-temps.csv")


unique(ps2_slice$genus_species) 

ps2_slice %>% 
  filter(genus_species == "Paralichthys lethostigma") %>% 
  ggplot(aes(x = date, y = abundance)) + geom_line()
