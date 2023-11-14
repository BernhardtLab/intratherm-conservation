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
  