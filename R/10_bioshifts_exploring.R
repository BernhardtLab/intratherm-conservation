### looking at bioshifts data


library(tidyverse)
library(janitor)


bioshifts <- read_csv("data-raw/bioshifts/Bioshifts/BioShifts.csv") %>% 
  clean_names()




bioshifts %>% 
  filter(gradient == "Latitudinal") %>%
  ggplot(aes(x = shift_r)) + geom_histogram()
  