

remotes::install_github("ropensci/rfishbase")
library(rfishbase)

trout <- common_to_sci("trout")
dat <- species(trout$Species, fields=c("Species", "PriceCateg", "Vulnerability"))


species(trout$Species) %>% View


list_fields("Age")
maturity("Oreochromis niloticus") %>% View
