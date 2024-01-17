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

path_gdb <- "tmp/Bioshifts/Study_Areas.gdb"
bio <- read_csv("tmp/Bioshifts/BioShifts.csv")
ref <- read_csv("tmp/Bioshifts/References.csv")
lay <- sf::st_layers("tmp/Bioshifts/Study_Areas.gdb")


bioshifts_species <- bioshifts %>% 
  select(species) %>% 
  distinct()


acclitherm_species <- acclitherm_or %>% 
  select(genus_species) %>% 
  distinct()

acclitherm_species %>% 
  left_join(bioshifts_species, by = c("genus_species" = "species")) %>% View

intersect(bioshifts_species$species, acclitherm_species$genus_species) ### 57 species in BioShifts and acclitherm

b2 <- bioshifts %>% 
  filter(species %in% c(acclitherm_species$genus_species))


### ok let's get out the files 
berk <- rast("~/Documents/too-big-for-github/temperature-data/Complete_TAVG_Daily_LatLong1_2020.nc")
berk1 <- rast("~/Documents/too-big-for-github/temperature-data/Complete_TAVG_Daily_LatLong1_2000.nc")
filename <- "~/Documents/too-big-for-github/temperature-data/Complete_TAVG_Daily_LatLong1_2000.nc"

library(ncdf4)
thing <- nc_open(filename)

date <- ncvar_get(thing, "date_number")
arr.anom <-ncvar_get(thing, "temperature")
arr.clim <- ncvar_get(thing, "climatology")
dim(arr.anom) ### ok so this is a leap year thing, I think!
dim(arr.clim)
dim(date)

df <- data.frame(date = date, anomaly = arr.anom, climatology = arr.clim)

ncvar_get()







# terrestrial temp extraction ---------------------------------------------

#########################################
##      GETTING TERRESTRIAL TEMPS      ##
#########################################
temperature_data <- data.frame(matrix(nrow = 32873))
unique_pairs <- all_locs %>%
  filter(realm_of_population == "Terrestrial") %>%
  select(latitude, longitude, temp_id) %>%
  unique()

unique_loc <- 1
while (unique_loc < nrow(unique_pairs) + 1) {
  
  loc_cumulative <- data.frame(matrix(ncol=2))
  colnames(loc_cumulative) <- c("date", "temp_value")
  rep = 1930
  
  while (rep < 2020) {
    print(paste("On population number ", unique_loc, ", getting temp data from ", rep, sep = ""))
    
    ## read in gridded data in nc file for the file_index from berkeley earth and store data in R workspace 
    # filename <- paste("data-raw/temperature-data/Complete_TAVG_Daily_LatLong1_", rep, ".nc", sep = "")
    filename <- paste("~/Documents/too-big-for-github/temperature-data/Complete_TAVG_Daily_LatLong1_", rep, ".nc", sep = "")
    ncfile <- nc_open(filename)
    
    ## create variables for things needed to use data
    date <- ncvar_get(ncfile, "date_number")
    arr.anom <-ncvar_get(ncfile, "temperature")
    arr.clim <- ncvar_get(ncfile, "climatology")
    
    if (rep == 1930 & unique_loc == 1) {
      lat <- ncfile$dim$latitude$vals
      lon <- ncfile$dim$longitude$vals
    }
    
    nc_close(ncfile)
    
    ## get clim and anom data for collection location of species 
    ## NaN here if location does not have data 
    loc_long_index <- which.min(abs(lon - unique_pairs$longitude[unique_loc]))
    loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[unique_loc]))
    loc.anom <- arr.anom[loc_long_index,loc_lat_index,]
    loc.clim.365d <- arr.clim[loc_long_index,loc_lat_index,]
    
    ## account for leap year - duplicate day index added on feb 28 in clim array (this seems to be how they dealt with it when calculating anom)
    index_59 <- loc.clim.365d[59]
    loc.clim.366d <- append(loc.clim.365d, index_59, after = 59)
    
    ## repeat day list loc.clim.365d on normal years + loc.clim.366d on leap years 
    last_year = (rep + 10)
    loc.clim <- c()
    
    while (rep < last_year) {
      if (rep == 2020) {
        loc.clim <- append(loc.clim, loc.clim.365d[1:151], after = length(loc.clim))
      }
      else if (rep %% 4 == 0){
        if (rep == 1900) { 
          loc.clim <- append(loc.clim, loc.clim.365d, after = length(loc.clim))
        }
        else {
          loc.clim <- append(loc.clim, loc.clim.366d, after = length(loc.clim))
        }
      }
      else {
        loc.clim <- append(loc.clim, loc.clim.365d, after = length(loc.clim))
      }
      rep = rep + 1
    }
    
    ## create dataframe of actual temp values at location by adding anomaly and climatology values over the 10 years 
    temp_list <- c()
    max <- rep
    rep <- rep - 10
    d <- 1
    
    while (rep < max) {
      if (rep == 2020) {
        temps <- loc.anom[d:(d+150)] + loc.clim[d:(d+150)]
        temp_list <- append(temp_list, temps, after = length(temp_list))
        d = d + 150
      }
      else if (rep %% 4 == 0) {
        if (rep == 1900) {
          temps <- loc.anom[d:(d+364)] + loc.clim[d:(d+364)]
          temp_list <- append(temp_list, temps, after = length(temp_list))
          d = d + 365
        }
        else { 
          temps <- loc.anom[d:(d+365)] + loc.clim[d:(d+365)]
          temp_list <- append(temp_list, temps, after = length(temp_list))
          d = d + 366
        }
      }
      else {
        temps <- loc.anom[d:(d+364)] + loc.clim[d:(d+364)]
        temp_list <- append(temp_list, temps, after = length(temp_list))
        d = d + 365
      }
      rep = rep + 1
    }
    
    ## make dataframe of date and corresponding temp values
    loc <- data.frame(date[], temp_list[])
    colnames(loc) <- c("date", "temp_value")
    
    ## add loc to loc_cumulative so one data frame contains data from all 10 year datasets
    loc_cumulative <- rbind(loc_cumulative, loc)
  }
  
  pop_id <- unique_pairs$temp_id[unique_loc]
  
  ## add column for population to temperature data 
  if (unique_loc == 1){
    temperature_data <- cbind(temperature_data, loc_cumulative)
    temperature_data <- temperature_data[,-1]
    colnames(temperature_data)[unique_loc+1] <- pop_id
  }
  else {
    temperature_data <- cbind(temperature_data, loc_cumulative[,2])
    colnames(temperature_data)[unique_loc+1] <- pop_id
  }
  
  unique_loc = unique_loc + 1;
  
}


terrestrial_temps <- temperature_data[-1,]



#### try rerdapp

OISST_sub_dl <- function(time_df){
  OISST_dat <- rerddap::griddap(datasetx = "ncdcOisst21Agg_LonPM180",
                                url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                                time = c(time_df$start, time_df$end), 
                                zlev = c(0, 0),
                                latitude = c(-40, -35),
                                longitude = c(15, 21),
                                fields = "sst")$data %>% 
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst, lon = longitude, lat = latitude) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    stats::na.omit()
}


# Date download range by start and end dates per year
dl_years <- data.frame(date_index = 1,
                       start = c("1982-01-01"),
                       end = c("1989-12-31"))

# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed

test1 <- OISST_sub_dl(dl_years[1,])

base::system.time(
  OISST_data <- dl_years %>% 
    dplyr::group_by(date_index) %>% 
    dplyr::group_modify(~OISST_sub_dl(.x)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(lon, lat, t, temp)
) # 518 seconds, ~100 seconds per batch

#####


res <- data.frame(
  source = unique(bio$Source),
  slope = NA,
  pval = NA,
  nval = NA
)

plg <- vect(path_gdb, layer = res$source[1])

geom(plg) %>% dim

?ncvar_get

?terra::extract
dat <- terra::extract(berk1, plg) %>% 
  slice(1) %>% 
  mutate(unique_id = rownames(.)) %>% 
  select(unique_id, everything())
dim(dat)
class(dat)
?terra::extract
  

dat2 <- dat %>% 
  gather(key = date, value = temperature, 4:ncol(dat)) %>% 
  filter(grepl("temperature", date)) %>% 
  separate(date, into = c("variable", "day"), sep = "_") %>% 
  select(-variable) %>% 
  mutate(day = as.numeric(day))


dat3 <- dat %>% 
  slice(1) %>% 
  gather(key = date, value = temperature, 4:ncol(dat)) %>% View
  filter(grepl("climatology", date)) %>% 
  separate(date, into = c("variable", "day"), sep = "=") %>% 
  select(-variable) %>% 
  rename(climatology = temperature)

all_temps <- left_join(dat2, dat3)


?separate

unique(dat2$date)


rep(seq_len(ncol(dat)), each = nrow(dat)) ~ unlist(dat)


write.csv(dat, sprintf("tmp/ranges/%s.csv", res$source[1]))
dat <- dat |>
  dplyr::select(-ID)
res_lm <- lm(rep(seq_len(ncol(dat)), each = nrow(dat)) ~ unlist(dat)) |>
  summary()
res_lm


res$slope[1] <- res_lm$coefficients[2, 1]
res$pval[1] <- res_lm$coefficients[2, 4]
res$nval[1] <- nrow(dat)





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
