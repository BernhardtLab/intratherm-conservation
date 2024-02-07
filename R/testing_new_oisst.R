
library(tidyverse)
library(ncdf4)
library(rerddap)

all_locs <- read_csv("data-processed/all_locs.csv")

unique_pairs <- all_locs %>% ### now we have 288 species (we used to have 250 here)
  filter(realm_of_population == "Marine") %>%
  select(latitude, longitude, temp_id) %>%
  unique()


info <- info("ncdcOisst21Agg_LonPM180")


## make latitude and longitude vectors based on NOAA format
## longitude: Uniform grid with centers from -179.875 to 179.875 by 0.25 degrees.
lon <- rep(-179.875, times = 1439)
n = 2
while (n < 1441) {
  lon[n] <- lon[n -1] + 0.25
  n = n+1
}
## latitude: Uniform grid with centers from -89.875 to 89.875 by 0.25 degrees.
lat <- rep(-89.875, times = 719)
n = 2
while (n < 721) {
  lat[n] <- lat[n -1] + 0.25
  n = n+1
}


grid_lat <- c()
grid_lon <- c()

## find closest lat lon grid cell to each population collection location 
num_unique <- 1
while (num_unique < nrow(unique_pairs) + 1) {
  loc_lon_index <- which.min(abs(lon - unique_pairs$longitude[num_unique]))
  loc_lat_index <- which.min(abs(lat - unique_pairs$latitude[num_unique]))
  
  grid_lon <- append(grid_lon, lon[loc_lon_index])
  grid_lat <- append(grid_lat, lat[loc_lat_index])
  
  num_unique = num_unique + 1
}

unique_pairs <- unique_pairs %>%
  mutate(grid_lon = grid_lon) %>%
  mutate(grid_lat = grid_lat) 






OISST_sub_dl <- function(time_df){
  temps <- rerddap::griddap(datasetx = "ncdcOisst21Agg_LonPM180",
                                url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                                time = c(time_df$start, time_df$end), 
                                zlev = c(0, 0),
                                latitude = c(unique_pairs$grid_lat[unique_loc],unique_pairs$grid_lat[unique_loc]),
                                longitude = c(unique_pairs$grid_lon[unique_loc], unique_pairs$grid_lon[unique_loc]),
                                fields = "sst")$data %>% 
    dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst, lon = longitude, lat = latitude) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    stats::na.omit()
}



# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed

test1 <- OISST_sub_dl(dl_years[1,])
test2 <- OISST_sub_dl(dl_years[3,])




##### Now implement

## loop through each population getting temp data for its grid cell and adding to temp data

dl_years <- data.frame(date_index = 1:5,
                       start = c("1982-01-01", "1990-01-01", 
                                 "1998-01-01", "2006-01-01", "2014-01-01"),
                       end = c("1989-12-31", "1997-12-31", 
                               "2005-12-31", "2013-12-31", "2019-12-31"))


temperature_data <- data.frame(matrix(nrow = 2922))

time_df <- dl_years[1,]
unique_loc <- 1
while (unique_loc < nrow(unique_pairs) + 1) {
  print(paste("On population number", unique_loc))
  time_series <- griddap(datasetx = "ncdcOisst21Agg_LonPM180",
                         url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                         time = c(time_df$start, time_df$end), 
                         zlev = c(0, 0),
                         latitude = c(unique_pairs$grid_lat[unique_loc],unique_pairs$grid_lat[unique_loc]),
                         longitude = c(unique_pairs$grid_lon[unique_loc], unique_pairs$grid_lon[unique_loc]))
  temps <- time_series$data$sst
  print(paste("Successfully got time series of length", length(temps)))
  
  if (unique_loc == 1) {
    times <- time_series$data$time
    temperature_data[,1] <- times
    colnames(temperature_data) <- c("date")
  }
  
  temperature_data$temp <- temps
  pop_id <- unique_pairs$temp_id[unique_loc]
  colnames(temperature_data)[unique_loc+1]<- pop_id
  
  print("Stored data in temperature_data and moving on to next population!")
  
  unique_loc <- unique_loc + 1
}

## save
marine_temps <- temperature_data

saveRDS(marine_temps, "data-processed/temperature-data/marine_tavg.rds")
marine_temps <- readRDS("data-processed/temperature-data/marine_tavg.rds")
colnames(marine_temps)[1] = "date"

## get rid of missing temps 
marine_missing <- unique_pairs[which(unique_pairs$temp_id %in% colnames(marine_temps)[which(is.na(marine_temps[1,]))]),] %>%
  select(-grid_lat,-grid_lon)

## could not find 64 locations
marine_temps <- marine_temps[,-which(colnames(marine_temps) %in% marine_missing$temp_id)]

write.csv(marine_temps, "data-processed/temperature-data/marine_tavg.csv", row.names = FALSE)

