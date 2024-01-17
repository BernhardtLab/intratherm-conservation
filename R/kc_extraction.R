library(tidyverse)
library(curl) # download data via curl
library(terra) # spatial data wrangling
library(tidyterra) # tidyverse like grammar for spatial data
library(cli) # for good looking console messages

# --- Download BioShifts Database
dir.create("tmp")
curl_download(
    "https://figshare.com/ndownloader/articles/7413365/versions/1",
    destfile = "bioshifts_origin.zip"
)
unzip("bioshifts_origin.zip", exdir = "tmp")
unzip("tmp/BioShifts.zip", exdir = "tmp")
unlink("bioshifts_origin.zip")

path_gdb <- "tmp/Bioshifts/Study_Areas.gdb"
bio <- read_csv("tmp/Bioshifts/BioShifts.csv")
ref <- read_csv("tmp/Bioshifts/References.csv")
lay <- sf::st_layers("tmp/Bioshifts/Study_Areas.gdb")

# --- Download Climatic Data
## Berkley Earth https://berkeleyearth.org/data/
## requires to send a request to have access to high resolution data
## this is the deviation from a mean values that are not included
berk <- rast("tmp/Global_TAVG_Gridded_0p25_1980s.nc", lyrs = 3:122)
?rast

dir.create("tmp/ranges", showWarnings = FALSE)
# --- Extraction for the difference source
## covering only terrestrial
biot <- bio |>
    filter(Ecosystem == "Terrestrial")
res <- data.frame(
    source = unique(bio$Source),
    slope = NA,
    pval = NA,
    nval = NA
)
# raw extraction 1850-now for all 
for (i in seq_len(nrow(res))) {
    cli_alert_info("Dealing with {res$source[i]} {i}/{nrow(res)}")
    # get the polygon
    plg <- vect(path_gdb, layer = res$source[i])
    # do the extraction
    dat <- terra::extract(berk, plg)
    # creates a csv with 1 column per year extracted
    write.csv(dat, sprintf("tmp/ranges/%s.csv", res$source[i]))
    dat <- dat |>
        dplyr::select(-ID)
    res_lm <- lm(rep(seq_len(ncol(dat)), each = nrow(dat)) ~ unlist(dat)) |>
        summary()
    res$slope[i] <- res_lm$coefficients[2, 1]
    res$pval[i] <- res_lm$coefficients[2, 4]
    res$nval[i] <- nrow(dat)
}
