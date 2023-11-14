


library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(geosphere)
library(broom)
library(janitor)


intratherm <- read_csv("data-processed/intratherm-with-elev.csv") %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	mutate(population_id = paste(genus_species, latitude, elevation_of_collection, longitude, sep = "_"))


intratherm %>% 
	filter(genus_species == "Zoarces viviparus") %>% View

### how many different geographic locations?

	
### Which populations have multiple acclimation  temperatures?
multi_acc <- intratherm %>% 
	filter(parameter_tmax_or_tmin =="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	tally() %>% 
	filter(n > 1)

arrs <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(population_id %in% c(multi_acc$population_id)) %>% ## gets us the populations which have multiple acclimation temps
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(realm_general2, genus_species, population_id, lat_long) %>% 
	do(tidy(lm(parameter_value ~ acclim_temp, data=.))) 



intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	# filter(population_id == "Lithobates sylvaticus_37.56_NA_-84.3") %>% 
	ggplot(aes(x = acclim_temp, y = parameter_value, group = population_id)) + 
	geom_smooth(method = "lm", se = FALSE, alpha = 0.5) + geom_point() +
	ylab("CTmax") + xlab("Acclimation temperature")

# arr_slopes <- arrs %>% 
# 	filter(term != "(Intercept)") %>% ## ok fewer than half of the data have more than 2 acclimation temperatures
# 	select(genus_species, population_id, estimate) %>% 
# 	rename(slope = estimate) %>% 
# 	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") %>% ## this is the super high ARR, it's only got two data points
# 	filter(population_id != "Perca flavescens_42.08_NA_-81.34") ### this is the super low ARR
# 
# intercepts <- arrs %>% 
# 	filter(term == "(Intercept)")  %>% 
# 	select(genus_species, population_id, estimate) %>% 
# 	rename(intercept = estimate) %>% 
# 	filter(population_id != "Perca flavescens_42.08_NA_-81.34") %>%  ### this is the super low ARR
# 	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") ## this is the super high ARR, it's only got two data points
# 
# 
# slopes_int <- left_join(intercepts, arr_slopes)

View(slopes_int) ### JB come back here! (November 14 to figure out why we are only getting back one line per species)
### ok actually, that is not true, we do have some cases where we have more than one line per species. Phew.

ctmax_20 <- arrs %>% 
	select(genus_species, population_id, term, estimate) %>% 
	spread(key = term, value = estimate) %>% 
	rename(intercept = `(Intercept)`) %>%
	mutate(ctmax_20 = acclim_temp*20 + intercept) %>% 
	filter(!is.na(ctmax_20)) 

View(ctmax_20)

ctmax_20 %>% 
	group_by(genus_species) %>% 
	tally() %>% View

# trying now grouping by species, not population for the ARRs -------------

### ok this doesn't seem to help anything
multi_acc_s <- intratherm %>% 
	filter(parameter_tmax_or_tmin =="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species) %>% 
	tally() %>% 
	filter(n > 1)

arrs_s <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(population_id %in% c(multi_acc$population_id)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(realm_general2, genus_species) %>% 
	do(tidy(lm(parameter_value~acclim_temp, data=.))) 

intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	# filter(population_id == "Lithobates sylvaticus_37.56_NA_-84.3") %>% 
	ggplot(aes(x = acclim_temp, y = parameter_value, group = population_id)) + 
	geom_smooth(method = "lm", se = FALSE, alpha = 0.5) + geom_point() +
	ylab("CTmax") + xlab("Acclimation temperature")

arr_slopes_s <- arrs_s %>% 
	filter(term != "(Intercept)") %>% ## ok fewer than half of the data have more than 2 acclimation temperatures
	select(genus_species, estimate) %>% 
	rename(slope = estimate) %>% 
	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") %>% ## this is the super high ARR, it's only got two data points
	filter(population_id != "Perca flavescens_42.08_NA_-81.34") ### this is the super low ARR

intercepts_s <- arrs_s %>% 
	filter(term == "(Intercept)")  %>% 
	select(genus_species, population_id, estimate) %>% 
	rename(intercept = estimate) %>% 
	filter(population_id != "Perca flavescens_42.08_NA_-81.34") %>%  ### this is the super low ARR
	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") ## this is the super high ARR, it's only got two data points


slopes_int <- left_join(intercepts, arr_slopes)

View(slopes_int) ### JB come back here! (November 14 to figure out why we are only getting back one line per species)
### ok actually, that is not true, we do have some cases where we have more than one line per species. Phew.

ctmax_20_s <- arrs_s %>% 
	select(genus_species, term, estimate) %>% 
	spread(key = term, value = estimate) %>% 
	rename(intercept = `(Intercept)`) %>%
	mutate(ctmax_20 = acclim_temp*20 + intercept) %>% 
	filter(!is.na(ctmax_20)) 


View(ctmax_20)

all_ctmax <- multi_acc %>% 
	left_join(., ctmax_20) ### I think this is where the problem is


# end that section --------------------------------------------------------





fw <- read_csv("data-processed/intratherm-freshwater-temp-data-daily.csv") 
fw <- read_delim("~/Documents/too-big-for-github/intratherm-freshwater-temp-data-daily.txt", delim = ",") 

dim(fw)

fw_ctmax <- all_ctmax %>% 
	filter(realm_general2 == "Freshwater")

### ok need to come back here to get the sd of temps
fw2 <- fw %>% 
	gather(key = population_id, value = monthly_temp, 2:383) %>%
	separate(date, into = c("year", "fraction")) %>% 
	group_by(population_id, year) %>% 
	summarise(max_yearly_temp = max(monthly_temp)) %>% 
	group_by(population_id) %>% 
	summarise(mean_yearly_max_temp = mean(max_yearly_temp))


intratherm_traits <- intratherm %>% 
	select(population_id, age_maturity_days_female, dispersal_distance_category, lifespan_days, average_body_size_female)


### this is the dataset that contains all the FW species, their traits and the temp data
intra_fw2 <- fw_ctmax %>% 
	left_join(., fw2) %>% 
	left_join(., intratherm_traits) %>% 
	mutate(age_maturity_days_female = as.numeric(age_maturity_days_female)) %>% 
	mutate(average_body_size_female = as.numeric(average_body_size_female)) %>% 
	mutate(lifespan_days  = as.numeric(lifespan_days)) %>% 
	distinct(., .keep_all = TRUE) %>% 
	filter(!is.na(ctmax_20))




library(nlme)

	intra_fw2 %>% 
	ggplot(aes(x = age_maturity_days_female, y = lifespan_days)) + geom_point()


intra_fw3 <- intra_fw2 %>% 
	ungroup() %>% 
	filter(complete.cases(.))

str(intra_fw2)

### run models!

#sd_ctmax ~ sd_env_temperature + sd_temp*Mean_dispersal_distance + sd_temp*ARR + body_size, random = phylum/class/order/genus
## add dispersal_distance*collection_distance

### ok so somewhere here I need to add the distance between populations.

View(intra_fw3)

### bring in some of the distance data

distances <- read_csv("data-processed/initialize_pairwise_differences_experienced_output.csv")

d2 <- distances %>% 
	select(genus_species, lat_lon_1, lat_lon_2, distance) %>% 
	rename(lat_long = lat_lon_1)

i4 <- intra_fw3 %>% 
	select(genus_species, lat_long)


d2 %>% 
	left_join(i4) %>% View
intra4 <- intra_fw3 %>% 
	group_by(genus_species, dispersal_distance_category, average_body_size_female) %>% 
	summarise(sd_ctmax = sd(ctmax_20),
			  sd_temperature = sd(mean_yearly_max_temp),
			  mean_arr = mean(acclim_temp))


mod1 <- lm(sd_ctmax ~ sd_temperature + sd_temperature*dispersal_distance_category + sd_temperature*mean_arr + average_body_size_female, data = intra4)
mod2 <- lm(sd_ctmax ~ sd_temperature + sd_temperature*dispersal_distance_category + sd_temperature*mean_arr, data = intra4)

AIC(mod1, mod2)
summary(mod2)

library(visreg)

visreg(mod2)


mod1 <- lme(ctmax_20 ~ age_maturity_days_female + dispersal_distance_category +
				lifespan_days + average_body_size_female, data = intra_fw3, random = ~ 1 | genus_species)

mod1 <- lm(ctmax_20 ~ age_maturity_days_female + dispersal_distance_category +
		lifespan_days + average_body_size_female, data = intra_fw3)

summary(mod1)


### all temperatures
## this is the pairwise temperatures
temps <- read_csv("data-processed/initialize_pairwise_differences_experienced_and_topt.csv")

## raw daily temperature data
terr_temps <- read_csv("data-processed/intratherm-terrestrial-temps-tavg.csv")
terr_temps <- read_csv("data-processed/intratherm-temp-data-yearly-maxes-daily-avg.csv")
terr_temps2 <- read_csv("data-processed/intratherm-temp-data-yearly-maxes.csv")




fw <- read_csv("data-processed/intratherm-freshwater-temp-data.csv") 
marine_temps <- read_csv("data-processed/intratherm-marine-temp-data.csv")
# operative_temps <- read.csv("data-processed/OperativeTemperatures_shade.csv", sep = "\t")

terr_temps

operative_temps <- read.delim("data-processed/OperativeTemperatures_shade.csv", sep = " ") %>%
	rename(intratherm_id = ID) %>% 
	clean_names()

operative_temps %>% 
	# gather(2:5, key = temperature_type, value = temperature) %>% 
	ggplot(aes(x = airtq75, y = te_sun_q75)) + geom_point() + geom_abline(intercept = 0, slope = 1)

operative_temps %>% 
	# gather(2:5, key = temperature_type, value = temperature) %>% 
	ggplot(aes(x = airtq75, y = te_burr_q75)) + geom_point() + geom_abline(intercept = 0, slope = 1)

operative_temps %>% 
	# gather(2:5, key = temperature_type, value = temperature) %>% 
	ggplot(aes(x = te_sun_q75, y = te_burr_q75)) + geom_point() + geom_abline(intercept = 0, slope = 1)


View(marine_temps)
library(lubridate)
m2 <- marine_temps %>% 
	gather(key = population_id, value = daily_temp, 2:72) %>% 
	mutate(date = ymd(date)) %>% 
	mutate(year = year(date)) %>% 
	mutate(month = month(date)) %>% 
	group_by(population_id, year) %>% 
	summarise(max_yearly_temp = max(daily_temp)) %>% 
	group_by(population_id) %>% 
	summarise(mean_yearly_max_temp = mean(max_yearly_temp))

fw2 <- fw %>% 
	gather(key = population_id, value = monthly_temp, 2:386) %>%
	separate(date, into = c("year", "fraction")) %>% 
	group_by(population_id, year) %>% 
	summarise(max_yearly_temp = max(monthly_temp)) %>% 
	group_by(population_id) %>% 
	summarise(mean_yearly_max_temp = mean(max_yearly_temp))

### now the ctmax data
intratherm <- read.csv("data-processed/intratherm-with-elev.csv") %>% 
	filter(!is.na(latitude)) %>% 
	filter(!is.na(longitude)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	mutate(population_id = paste(genus_species, latitude, elevation_of_collection, longitude, sep = "_"))



multi_acc <- intratherm %>% 
	filter(parameter_tmax_or_tmin =="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	tally() %>% 
	filter(n > 1)


terr_ct <- intratherm  %>% 
	filter(genus_species %in% multi_acc$genus_species) %>% 
	filter(realm_general2 == "Terrestrial") %>% 
	left_join(., operative_temps, by = "intratherm_id")


terr_ct %>% 
	filter(genus_species == "Ambystoma macrodactylum") %>% 
	ggplot(aes(x = acclim_temp, y = parameter_value)) + geom_point() +
	geom_smooth(method = "lm") +ylab("CTmax (°C)") + xlab("Acclimation temperature (°C)")
ggsave("figures/ambystoma-macrodactylum-arr.png", width = 6, height = 4)



terr_ct %>% 
	group_by(population_id) %>% 
	distinct(acclim_temp) %>% 
	tally() %>% View
	
	
	terr_ct %>% 
		filter(population_id == "Drosophila melanogaster_-17.525_NA_146.031845") %>% 
		ggplot(aes(x = acclim_temp, y = parameter_value)) + geom_point() +
		geom_smooth(method = "lm") +ylab("CTmax (°C)") + xlab("Acclimation temperature (°C)")
	ggsave("figures/drosophila-arr.png", width = 6, height = 4)

arrs <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(population_id %in% c(multi_acc$population_id)) %>% 
	mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% 
	group_by(realm_general2, genus_species, population_id, lat_long) %>% 
	do(tidy(lm(parameter_value~acclim_temp, data=.))) 

arr_slopes <- arrs %>% 
	filter(term != "(Intercept)") %>% ## ok fewer than half of the data have more than 2 acclimation temperatures
	select(genus_species, population_id, estimate) %>% 
	rename(slope = estimate) %>% 
	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") %>% ## this is the super high ARR, it's only got two data points
	filter(population_id != "Perca flavescens_42.08_NA_-81.34") ### this is the super low ARR

intercepts <- arrs %>% 
	filter(term == "(Intercept)")  %>% 
	select(genus_species, population_id, estimate) %>% 
	rename(intercept = estimate) %>% 
	filter(population_id != "Perca flavescens_42.08_NA_-81.34") %>%  ### this is the super low ARR
	filter(population_id != "Retropinna retropinna_-37.595991_NA_175.104216") ## this is the super high ARR, it's only got two data points


slopes_int <- left_join(intercepts, arr_slopes)

ctmax_20 <- arrs %>% 
	select(genus_species, population_id, term, estimate) %>% 
	spread(key = term, value = estimate) %>% 
	rename(intercept = `(Intercept)`) %>% 
	mutate(ctmax_20 = acclim_temp*20 + intercept) %>% 
	filter(!is.na(ctmax_20)) %>% 
	rename(arr = acclim_temp)


terr_ct_max %>% 
	ggplot(aes(x = latitude, y = airtq75)) + geom_point()

all_ctmax <- multi_acc %>% 
	left_join(., ctmax_20)

terr_ct_max <- ctmax_20 %>% 
	left_join(., terr_ct) %>% 
	ungroup() %>% 
	filter(realm_general2 == "Terrestrial") %>% 
	select(contains("75"), everything()) %>% 
	mutate(age_maturity_days_female = as.numeric(age_maturity_days_female)) %>% 
	mutate(lifespan_days = as.numeric(lifespan_days)) %>% 
	mutate(average_body_size_female = as.numeric(average_body_size_female)) %>% 
	mutate(age_maturity_days = ifelse(is.na(age_maturity_days_female), age_maturity_days_male, age_maturity_days_female)) %>% 
	mutate(age_maturity_days = as.numeric(age_maturity_days))


terr_ct_max %>% 
	group_by(genus_species) %>% 
	tally() %>% View

	### ok now fit some models for the terrestrial data

str(terr_ct_max)

terr_ct_max %>% 
	ggplot(aes(x = age_maturity_days, y = lifespan_days)) + geom_point()

length(unique(ctmax_20$genus_species)) 

terr2 <- terr_ct_max %>% 
	filter(!is.na(age_maturity_days)) %>% 
	filter(!is.na(lifespan_days))
cor(terr2$age_maturity_days, terr2$lifespan_days)

unique(terr2)

library(lme4)
library(nlme)

mod1 <- lm(ctmax_20 ~ age_maturity_days*airtq75 + dispersal_distance_category*airtq75 + 
   	arr*airtq75 + maximum_body_size_svl_hbl_cm*airtq75, data = terr2)
summary(mod1)

mod1a <- lm(ctmax_20 ~ lifespan_days*airtq75 + dispersal_distance_category*airtq75 + 
		   	arr*airtq75 + maximum_body_size_svl_hbl_cm*airtq75, data = terr2)
mod1b <- lm(ctmax_20 ~ age_maturity_days*airtq75 + dispersal_distance_category*airtq75 + 
		   	arr*airtq75 + lifespan_days*airtq75 + maximum_body_size_svl_hbl_cm, data = terr2) ### this one looks like it's the best
mod1c <- lm(ctmax_20 ~ age_maturity_days*airtq75 + dispersal_distance_category*airtq75 + 
				 lifespan_days*airtq75 + maximum_body_size_svl_hbl_cm, data = terr2)

AIC(mod1, mod1a, mod1b, mod1c)
summary(mod1b)

library(piecewiseSEM)
library(MuMIn)
mod1 <- lme(ctmax_20 ~ age_maturity_days*airtq75 + dispersal_distance_category*airtq75 + 
		   	arr*airtq75 + maximum_body_size_svl_hbl_cm*airtq75, random = ~1|genus_species, data = terr2)

mod1a <- lme(ctmax_20 ~ lifespan_days*airtq75 + dispersal_distance_category*airtq75 + 
				arr*airtq75 + maximum_body_size_svl_hbl_cm*airtq75, random = ~1|genus_species, data = terr2)

mod1b <- lme(ctmax_20 ~ age_maturity_days*airtq75 + dispersal_distance_category*airtq75 + 
				arr*airtq75 + lifespan_days*airtq75 + maximum_body_size_svl_hbl_cm, random = ~1|genus_species, data = terr2)

mod1c <- lme(ctmax_20 ~ age_maturity_days*airtq75 + dispersal_distance_category*airtq75 + 
				lifespan_days*airtq75 + maximum_body_size_svl_hbl_cm, random = ~1|genus_species, data = terr2)

model.sel(mod1, mod1a, mod1b, mod1c) %>% View

summary(mod1b)
rsquared(mod1b)
anova(mod1b)
ranef(mod1b) # random
fixef(mod1b) # fixed

library(stargazer)

stargazer(mod1b, type = "html", out = "tables/terr-ctmax.html")

op_temps <- read.delim("data-processed/OpTemperatures_version_JAN_2021.csv", sep = " ") %>%
	rename(intratherm_id = ID) %>% 
	clean_names()

table(terr2$is.nocturnal)

names(op_temps)
terr3 <- terr2 %>% 
	left_join(op_temps) %>% 
	filter(is.nocturnal != "unk") %>% 
	mutate(thermo_cap = ifelse(is.nocturnal == "N", te_sun_q75_nobehav - te_shade_q75, te_sun_q75_nobehav - te_shade_q75))

terr2 %>% 
	distinct(genus_species) %>% View

terr3 %>% 
	distinct(genus_species) %>% View


missing <- op_temps %>% 
	filter(is.na(te_shade_q75))


length(unique(missing$genus_species))

length(unique(terr3$genus_species))

terr3 %>% 
	ggplot(aes(x = thermo_cap)) + geom_histogram()

View(terr2)

View(intratherm)
terr3 %>% 
	filter(!is.na(thermo_cap)) %>% View
		


mod1b <- lme(ctmax_20 ~ age_maturity_days*airtq75 + dispersal_distance_category*airtq75 + 
			 	arr*airtq75 + lifespan_days*airtq75 + maximum_body_size_svl_hbl_cm, random = ~1|genus_species, data = terr3)

library(stargazer)
stargazer(mod1b, type = "html", out = "tables/terr-ctmax-feb.html")


mod1b <- lme(ctmax_20 ~ thermo_cap + age_maturity_days*airtq75 + dispersal_distance_category*airtq75 + 
			 	arr*airtq75 + lifespan_days*airtq75, random = ~1|genus_species, data = terr3)

unique(terr3$thermo_cap)

mod1b <- lme(ctmax_20 ~  thermo_cap, random = ~1|genus_species, data = terr3)

summary(mod1b)

mod2 <- lme(ctmax_20 ~ age_maturity_days*burrtq75 + dispersal_distance_category*burrtq75 + 
		   	arr*burrtq75 + lifespan_days*burrtq75 + maximum_body_size_svl_hbl_cm, data = terr2, random = ~1|genus_species)

mod3 <- lme(ctmax_20 ~ age_maturity_days*te_sun_q75 + dispersal_distance_category*te_sun_q75 + 
		   	arr*te_sun_q75 + lifespan_days*te_sun_q75 + maximum_body_size_svl_hbl_cm, data = terr2, random = ~1|genus_species)

mod4 <- lme(ctmax_20 ~ age_maturity_days*te_burr_q75 + dispersal_distance_category*te_burr_q75 + 
		   	arr*te_burr_q75 + lifespan_days*te_burr_q75 + maximum_body_size_svl_hbl_cm, data = terr2, random = ~1|genus_species)



### burrow only for nocturnal species and sun temperature for dirunal species
(AIC(mod1b, mod2, mod3, mod4))

model.sel(mod1b, mod2, mod3, mod4, rank = "AICc", extra = "rsquared") %>% View

summary(mod1)
library(visreg)

terr2 %>% 
	ggplot(aes(x = ctmax_20, y = te_sun_q75)) + geom_point() +
	geom_abline(intercept = -3, slope = 1) + ylab("Max operative temp (75% quantile)\n in exposed areas") +
	xlab("CTmax at 20C")
ggsave("figures/sun-operative-temp-ctmax.png", width = 6, height = 4)

terr2 %>% 
	ggplot(aes(x = ctmax_20, y = airtq75)) + geom_point() +
	geom_abline(intercept = -3, slope = 1) + ylab("Max air temperature from NicheMapR \n (75% quantile during the middle day of the warmest month)") +
	xlab("CTmax at 20C")
ggsave("figures/air-temp-ctmax.png", width = 8, height = 6)

visreg(mod1)

terr_ct_max %>% 
	filter(is.na(maximum_body_size_svl_hbl_cm)) %>% View

summary(mod1)

terr_ct_max %>% 
	ggplot(aes(x = maximum_body_size_svl_hbl_cm, y = average_body_size_male)) + geom_point()




anova(mod1)

# View(multi_acc)

### ok we are aiming for something like this: 
# mod1 <- lme(ctmax_20 ~ age_maturity_days_female*temperature + dispersal_distance_category*temperature + 
# ARR*temperature + lifespan_days*temperature + average_body_size_female + sd_temperature, data = intra_fw2, random = ~ 1 | genus_species)




mod1 <- lm(ctmax_20 ~ age_maturity_days_female*temperature + dispersal_distance_category*temperature + ARR*temperature + lifespan_days*temperature + average_body_size_female , data = intra_fw2)




# read in population dynamics data ----------------------------------------

pops <- read_rds("data-processed/population-dynamics-with-temp-ts.rds")
x <- bind_rows(pops)

g <- x %>% 
	filter(abundance < 1) 

ggplot(data = g, aes(x = date, y = abundance)) + 
	geom_path(data = na.omit(g), aes(colour = population_id)) + 
	labs(y = "Year", x = "Abundance")  +
	theme(legend.position = "none")




