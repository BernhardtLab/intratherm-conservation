## predicted CTmax and population dynamics analysis 
library(tidyverse)
library(broom)

## read in R object of dataframes:
pops <- readRDS("data-processed/population-dynamics-with-temp-ts.rds")


## as a test, use temperature 14 days before to 'acclimate' and model CTmax of organism using average ARR for species from intratherm
## our model will be more complicated 
intratherm <- read_csv("data-processed/intratherm-with-elev.csv") %>%
	mutate(population_id = paste(population_id, longitude, sep = "_"))

multi_acc <- intratherm %>% 
	filter(parameter_tmax_or_tmin=="tmax") %>%
	filter(!is.na(acclim_temp)) %>%
	group_by(genus_species, population_id) %>% 
	tally() %>%   
	filter(n > 1)

## make predictions for each population based on groups of each species:
arr_fits <- intratherm %>% 
	filter(parameter_tmax_or_tmin == "tmax") %>% 
	filter(!is.na(acclim_temp)) %>%
	filter(population_id %in% c(multi_acc$population_id)) %>% 
	group_by(genus_species) %>% 
	filter(length(unique(acclim_temp)) > 1) %>%
	do(fit = MASS::rlm(parameter_value~acclim_temp, data=.))

pops <- bind_rows(pops) %>%
	mutate(genus_species = str_split_fixed(population_id, pattern = "_", n = 3)[,1]) %>%
	mutate(acclim_temp = temperature) %>%
	## mutate(acclim_temp = lag(temperature, n = 14)) %>%
	filter(!is.na(acclim_temp))

pops %>% 
	filter(!is.na(abundance)) %>% 
	group_by(population_id) %>% 
	mutate(scaled_abundance = scale(abundance)) %>% 
	# ungroup() %>% 
	# distinct(population_id) %>% 
	ggplot(aes(x = date, y = scaled_abundance, group = population_id, color = population_id)) + geom_line() +
	theme(legend.position = "none") +ylab("Scaled population abundance") + xlab("Year") +
	theme(text = element_text(size=20))
ggsave("figures/population-time-series-scaled.png", width = 20, height = 12)


pops %>% 
	filter(!is.na(abundance)) %>% 
	group_by(population_id) %>% 
	distinct(date) %>% 
	tally() %>%
	filter(n < 50) %>% 
	ggplot(aes(x = n)) + geom_histogram(bins = 70) + 
	scale_x_continuous(breaks = c(1:36))
ggsave("figures/pop-dyn-time-point-distribution.png", width = 9, height = 5)

pops %>% 
	filter(!is.na(abundance)) %>% 
	group_by(population_id) %>% 
	mutate(scaled_abundance = scale(abundance)) %>% 
	add_tally() %>% 
	filter(n > 10) %>% 
	ggplot(aes(x = date, y = scaled_abundance, group = population_id, color = population_id)) + geom_line() +
	theme(legend.position = "none") +
	facet_wrap( ~ population_id) +
	theme(
		strip.background = element_blank(),
		strip.text.x = element_blank()
	)
ggsave("figures/pop-dyn-time-series.png", width = 15, height = 10)


### let's see if we can make a map of the population dynamics locations

pops_loc <- pops %>% 
	separate(population_id, into = c("species", "latitude", "longitude", "extra"), sep = "_")


predictions <- pops %>%
	select(genus_species, acclim_temp, date, temperature, population_id, abundance) %>%
	group_by(genus_species) %>% 
	nest() %>% 
	left_join(., arr_fits) %>% 
	group_by(genus_species) %>% 
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(predictedCTmax = .fitted, se = .se.fit) 
	

##check out each population:
i = 1
while (i < length(unique(predictions$population_id))+1) {
	pop <- predictions %>%
		filter(population_id == unique(predictions$population_id)[i])
	
	avg <- (mean(pop$temperature, na.rm=TRUE) +  mean(pop$predictedCTmax, na.rm=TRUE)) /2
	avg_abd <- mean(pop$abundance, na.rm=TRUE)
	
	scale <- avg/avg_abd
	
	pop_id_split <- str_split_fixed(pop$population_id, n = 4, pattern = "_")
	label <- paste(pop_id_split[1,1], "\n", pop_id_split[1,2], ", ", pop_id_split[1,3], sep = "")
	
	pop %>% 
		ggplot(aes(x = date, y = temperature)) + 
		geom_line(aes(colour = "Temperature")) +
		geom_line(aes(y = predictedCTmax, x = date, colour = "Predicted CTmax")) +
		geom_line(data = na.omit(pop), aes(y = abundance*scale, x = date, colour = "Abundance")) + 
		labs(x = "Date", y = "Temperature (°C)") +
		scale_y_continuous(sec.axis = sec_axis(~./scale, name = "Abundance")) + 
		theme(axis.title.y.right = element_text(color = "blue")) +
		scale_colour_manual(name = label,
							values = c(`Predicted CTmax`="orange", 
												 Abundance="blue", Temperature="black"))
	
	ggsave(filename = paste("figures/temp-ctmax-abundance-figures/", unique(pop$population_id), ".png", sep = ""), height = 6, width = 12, units = "in", device = "png")
	
	i = i + 1
}

 
## see how many times temperature exceeds predicted CTmax when time lag in acclimation is 0 days 
count <- predictions %>%
	mutate(temp_dif = predictedCTmax - temperature) 

length(which(count$temp_dif < 0)) ## CTmax is exceeded a total of 23 times 

## how many different populations is CTmax exceeded in?
length(unique(count$population_id[which(count$temp_dif < 0)]))  ## in 2 populations only

count <- count %>% 
	filter(population_id %in% unique(count$population_id[which(count$temp_dif < 0)]))



## see how much that changes when you incorporate a delay in acclimation
pops2 <- pops %>%
	mutate(acclim_temp = lag(temperature, n = 7))

predictions2 <- pops %>%
	select(genus_species, acclim_temp, date, temperature, population_id, abundance) %>%
	group_by(genus_species) %>% 
	nest() %>% 
	left_join(., arr_fits) %>% 
	group_by(genus_species) %>% 
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(predictedCTmax = .fitted, se = .se.fit) 

count2 <- predictions2 %>%
	mutate(temp_dif = predictedCTmax - temperature) 

length(which(count2$temp_dif < 0)) ## 23

count2 <- count2 %>% 
	filter(population_id %in% unique(count2$population_id[which(count2$temp_dif < 0)]))


## see how many days the temperature is within 3 degrees of ctmax 
count3 <- predictions %>%
	mutate(temp_dif = predictedCTmax - (temperature + 3))

length(which(count3$temp_dif < 0)) ## 1431

## how many different populations is CTmax exceeded in?
length(unique(count3$population_id[which(count3$temp_dif < 0)])) ## 27

count3 <- count3 %>% 
	filter(population_id %in% unique(count3$population_id[which(count3$temp_dif < 0)]))



## try something new:
## when acclim_temp is outside of the range used in the original studies, don't allow extrapolation -- instead assume slope of 0


## add column for range of acclimation temps used for each species in study 
predictions3 <- intratherm %>%
	group_by(genus_species) %>%
	mutate(max_acclim_temp = max(acclim_temp)) %>%
	mutate(min_acclim_temp = min(acclim_temp)) %>% 
	select(genus_species, min_acclim_temp, max_acclim_temp) %>%
	distinct() %>%
	rename(acclim_temp = min_acclim_temp) %>%
	nest() %>% 
	left_join(., arr_fits, by = "genus_species") %>% 
	group_by(genus_species) %>% 
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(min_parameter_value = .fitted, se.min = .se.fit) %>%
	select(genus_species, max_acclim_temp, everything()) %>%
	rename(min_acclim_temp = acclim_temp, acclim_temp = max_acclim_temp) %>%
	nest() %>% 
	left_join(., arr_fits, by = "genus_species") %>%
	do(augment(.$fit[[1]], newdata = .$data[[1]])) %>%
	rename(max_parameter_value = .fitted, max_acclim_temp = acclim_temp) %>%
	ungroup() %>%
	select(genus_species, min_acclim_temp, min_parameter_value, max_acclim_temp, max_parameter_value) %>%
	left_join(predictions, ., by = "genus_species") %>%
	mutate(predictedCTmax = ifelse(max_acclim_temp < acclim_temp, 
								   max_parameter_value, 
								   predictedCTmax
								   ))

count4 <- predictions3 %>%
	mutate(temp_dif = predictedCTmax -  (temperature + 3)) 

length(which(count4$temp_dif < 0)) ## CTmax is exceeded a total of 1861 times now

## how many different populations is CTmax exceeded in?
length(unique(count4$population_id[which(count4$temp_dif < 0)])) ## 44

count4 <- count4 %>% 
	filter(population_id %in% unique(count4$population_id[which(count4$temp_dif < 0)]))


## check out those graphs:
i = 1
while (i < length(unique(count4$population_id))+1) {
	pop <- count %>%
		filter(population_id == unique(count4$population_id)[i])
	
	avg <- (mean(pop$temperature, na.rm=TRUE) +  mean(pop$predictedCTmax, na.rm=TRUE)) /2
	avg_abd <- mean(pop$abundance, na.rm=TRUE)
	
	scale <- avg/avg_abd
	
	pop_id_split <- str_split_fixed(pop$population_id, n = 4, pattern = "_")
	label <- paste(pop_id_split[1,1], "\n", pop_id_split[1,2], ", ", pop_id_split[1,3], sep = "")
	
	pop %>% 
		ggplot(aes(x = date, y = temperature)) + 
		geom_line(aes(colour = "Temperature")) +
		geom_line(aes(y = predictedCTmax, x = date, colour = "Predicted CTmax")) +
		geom_line(data = na.omit(pop), aes(y = abundance*scale, x = date, colour = "Abundance")) + 
		labs(x = "Date", y = "Temperature (°C)") +
		scale_y_continuous(sec.axis = sec_axis(~./scale, name = "Abundance")) + 
		theme(axis.title.y.right = element_text(color = "blue")) +
		scale_colour_manual(name = label,
							values = c(`Predicted CTmax`="orange", 
									   Abundance="blue", Temperature="black"))
	
	ggsave(filename = paste("figures/temp-ctmax-abundance-figures/adapted-ARR-function/", unique(pop$population_id), ".png", sep = ""), height = 6, width = 12, units = "in", device = "png")
	
	i = i + 1
	
}
