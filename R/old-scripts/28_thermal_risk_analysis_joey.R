
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
data_temp <- read_csv("~/Documents/too-big-for-github/population-dynamics-with-temp-ts.csv")
ctmax_data <- read_csv("data-processed/old-processed-data/intratherm-with-elev.csv")
ctmax_data <- subset(ctmax_data, ctmax_data$parameter_tmax_or_tmin == "tmax")



### in 2021 we had 137 species in the population abundances set
dtb <- data_temp %>% 
	mutate(genus_species = str_replace(population_id, "\\_.*", ""))

unique(dtb$genus_species)
unique(ctmax_data$genus_species)

dt2 <- data_temp %>% 
	mutate(log_n = log(abundance))

dt3 <- dt2 %>% 
	filter(!is.na(abundance)) %>%
	filter(abundance > 0) %>% 
	group_by(population_id) %>% 
	mutate(time_interval = date - lag(date)) %>%
	mutate(time_interval_sd = sd(time_interval, na.rm = TRUE)) %>% 
	filter(time_interval_sd == 0) %>% 
	filter(time_interval == 1) %>% ### I think this is getting us the time series that are sampled every year
	group_by(population_id) %>% 
	tally() %>% 
	filter(n > 10)


length(unique(dt3$population_id))

long_series <- data_temp %>%  
	filter(abundance > 0) %>% 
	filter(population_id %in% c(dt3$population_id)) %>% ### this is pulling out the populations which have time series > 10 years
	mutate(genus_species = str_replace(population_id, "\\_.*", "")) %>% 
	group_by(population_id) %>% 
	mutate(time_interval = date - lag(date)) %>% 
	mutate(time_interval_sd = sd(time_interval, na.rm = TRUE)) 

ctmax_max <- ctmax_data %>% 
	group_by(genus_species) %>% 
	summarise(max_ctmax = min(parameter_value)) 

	trisk <- function(x, CTmax = 35.5, alpha = 0.5){ # Thermal Risk Function
		y <- exp(-alpha * (CTmax -x))
		return(y)
	}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
p + 
	stat_function(fun = trisk, args = list(CTmax = 35, alpha = 0.5), color = "pink", size = 2) +
	stat_function(fun = trisk, args = list(CTmax = 35, alpha = 0.2), color = "purple", size = 2) +
	stat_function(fun = trisk, args = list(CTmax = 35, alpha = 0.1), color = "blue", size = 2) +
	stat_function(fun = trisk, args = list(CTmax = 35, alpha = 1), color = "orange", size = 2) +
	xlim(0, 40) + ylim(0, 6) + theme_bw() + xlab("Temperature (°C)") + ylab("Thermal risk") +
	annotate("text", label = "alpha = 0.5", x = 10, y = 2, size = 8, colour = "pink") +
	annotate("text", label = "alpha = 0.2", x = 10, y = 10000, size = 8, colour = "purple") +
	annotate("text", label = "alpha = 0.1", x = 10, y = 20000, size = 8, colour = "blue") +
	annotate("text", label = "alpha = 1", x = 10, y = 400000, size = 8, colour = "orange") +
	annotate("text", label = "CTmax = 35", x = 10, y = 10000000, size = 8, colour = "black") +
	geom_vline(xintercept = 35) + scale_y_log10()
ggsave("figures/thermal-risk-sample.png", width = 8, height = 6)



### ok this is the correct one
risk2 <- data_temp %>%  
	mutate(genus_species = str_replace(population_id, "\\_.*", "")) %>% 
	# filter(abundance > 0) %>% 
	filter(population_id %in% c(dt3$population_id)) %>% 
	left_join(., ctmax_max, by = "genus_species") %>% 
	separate(date, into = c("year", "decimal"), remove = FALSE) %>% 
	mutate(thermal_risk = exp(-0.5 * (max_ctmax - temperature))) %>% 
	mutate(log_abundance = log(abundance)) %>%
	# filter(!is.na(thermal_risk)) %>% 
	# group_by(genus_species, population_id, abundance, year, log_abundance) %>% 
	# top_frac(thermal_risk, n = 0.25) %>% 
	group_by(genus_species, population_id, year) %>% 
	summarise(thermal_risk_measure = mean(thermal_risk)) 

#### ok trying a time dependent measure

risk2_time <- data_temp %>%  
	mutate(genus_species = str_replace(population_id, "\\_.*", "")) %>% 
	# filter(abundance > 0) %>% 
	filter(population_id %in% c(dt3$population_id)) %>% 
	left_join(., ctmax_max, by = "genus_species") %>% 
	separate(date, into = c("year", "decimal"), remove = FALSE) %>% 
	mutate(within3 = ifelse(max_ctmax-3 -temperature <0, 1, 0)) %>% 
	group_by(genus_species, population_id, year) %>% 
	summarise(total_days_within3 = sum(within3))

unique(risk2_time$within3)


risk2_raw <- data_temp %>%  
	mutate(genus_species = str_replace(population_id, "\\_.*", "")) %>% 
	# filter(abundance > 0) %>% 
	filter(population_id %in% c(dt3$population_id)) %>% 
	left_join(., ctmax_max, by = "genus_species") %>% 
	separate(date, into = c("year", "decimal"), remove = FALSE) %>% 
	mutate(thermal_risk = exp(-0.5 * (max_ctmax - temperature))) %>% 
	mutate(log_abundance = log(abundance))

risk2_raw %>% 
	filter(genus_species == "Micropterus salmoides") %>% View


### ok this is the correct one, with the top 25% 
risk2b <- data_temp %>%  
	mutate(genus_species = str_replace(population_id, "\\_.*", "")) %>% 
	# filter(abundance > 0) %>% 
	filter(population_id %in% c(dt3$population_id)) %>% 
	left_join(., ctmax_max, by = "genus_species") %>% 
	separate(date, into = c("year", "decimal"), remove = FALSE) %>% 
	mutate(thermal_risk = exp(-0.5 * (max_ctmax - temperature))) %>% 
	mutate(log_abundance = log(abundance)) %>%
	filter(!is.na(thermal_risk)) %>% 
	group_by(genus_species, population_id, abundance, year, log_abundance) %>% 
	top_frac(thermal_risk, n = 0.25) %>% 
	ungroup() %>% 
	group_by(genus_species, population_id, year) %>% 
	summarise(thermal_risk_measure = mean(thermal_risk)) 

risk1 <- data_temp %>%  
	mutate(genus_species = str_replace(population_id, "\\_.*", "")) %>% 
	# filter(abundance > 0) %>% 
	filter(population_id %in% c(dt3$population_id)) %>% 
	left_join(., ctmax_max, by = "genus_species") %>% 
	separate(date, into = c("year", "decimal"), remove = FALSE) %>% 
	mutate(thermal_risk = exp(-0.1 * (max_ctmax - temperature))) %>% 
	mutate(log_abundance = log(abundance)) %>%
	filter(!is.na(thermal_risk)) 



	
	plot1 <- risk1 %>% 
		filter(genus_species == "Ambloplites rupestris") %>% 
		ggplot(aes(x = date, y = temperature)) + geom_line() +
		geom_hline(yintercept = 31.2)
		
	plot2 <- risk1 %>% 
		filter(genus_species == "Ambloplites rupestris") %>% 
		ggplot(aes(x = date, y = thermal_risk)) + geom_line(color = "pink")	
	
	risk1b <- risk1 %>% 
		filter(genus_species == "Ambloplites rupestris") %>% 
		filter(year == 2001)
	
	risk1c <- risk1 %>% 
		filter(genus_species == "Ambloplites rupestris") %>% 
		filter(year == 1998)
	
	plot2 <- risk1 %>% 
		filter(genus_species == "Ambloplites rupestris") %>% 
		ggplot(aes(x = date, y = thermal_risk, color = factor(year))) +
		geom_line()	+ylab("daily thermal risk") 
	
	plot3 <- risk1 %>% 
		filter(genus_species == "Ambloplites rupestris") %>% 
		filter(!is.na(log_abundance)) %>% 
		ggplot(aes(x = date, y = log_abundance, color = factor(year))) + geom_point(size = 5) +ylab("Log N") + geom_path(color = "grey")
	
	plot4 <- risk2 %>% 
		filter(genus_species == "Ambloplites rupestris") %>% 
		filter(!is.na(thermal_risk_measure)) %>% 
		ggplot(aes(x = year, y = thermal_risk_measure, color = factor(year))) + geom_point(size = 5) + ylab("Annual thermal risk") +
		geom_path(color = "grey")
	
	plot4b <- risk2b %>% 
		filter(genus_species == "Ambloplites rupestris") %>% 
		filter(!is.na(thermal_risk_measure)) %>% 
		ungroup() %>% 
		mutate(year = as.numeric(year)) %>% 
		ggplot(aes(x = year, y = thermal_risk_measure, color = factor(year))) + geom_point(size = 5) + ylab("Annual thermal risk top") +
		geom_path(color = "grey")
	
	library(patchwork)
	
	multi_plot <- plot1 / plot2 / plot3 / plot4 / plot4b
ggsave(plot = multi_plot, filename = "figures/sample-thermal-risk.png", width = 8, height = 10)

multi_plot2 <- plot2 / plot4
ggsave(plot = multi_plot2, filename = "figures/sample-thermal-risk-temps.png", width = 10, height = 6)
		


risk1b <- risk1 %>% 
	filter(genus_species == "Ambloplites rupestris") %>% 
	filter(!is.na(log_abundance)) %>% 
	mutate(change_in_log_N = log_abundance - lag(log_abundance))

risk2c <- risk2 %>% 
	filter(genus_species == "Ambloplites rupestris") %>% 
	filter(!is.na(thermal_risk_measure)) %>% 
	mutate(change_thermal_risk = thermal_risk_measure - lag(thermal_risk_measure))

risk_amb <- left_join(risk2c, risk1b) %>% 
	mutate(change_in_thermal_risk_previous_year = lag(change_thermal_risk))

risk_amb %>% 
	ggplot(aes(x = change_thermal_risk, y = change_in_log_N, color = factor(year))) + geom_point(size = 4) +
	geom_smooth(method = "lm", color = "black")

risk_amb %>% 
	ggplot(aes(label = year, x = change_in_thermal_risk_previous_year, y = change_in_log_N, color = factor(year))) +
	geom_point(size = 1) +
	geom_smooth(method = "lm", color = "black") + geom_text()






(### now for the whole dataset

risk2 ## has the thermal risks
View(risk1)
risk1e <- risk1 %>% 
	filter(!is.na(log_abundance)) %>% 
	group_by(population_id) %>% 
	mutate(change_in_log_N = log_abundance - lag(log_abundance)) %>% 
	mutate(time = date - lag(date))

all_risks <- left_join(risk1e, risk2) %>% 
	group_by(population_id) %>% 
	arrange(year) %>% 
	mutate(change_in_log_thermal_risk = log(thermal_risk_measure) - lag(log(thermal_risk_measure))) %>% 
mutate(change_in_thermal_risk = (thermal_risk_measure) - lag((thermal_risk_measure)))

write_csv(all_risks, "data-processed/all-thermal-risks.csv")

all_risks_time <- left_join(risk1e, risk2_time) %>% 
	group_by(population_id) %>% 
	arrange(year) %>% 
	mutate(change_in_thermal_risk_time = (total_days_within3) - lag((total_days_within3))) %>% 
	arrange(population_id, year)

write_csv(all_risks_time, "data-processed/thermal-risks-time.csv")

all_risks_time %>% 
	ggplot(aes(x = change_in_thermal_risk_time, y = change_in_log_N, color = genus_species)) + geom_point() +
	geom_smooth(method = "lm", color = "black") +
	xlab("Change in number of days per year within 3°C of CTmax") + ylab("Change in log abundance") +
	scale_color_discrete(name = "Species")
ggsave("figures/change-in-time-within-3-degrees.png", width = 16, height = 6)


acip <- data_temp %>%  
	mutate(genus_species = str_replace(population_id, "\\_.*", "")) %>% 
	# filter(abundance > 0) %>% 
	filter(population_id %in% c(dt3$population_id)) %>% 
	left_join(., ctmax_max, by = "genus_species") %>% 
	separate(date, into = c("year", "decimal"), remove = FALSE) %>% 
	mutate(within3 = ifelse(max_ctmax-3 -temperature <0, 1, 0)) %>% 
	filter(genus_species == "Acipenser brevirostrum") 


plotb <- all_risks_time %>% 
	filter(genus_species == "Acipenser brevirostrum") %>% 
	filter(!is.na(total_days_within3)) %>% 
	mutate(year = as.numeric(year)) %>% 
	ggplot(aes(x = year, y = total_days_within3, group = population_id)) + geom_point() + geom_line() +
	scale_x_continuous(breaks = c(1985:1996)) + ylab("Days per year within 3°C of CTmax")

plota <- acip %>% 
	ggplot(aes(x = date, y = temperature, color = factor(within3))) + geom_point() +
	geom_hline(yintercept = 27.6) + scale_x_continuous(breaks = c(1985:1996)) +ylab("Temperature (°C)")

plotc <- acip %>% 
	ungroup() %>% 
	filter(!is.na(abundance)) %>% 
	ggplot(aes(x = date, y = abundance, group = population_id)) + geom_point() + geom_line() +
	scale_x_continuous(breaks = c(1985:1996)) + ylab("Population abundance")


mplot <- plota / plotb / plotc
ggsave(plot = mplot, filename = "figures/within3.png", width = 8, height = 10)

risk2 %>% View

all_risks %>% 
	ggplot(aes(x = change_in_log_thermal_risk, y = change_in_log_N)) + geom_point() +
	geom_smooth(method = "lm")

	
	plot1 <- all_risks %>% 
		filter(genus_species == "Micropterus salmoides") %>% 
		ggplot(aes(x = year, y = thermal_risk_measure)) + geom_point()
	
	plot2 <- all_risks %>% 
		filter(genus_species == "Micropterus salmoides") %>% 
		ggplot(aes(x = year, y = temperature)) + geom_point()

ls2 <- long_series %>%
	left_join(., ctmax_max, by = "genus_species") %>% 
	mutate(thermal_risk = exp(-0.5 * (max_ctmax - temperature))) %>% 
	mutate(log_abundance = log(abundance)) %>% 
	separate(date, into = c("year", "decimal"), remove = FALSE) %>% 
	group_by(genus_species, population_id, abundance, year, log_abundance) %>% 
	summarise(thermal_risk_measure = mean(thermal_risk)) %>% 
	filter(!is.na(log_abundance)) 

ls3 <- ls2 %>% 
	mutate(year = as.numeric(year)) %>% 
	group_by(genus_species, population_id) %>% 
	arrange(year) %>% 
	mutate(change_in_log_N = log_abundance - lag(log_abundance)) 

write_csv(ls3, "data-processed/thermal-risk-abundances.csv")

risk2 %>% 
	ggplot(aes(x = year, y = log_abundance, group = population_id, color = log(thermal_risk_measure))) + geom_point() +
	geom_line() + 
	scale_color_viridis_c(option = "inferno", direction = -1)
ggsave("figures/log-abundance-thermal-risk.png", width = 12, height = 8)

str(ls3)

risk2 %>% 
	ungroup() %>% 
	# filter(thermal_risk_measure < 0.2) %>% 
	ggplot(aes(x = thermal_risk_measure, y = change_in_log_N, color = genus_species)) + geom_point() +
	geom_hline(yintercept = 0) +
	ylab("Change in log abundance") + xlab("Thermal risk (estimating using the lowest CTmax for each species)") +
	geom_smooth(method = "lm", color = "black")
ggsave("figures/change-log-abundance-thermal-risk-using-min-ctmax-alpha-0.5-new.png", width = 15, height = 6)

thermalriskabundances <- ls3
##arrange in order
thermalriskabundances <- thermalriskabundances %>%
	arrange(population_id, year)
## calculate differences in thermal risk between years
thermalriskabundances <- thermalriskabundances %>%
	group_by(population_id) %>%
	mutate(change_in_thermal_risk = thermal_risk_measure - lag(thermal_risk_measure))
View(thermalriskabundances)

length(unique(long_series$genus_species))
length(unique(ctmax_data$genus_species))
setdiff(unique(long_series$genus_species), unique(ctmax_data$genus_species))


View(long_series)



dt3 %>% 
	ggplot(aes(x = time_interval)) + geom_histogram()

dt3b <- dt3 %>% 
	group_by(population_id) %>% 
	tally() %>% 
	filter(n > 9)


regular_time_series <- subset(time_series_sel, time_series_sel$sdtime == 0)
long_time_series <- subset(regular_time_series, regular_time_series$npoints > 10)

### also need to get the populations for which we have more than 10 time points


trisk <- function(temp, CTmax, alpha){ # Thermal Risk Function
	y <- exp(-alpha * (CTmax - temp))
	# y <- 1 / (1 + exp(alpha * (CTmax - temp)))
	return(y)
}

# Calculate intervals between periods t-1 -> t
# this loop creates a factor "integr_interv" with different levels for each interval between
# one abundance measurement and the next. We will then integrate thermal risk over each level of "integr_interv"
# so we can estimate the average risk between one abundance measurement and the next. 

integr_interv <- numeric(nrow(pop_data))
if(!is.na(pop_data$abundance[1])) integr_interv[1] <- 1
for(i in 2:nrow(pop_data)){
	if(is.na(pop_data$abundance[i])){
		integr_interv[i] = integr_interv[i-1]
	} else {
		integr_interv[i] = integr_interv[i-1]+1
	}
}
length(integr_interv)

annual_risk <- array(NA, dim=c(length(unique(integr_interv)), length(alphas)))
colnames(annual_risk) <- alphas
for(a in 1:length(alphas)){
	point_trisk <- trisk(temp = Ta, CTmax, alpha = 1)
	annual_risk[,a] <- tapply(point_trisk, integr_interv, mean) 
}




