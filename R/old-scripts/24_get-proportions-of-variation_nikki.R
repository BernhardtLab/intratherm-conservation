## script to experiment with Fourier transforms on intratherm tmeperature data
## getting proportions of variation for intratherm project locations
library(tidyverse)
library(imputeTS)
library(gridExtra)


## Terrestrial populations:
###########################################################################################################
total <- c()
seasonal <- c()
annual <- c()
halfyearly <- c()
interannual <- c()

## read in all of the TAVG temp data for populations in intratherm:
temps <- read_csv("./data-processed/intratherm-terrestrial-temps-tavg.csv")
temps <- as.data.frame(temps)


i = 1
while (i < ncol(temps)) {
	loc <- temps[1:32143,i+1] ## chop to an even year 
	
	if(length(which(is.na(loc))) > 0 & length(which(!is.na(loc))) > 3) {
		loc <- na_seadec(loc, find_frequency = TRUE, algorithm = "interpolation") ## interpolate
		total <- append(total, get.total.SS(loc), after = length(total))
		seasonal <- append(seasonal, get.seasonal.SS(loc), after = length(seasonal))
		annual <- append(annual, get.annual.SS(loc), after = length(annual))
		halfyearly <- append(halfyearly, get.halfyearly.SS(loc), after = length(halfyearly))
		interannual <- append(interannual, get.interannual.SS(loc), after = length(interannual))
		
		i = i + 1
	}
	else if (length(which(is.na(loc))) == 32143) {
		total <- append(total, NA, after = length(total))
		seasonal <- append(seasonal, NA, after = length(seasonal))
		annual <- append(annual, NA, after = length(annual))
		halfyearly <- append(halfyearly, NA, after = length(halfyearly))
		interannual <- append(interannual, NA, after = length(interannual))
		
		i = i + 1
	}
	else {
		total <- append(total, get.total.SS(loc), after = length(total))
		seasonal <- append(seasonal, get.seasonal.SS(loc), after = length(seasonal))
		annual <- append(annual, get.annual.SS(loc), after = length(annual))
		halfyearly <- append(halfyearly, get.halfyearly.SS(loc), after = length(halfyearly))
		interannual <- append(interannual, get.interannual.SS(loc), after = length(interannual))
		
		i = i + 1
	}
}


output <- data.frame(population_id = colnames(temps)[2:ncol(temps)], seasonal, 
					 annual, interannual, total) %>%
	mutate(proportion_seasonal = seasonal/total) %>%
	mutate(proportion_annual = annual/total) %>%
	mutate(proportion_interannual = interannual/total)

terrestrial <- read.csv("~/Documents/intra-therm/data-processed/intratherm-may-2020-squeaky-clean.csv")%>%
	mutate(population_id = paste(genus_species, latitude, elevation_of_collection, longitude, sep = "_")) %>%
	filter(realm_general2 == "Terrestrial")

terrestrial <- left_join(terrestrial, output)



## Freshwater populations:
###########################################################################################################
total <- c()
seasonal <- c()
annual <- c()
halfyearly <- c()
interannual <- c()

## read in all of the TAVG temp data for populations in intratherm:
temps <- read_csv("./data-processed/intratherm-freshwater-temp-data-daily.csv")
temps <- as.data.frame(temps)


i = 1
while (i < ncol(temps)) {
	loc <- temps[,i+1] 
	
	if(length(which(is.na(loc))) > 0 & length(which(!is.na(loc))) > 3) {
		loc <- na_seadec(loc, find_frequency = TRUE, algorithm = "interpolation") ## interpolate
		total <- append(total, get.total.SS(loc), after = length(total))
		seasonal <- append(seasonal, get.seasonal.SS(loc), after = length(seasonal))
		annual <- append(annual, get.annual.SS(loc), after = length(annual))
		halfyearly <- append(halfyearly, get.halfyearly.SS(loc), after = length(halfyearly))
		interannual <- append(interannual, get.interannual.SS(loc), after = length(interannual))
		
		i = i + 1
	}
	else if (length(which(is.na(loc))) == 16071) {
		total <- append(total, NA, after = length(total))
		seasonal <- append(seasonal, NA, after = length(seasonal))
		annual <- append(annual, NA, after = length(annual))
		halfyearly <- append(halfyearly, NA, after = length(halfyearly))
		interannual <- append(interannual, NA, after = length(interannual))
		
		i = i + 1
	}
	else {
		total <- append(total, get.total.SS(loc), after = length(total))
		seasonal <- append(seasonal, get.seasonal.SS(loc), after = length(seasonal))
		annual <- append(annual, get.annual.SS(loc), after = length(annual))
		halfyearly <- append(halfyearly, get.halfyearly.SS(loc), after = length(halfyearly))
		interannual <- append(interannual, get.interannual.SS(loc), after = length(interannual))
		
		i = i + 1
	}
}


output <- data.frame(population_id = colnames(temps)[2:ncol(temps)], seasonal, 
					 annual, interannual, total) %>%
	mutate(proportion_seasonal = seasonal/total) %>%
	mutate(proportion_annual = annual/total) %>%
	mutate(proportion_interannual = interannual/total)

freshwater <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")%>%
	mutate(population_id = paste(genus_species, latitude, elevation_of_collection, longitude, sep = "_")) %>%
	filter(realm_general2 == "Freshwater")

freshwater <- left_join(freshwater, output)



## Marine populations:
###########################################################################################################
total <- c()
seasonal <- c()
annual <- c()
halfyearly <- c()
interannual <- c()

## read in all of the TAVG temp data for populations in intratherm:
temps <- read_csv("./data-processed/intratherm-marine-temp-data.csv")
temps <- as.data.frame(temps)


i = 1
while (i < ncol(temps)) {
	loc <- temps[1:13880,i+1] 
	
	if(length(which(is.na(loc))) > 0 & length(which(!is.na(loc))) > 3) {
		loc <- na_seadec(loc, find_frequency = TRUE, algorithm = "interpolation") ## interpolate
		total <- append(total, get.total.SS(loc), after = length(total))
		seasonal <- append(seasonal, get.seasonal.SS(loc), after = length(seasonal))
		annual <- append(annual, get.annual.SS(loc), after = length(annual))
		halfyearly <- append(halfyearly, get.halfyearly.SS(loc), after = length(halfyearly))
		interannual <- append(interannual, get.interannual.SS(loc), after = length(interannual))
		
		i = i + 1
	}
	else if (length(which(is.na(loc))) == 13880) {
		total <- append(total, NA, after = length(total))
		seasonal <- append(seasonal, NA, after = length(seasonal))
		annual <- append(annual, NA, after = length(annual))
		halfyearly <- append(halfyearly, NA, after = length(halfyearly))
		interannual <- append(interannual, NA, after = length(interannual))
		
		i = i + 1
	}
	else {
		total <- append(total, get.total.SS(loc), after = length(total))
		seasonal <- append(seasonal, get.seasonal.SS(loc), after = length(seasonal))
		annual <- append(annual, get.annual.SS(loc), after = length(annual))
		halfyearly <- append(halfyearly, get.halfyearly.SS(loc), after = length(halfyearly))
		interannual <- append(interannual, get.interannual.SS(loc), after = length(interannual))
		
		i = i + 1
	}
}


output <- data.frame(population_id = colnames(temps)[2:ncol(temps)], seasonal, 
					 annual, interannual, total) %>%
	mutate(proportion_seasonal = seasonal/total) %>%
	mutate(proportion_annual = annual/total) %>%
	mutate(proportion_interannual = interannual/total)

marine <- read.csv("./data-processed/intratherm-may-2020-squeaky-clean.csv")%>%
	mutate(population_id = paste(genus_species, latitude, elevation_of_collection, longitude, sep = "_")) %>%
	filter(realm_general2 == "Marine")

marine <- left_join(marine, output)


all <- rbind(terrestrial, marine, freshwater)

write.csv(all, "./data-processed/intratherm-with-proportions-of-var.csv", row.names = FALSE)


# FUNCTIONS:
#############################

## Parseval’s theorem: the variance of the time series equals the sum of the squared absolute values of the output of the DFT
get.total.SS = function(series){
	L = length(series)
	# Fourier transform series and capture phases and amplitudes of series
	dft <-  fft(series) / L
	amp <- sqrt(Im(dft[-1]) ^ 2 + Re(dft[-1]) ^ 2)	# omit this first term
	amp <- amp[1:(L / 2)]				# snag just first half of amplitudes
	# squares of each
	ampt = (2 * amp) ^ 2 / 2
	X = sum(ampt, na.rm = TRUE)
	return(X)
}

## find out what portion of variance is seasonal, annual and interannual:
get.seasonal.SS = function(series){
	L = length(series)
	
	dft<-  fft(series)/L
	amp<-sqrt(Im(dft[-1])^2+Re(dft[-1])^2)	
	amp<-amp[1:(L/2)]			
	
	annual <- which(amp == max(amp))
	
	amp <- amp[annual+1:length(amp)] ## subset to amplitudes between 1/365 and year and 1 day
	# squares of each
	ampt = (2*amp)^2/2
	X = sum(ampt, na.rm = TRUE)
	return(X)
}

get.annual.SS = function(series){
	L = length(series)
	dft<-  fft(series)/L
	amp<-sqrt(Im(dft[-1])^2+Re(dft[-1])^2)	
	amp<-amp[1:(L/2)]			
	
	annual <- which(amp == max(amp))
	
	amp <- amp[annual] ## subset to amplitudes between 1/365 and year and 1 day
	# squares of each
	ampt = (2*amp)^2/2
	X = sum(ampt, na.rm = TRUE)
	return(X)
}

get.interannual.SS = function(series){
	L = length(series)
	
	dft<-  fft(series)/L
	amp<-sqrt(Im(dft[-1])^2+Re(dft[-1])^2)	
	amp<-amp[1:(L/2)]			
	
	annual <- which(amp == max(amp))
	
	amp <- amp[1:(annual-1)] ## subset to amplitudes between 1/365 and year and 1 day
	# squares of each
	ampt = (2*amp)^2/2
	X = sum(ampt, na.rm=TRUE)
	return(X)
}

get.halfyearly.SS = function(series){
	L = length(series)
	
	dft<-  fft(series)/L
	amp<-sqrt(Im(dft[-1])^2+Re(dft[-1])^2)	
	amp<-amp[1:(L/2)]			
	
	annual <- which(amp == max(amp))
	
	amp <- amp[annual*2] ## subset to amplitudes between 1/365 and year and 1 day
	# squares of each
	ampt = (2*amp)^2/2
	X = sum(ampt, na.rm=TRUE)
	return(X)
}