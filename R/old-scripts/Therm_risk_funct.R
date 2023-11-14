require(zoo)
require(tidyverse)

dir <- "..." # write path to working directory
data_temp <- read.csv(paste0(dir, "population-dynamics-with-temp-ts.csv"))
data_temp <- read_csv("~/Documents/too-big-for-github/population-dynamics-with-temp-ts.csv")



head(data_temp)
pop_ids <- unique(data_temp$population_id)
length(pop_ids)



dt2 <- data_temp %>% 
	mutate(log_n = log(abundance))
dt3 <- dt2 %>% 
filter(!is.na(abundance)) %>%
	group_by(population_id) %>% 
	mutate(time_interval = date - lag(date)) 


#### Time series
p=2
pop_data <- subset(data_temp, data_temp$population_id == pop_ids[p])
plot(abundance ~ date, pop_data, pch=20) # irregular, n=4

p=6
pop_data <- subset(data_temp, data_temp$population_id == pop_ids[p])
plot(abundance ~ date, pop_data, pch=20) # irregular

pop_data <- subset(data_temp, data_temp$population_id == "Esox lucius_54.3333333333333_-2.91666666666667_1823")
plot(abundance ~ date, pop_data, pch=20) # regular

#### SELECT APPROPRIATE TIME SERIES ####
# This code computes time intervals and length (n years) of each time series
time_series_sel <- as.data.frame(array(NA, dim=c(length(pop_ids), 3)))
colnames(time_series_sel) <- c("popID", "sdtime", "npoints")
time_series_sel[,1] <- pop_ids
View(time_series_sel)


for(p in 1:length(pop_ids)){
  pop_data <- subset(data_temp, data_temp$population_id == pop_ids[p])
  pop_data$abundance[which(pop_data$abundance == 0)] <- NA # remove the zeros
  abundance <- pop_data$abundance
  time <- pop_data$date
  
  ## Regularly spaced?
  abund_data <- abundance[which(!is.na(abundance))]
  abund_time <- time[which(!is.na(abundance))]
  regul <- numeric(length(abund_time))
  for(i in 2:length(regul)){
    regul[i-1] <- abund_time[i]-abund_time[i-1]
  }
  time_series_sel[p,2] <- sd(regul[-length(regul)])
  
  ## Number of years?
  year <- as.numeric(substr(time, 1, 4))
  
  time_series_sel[p,3] <- length(unique(year[which(!is.na(abundance))]))
  print(round(100* p / length(pop_ids),2))
}

# save(time_series_sel, file=paste0(dir, "time_series_sel.RData"))
# To avoid runing the code above, open:
load(file=paste0(dir, "time_series_sel.RData"))


alpha
#### COMPUTING THERMAL RISK ####
trisk <- function(temp, CTmax, alpha){ # Thermal Risk Function
  y <- exp(-alpha * (CTmax - temp))
  # y <- 1 / (1 + exp(alpha * (CTmax - temp)))
  return(y)
}

CTmax=35.5
curve(trisk(x, CTmax=CTmax, alpha=1), -20, 50, add=F, ylim=c(0,0.4), ylab="Thermal risk", xlab="Temperature (°C)",
      lwd=2, cex.lab=1.5, cex.axis=1)
abline(v=CTmax, lty=3)

pop_data <- subset(data_temp, data_temp$population_id == pop_ids[1])
hist(pop_data$temperature, freq=F, add=T)

# Subset time series: only regular (sd of sampling period = 0) and with > 10 years
regular_time_series <- subset(time_series_sel, time_series_sel$sdtime == 0)
long_time_series <- subset(regular_time_series, regular_time_series$npoints > 10)

long_time_series$pop_id_num <- rownames(long_time_series)

nrow(long_time_series)

alphas <- c(0.1, 0.5, 1, 1.5)
p <- 2

for(p in 1:nrow(long_time_series)){
  # Select time series 
  pop_data <- subset(data_temp, data_temp$population_id == long_time_series$popID[p])
  pop_data$abundance[which(pop_data$abundance == 0)] <- NA # remove zeros
  
  abundance <- pop_data$abundance
  time <- pop_data$date
  Ta <- pop_data$temperature
  
  abund_data <- abundance[which(!is.na(abundance))]
  abund_time <- time[which(!is.na(abundance))]
  
  # Pop sampling intervals
  regul <- numeric(length(abund_time))
  for(i in 2:length(regul)){
    regul[i-1] <- abund_time[i]-abund_time[i-1]
  }
  regul <- regul[-length(regul)]
  
  # Compute change in abundance between periods t-1 -> t
  abundance_change <- numeric(length(abund_data)-1)
  for(i in 2:length(abund_data)){
    abundance_change[i-1] <- (abund_data[i] - abund_data[i-1]) # / (abund_data[i-1] * regul[i-1])
    # Alternatively, we can estimate pop growth rate diving the change in abundance by "(abund_data[i-1] * regul[i-1])"
  }
  
  # Extract CTmax from intratherm
  ctmax_data <- read_csv("data-processed/intratherm-with-elev.csv")
  ctmax_data <- subset(ctmax_data, ctmax_data$parameter_tmax_or_tmin == "tmax")
  
  sp_abund <- sub("\\_.*", "", pop_data$population_id[1]) 
  ctmax_id <- which(str_detect(ctmax_data$genus_species, patter=sp_abund, negate = FALSE))
  if(length(ctmax_id)>0){
    CTmax=max(ctmax_data$parameter_value[ctmax_id])
  }else{
    CTmax=NA # For one (?) of the species, intratherm only has tmin (revise)
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
  
  # Now we integrate thermal risk at each level of "integr_interv" using tapply, and for each value of alpha
  annual_risk <- array(NA, dim=c(length(unique(integr_interv)), length(alphas)))
  colnames(annual_risk) <- alphas
  for(a in 1:length(alphas)){
    point_trisk <- trisk(temp=Ta, CTmax, alphas[a])
    annual_risk[,a] <- tapply(point_trisk, integr_interv, mean) 
  }

  
  # also estimate the mean Ta for each integr_interv period
  annual_Ta <- tapply(Ta, integr_interv, mean) 
  
  # and the P that T > CTmax over the period
  pCTmax_funct <- function(x){
    h1 <- hist(x, breaks=seq(-30,60,length.out = 100), plot=F)
    y = mean(h1$density[h1$mids > CTmax])
    return(y)
  }
  pCTmax <- tapply(Ta, integr_interv, pCTmax_funct)
  
  # If the abundance time series starts with NA, the first level of the integration interval will be 0,
  # and we will not use this level to calculate the relationship between abundance change vs integr thermal risk.
  # Check and exclude the first level if any(integr_interv==0):
  if(any(integr_interv==0)){
    annual_risk <- annual_risk[-1,]
    annual_Ta <- annual_Ta[-1]
    pCTmax <- pCTmax[-1]
  }
  
  # If the abundance time series is not even, the abundance_change time series will have one less value than the annual_risk
  # time series, so we will have to exclude the last value of annual_risk
  if(length(annual_risk) != length(abundance_change)){
    annual_risk <- annual_risk[-nrow(annual_risk),]
    annual_Ta <- annual_Ta[-length(annual_Ta)]
    pCTmax <- pCTmax[-length(pCTmax)]
  }
  
  
  result_tseries <- data.frame(abundance_change,annual_risk,annual_Ta,pCTmax,time=abund_time[-1],CTmax)
  
  # This loop creates one file for each time series (total 129)
  name <- pop_data$population_id[1]
  save(result_tseries,file=paste0("data-processed/selected-time-series/",name,".csv"))
  
  print(round(100*p/nrow(long_time_series),2))
}

#### WITHIN- TIME SERIES ANALYSES ####
files <- list.files(path=paste0("data-processed/selected-time-series/"))

results <- as.data.frame(array(NA,dim=c(length(files), 5)))
results[,1] <- files; colnames(results) <- c("popID", "exceedCTmax", "Beta_risk", "Beta_pCTmax", "Pvalue")
for(i in 1:length(files)){
  load(file=paste0("data-processed/selected-time-series/",files[i]))
  
  CTmax <- result_tseries$CTmax[1]
  # Compute abund change vs thermal risk (for alpha = 1)
  if(!is.na(CTmax)){
    mod_trisk <- lm(abundance_change ~ (X1), result_tseries)
    mod_temp <- lm(abundance_change ~ (pCTmax), result_tseries)
    results[i,2] <- length(which(result_tseries$pCTmax>0))
    results[i,3] <- coef(mod_trisk)[2]
    results[i,4] <- coef(mod_temp)[2]
    results[i,5] <- summary(mod_trisk)[[4]][2,4]
  }
}

which_significant <- which(results$Pvalue < 0.05)

hist(results$Beta_risk, breaks=20, main=NULL, freq=T, ylab="Frequency", xlab="Slope - Pop size vs Thermal risk")

length(which_significant) / nrow(results) # proportion of significant relationships
length(which(results$Beta_risk<0)) / nrow(results) # half of the relationships abund change vs risk were negative
length(which(results$Beta_risk[which_significant]<0)) / length(results$Beta_risk[which_significant]) # most of the significant relationships are negative

# put significant time series together
all_tseries <- c(NA,NA,NA)
i = 22
load(file=paste0("data-processed/selected-time-series/",files[i]))
all_tseries <- rbind(all_tseries, cbind(result_tseries$abundance_change, result_tseries$X1, rep(1,nrow(result_tseries)))) 
for(i in which_significant[-1]){
  load(file=paste0("data-processed/selected-time-series/",files[i]))
  if(!is.na(result_tseries$CTmax[1])){
    all_tseries <- rbind(all_tseries, cbind(result_tseries$abundance_change, result_tseries$X1, rep(i,nrow(result_tseries)))) 
  }
}

require(ggplot2)
all_tseries <- as.data.frame(all_tseries)
all_tseries <- all_tseries[-1,]
colnames(all_tseries) <- c("abundance_change","X1","ID")

ggplot(all_tseries, aes(y=(abundance_change), x=log(X1), colour=as.factor(ID))) + theme_classic() +
  geom_point() + geom_smooth(method="lm") + theme(legend.position = "none") +
  ylab("Change in population size") + xlab("Thermal risk")

ggplot(all_tseries, aes(y=(abundance_change), x=(X1))) + theme_classic() +
  geom_point() + geom_smooth(method="lm", col="black") + 
  ylab("Change in population size") + xlab("Thermal risk") +
  theme(legend.position = "none", strip.background = element_blank(), 
        strip.text.x = element_blank(), axis.title = element_text(size=14),
        axis.text = element_text(size=12)) +
  facet_wrap(~ID, scales="free")

ggplot(all_tseries, aes(y=(abundance_change), x=log(X1))) + theme_classic() +
  geom_point() + geom_smooth(method="lm", col="black") + 
  ylab("Change in population size") + xlab("log Thermal risk") +
  theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank()) +
  facet_wrap(~ID, scales="free")


#### BETWEEN- TIME SERIES ANALYSES ####

# subset of long (> 10 years) time series
long_time_series <- subset(time_series_sel, time_series_sel$npoints > 10)
nrow(long_time_series)

result_tseries <- data.frame("name"=numeric(nrow(long_time_series)), "CTmax"=numeric(nrow(long_time_series)), 
                             "nyears"=numeric(nrow(long_time_series)), 
                             "tendency"=numeric(nrow(long_time_series)),"riskchange"=numeric(nrow(long_time_series)),
                             "pvaltend"=numeric(nrow(long_time_series)),"pvalrisk"=numeric(nrow(long_time_series)),
                             "alpha0.1"=numeric(nrow(long_time_series)), "alpha0.5"=numeric(nrow(long_time_series)), 
                             "alpha1"=numeric(nrow(long_time_series)), "alpha1.5"=numeric(nrow(long_time_series)))

alphas <- c(0.1, 0.5, 1, 1.5)
for(p in 1:nrow(long_time_series)){
  # Select time series 
  pop_data <- subset(data_temp, data_temp$population_id == long_time_series$popID[p])
  pop_data$abundance[which(pop_data$abundance == 0)] <- NA
  
  abundance <- pop_data$abundance
  time <- pop_data$date
  Ta <- pop_data$temperature
  
  abund_data <- abundance[which(!is.na(abundance))]
  abund_time <- time[which(!is.na(abundance))]
  
  # Extract CTmax from intratherm
  # ctmax_data <- read.table("intratherm-with-elev.txt", header=T, sep=",")
  ctmax_data <- read_csv("data-processed/intratherm-with-elev.csv")
  ctmax_data <- subset(ctmax_data, ctmax_data$parameter_tmax_or_tmin == "tmax")
  
  sp_abund <- sub("\\_.*", "", pop_data$population_id[1]) 
  ctmax_id <- which(str_detect(ctmax_data$genus_species, patter=sp_abund, negate = FALSE))
  if(length(ctmax_id)>0){
    CTmax=max(ctmax_data$parameter_value[ctmax_id])
  }else{
    CTmax=NA
  }
  
  # Integrate annual thermal risk for each alpha value
  annual_risk <- numeric(length(alphas))
  names(annual_risk) <- alphas
  for(a in 1:length(alphas)){
    point_trisk <- trisk(temp=Ta, CTmax, alphas[a])
    annual_risk[a] <- mean(point_trisk) 
  }
  
  # Change in risk (for alpha = 1)
  point_trisk <- trisk(temp=Ta, CTmax, 1)
  if(!is.na(CTmax)){
    modrisk <- lm(log(point_trisk) ~ time) ## Note that we need to take log here to be able to analyse the change in thermal risk with a linear model
    riskchange = coef(modrisk)[2]
    pvalrisk = summary(modrisk)[[4]][2,4]
  }else{
    riskchange = NA
  }
  
  # Global tendency of the t series
  modtseries <- lm(log(abund_data) ~ abund_time)
  
  # Results
  name <- pop_data$population_id[1]
  year <- substr(abund_time, 1,4)
  nyears <- length(unique(year))
  result_tseries[p,1] <- name
  result_tseries[p,2] <- CTmax
  result_tseries[p,3:11] <- c(nyears, coef(modtseries)[2], riskchange, summary(modtseries)[[4]][2,4], pvalrisk, annual_risk)
  
  
  print(round(100*p/nrow(long_time_series),2))
}

hist(result_tseries$tendency, freq=F); abline(v=0, lwd=4)
length(which(result_tseries$pvaltend<0.05)) / nrow(result_tseries) # proportion of t series with a significant trend
hist(result_tseries$tendency[which(result_tseries$pvaltend<0.05)], add=T, breaks=40, col="blue", freq=F)
# approx 35% of the t series show a significant trend, either increase or decline

hist(result_tseries$riskchange, freq=F); abline(v=0, lwd=4)
length(which(result_tseries$pvalrisk<0.05)) / nrow(result_tseries) # proportin of t series in which therm risk has changed significantly
hist(result_tseries$riskchange[which(result_tseries$pvalrisk<0.05)], add=T, breaks=20, col="blue", freq=F)

length(which(result_tseries$riskchange[which(result_tseries$pvalrisk<0.05)]>0))

# approx 51% of the t series show a change in therm risk, and the slope is generally positive (therm risk increases)

require(ggplot2)
ggplot(result_tseries, aes(y=tendency, x=(alpha1))) + geom_point() + theme_classic() +
  ylab("Population trend") + xlab("Integrated thermal risk") +
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12))

ggplot(result_tseries, aes(y=tendency, x=log(alpha1))) + geom_point() + theme_classic() +
  ylab("Population trend") + xlab("log Integrated thermal risk") +
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12))

mod1 <- lm(tendency ~ log(alpha1), result_tseries)
summary(mod1)
# Across t series, trends are not related to the average thermal risk throughout the series

## Relationship pop tendency ~ risk change
# All t series
plot(tendency ~ (riskchange), result_tseries)
mod_tot <- lm(tendency ~ (riskchange), result_tseries)
summary(mod_tot)
# Across all t series, the change in abundance is not related to the change in thermal risk

# Only t series with significant increase in thermal risk
result_tseries_increase <- result_tseries[which(result_tseries$riskchange > 0),]

points(tendency ~ riskchange, result_tseries_increase[which(result_tseries_increase$pvalrisk < 0.05),], pch=20, col="blue")
mod_sign <- lm(tendency ~ riskchange, result_tseries_increase[which(result_tseries_increase$pvalrisk < 0.05),])
summary(mod_sign)
# t series for which change in thermal risk is significant and positive, do not show a significant relationship
# between change in abudance and change in therm risk

ggplot(result_tseries, aes(y=tendency, x=riskchange)) + geom_point() + 
  geom_point(result_tseries_increase, mapping=aes(y=tendency, x=riskchange), colour="blue") + 
  theme_classic() +
  ylab("Population trend") + xlab("Change in thermal risk") +
  theme(axis.title = element_text(size=14), axis.text = element_text(size=12))


