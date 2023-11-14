######## Install nichemapR from github
require(devtools)
install_github("mrke/NicheMapR")
require(NicheMapR)
get.global.climate(folder="path") # download global climate database

######## Climate data with NicheMapR
loc <- c(x=-3.70256, y=40.4165) # define lon / lat

# NichemapR has maaany parameters, but for what we need, I think we just need to include: 
# location, minimum and maximum shade levels (%), and the reference height at which temp is simulated, which is related to the size of the animal (here 10 cm)
# I estimate max shade levels of the canopy using the LAI index (below), but just as an example this is for 100% shade:

micro <- micro_global(loc = loc, minshade = 0, maxshade = 100, Usrhyt = 0.10) 
micro_sun <- as.data.frame(micro$metout) # meteorological conditions in the sun 
micro_shade <- as.data.frame(micro$shadmet) # and in the shade
micro_soil <- as.data.frame(micro$soil) # if we want soil T (see operative temperatures below!)

head(micro_sun)
# most important here is: DOY, TIME (min), TALOC (Air temperature), RHLOC (relative humidity), VLOC (wing speed), and SOLR (radiation)

# this is the maximum temperature (air T in the sun, at noon, in the warmest month of the year)
max(micro_sun$TALOC) 

# This is how an average day of july looks like in Madrid
month = 7
day = unique(micro_sun$DOY)[month] # this is the day of the year corresponding to month 7

AIRT_sun <- subset(micro_sun, DOY == day)$TALOC
AIRT_shade <- subset(micro_shade, DOY == day)$TALOC

plot(AIRT_sun, ylab="Air temperature, ºC", xlab="Time, hour") # temperature in the sun (month 7)
points(AIRT_shade, pch=20) # temperature in the shade

############################################

######## More realistic shade levels
# We can either use max T in the sun (as above), or calculate "the actual level of shade" in that location using the LAI index,
# and then introduce it in NicheMapR as "maxshade".

require(ncdf4) 
require(raster)
require(rgdal)
require(GeoLight)
 
loc <- c(x=-3.70256, y=40.4165) # lon / lat

## Load LAI data 
nc_data <- nc_open("LAI_mean_monthly_1981-2015.nc4")
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
lai.array <- ncvar_get(nc_data, "LAI")

resolution = 1 # set resolution of the LAI layer to match with your data (e.g., 1º x 1º)

LAI_vector <- numeric(12)
for(month in 1:12){
  # extract LAI from layer
  if(month>=8){m=month-7}else{m=month+5} # 1: aug, 2:sept....
  lai.slice <- lai.array[, , m]
  r <- raster(t(lai.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  r <- aggregate(r, fact=resolution/res(r)[1])
  r <- flip(r, direction='y')
  LAI <- extract(r, cellFromXY(r, loc))
  
  LAI_vector[month] <- LAI 
}

LAI_vector # LAI values every month in our location

# Functions to calculate the proportion of light that makes it through the canopy (Campbell & Norman 1998, in Algar et al. 2018 GEB)
shade <- function(ac=0.8, z, LAI){
  Kbc <- sqrt((1 + tan(z*pi/180)^2) / 2.00132)
  omega_p <- exp(-sqrt(ac) * Kbc * LAI)
  return((1-omega_p)*100)
}

zenith <- function(sun, lon, lat){
  lon <- as.numeric(lon); lat <- as.numeric(lat)
  rad <- pi/180
  hourAngle <- sun$solarTime + lon - 180
  cosZenith <- (sin(rad * lat) * sun$sinSolarDec + cos(rad * 
                                                         lat) * sun$cosSolarDec * cos(rad * hourAngle))
  cosZenith[cosZenith > 1] <- 1
  cosZenith[cosZenith < -1] <- -1
  acos(cosZenith)/rad
}

shade_vector <- numeric(12)
for(month in 1:12){
  # compute zenith angle value
  s <- solar(as.POSIXct(paste0("2000-",month,"-15"," ",1:24,":","00")))
  zen <- zenith(s, lon=loc[1], lat=loc[2])
  zen[zen>90] <- 90 # hourly zenith angles
  
  # extract LAI from vector
  LAI <- LAI_vector[month]
  
  shade_vector[month] <- mean(shade(0.8, zen, LAI)) # daily average of shade levels
}

shade_vector # Monthly max shade levels (%) in our location

######## Introducing shade in NicheMapR
# Now we need to introduce the level of shade in the "maxshade" argument of micro_global. We need to run micro_global 12 times 
# (1 for each month) because one cannot introduce different shade levels for each month in NicheMapR

AIRT_sun <- AIRT_shade <- array(NA, dim=c(24, 12))
for(month in 1:12){
  maxshade <- shade_vector[month]
  micro <- micro_global(loc = loc, minshade = 0, maxshade = maxshade, Usrhyt = 0.10) 
  micro_sun <- as.data.frame(micro$metout)
  micro_shade <- as.data.frame(micro$shadmet)
  
  day = unique(micro_sun$DOY)[month] 
  
  AIRT_sun[,month] <- subset(micro_sun, DOY == day)$TALOC
  AIRT_shade[,month] <- subset(micro_shade, DOY == day)$TALOC
}

AIRT_sun # This array contains air temperatures in the sun in 12 columns (months) with 24 rows (hours)

month = 7

plot(AIRT_sun[,month], ylab="Air temperature, ºC", xlab="Time, hour") # temperature in the sun (month 7)
points(AIRT_shade[,month], pch=20) # temperature in the shade

# The temperature in the sun (0% of shade) looks exactly the same, but the temperaure in the shade is 
# quite higher than before (where we assumed 100% of shade)

# If we want the max T of the warmest month, taking canopy cover into account:
max(AIRT_shade)

############################################ OPERATIVE TEMPERATURES
# This is the function used by Algar et al. (2018) (based on Buckley 2007) to compute operative temperatures of lizards in the sun. 
# We can parameterize this function using NicheMapR (for air T, soil T, wind velocity, and solar radiation) and our body size data 
# (body length)

Te_function <- function(S,    # Solar radiation (Wm-2)
                        Ta,   # Air temperature (ºC)
                        Tg,   # Ground temperature (ºC)
                        v,    # Wind velocity (m/s)
                        d,    # Body length (m)
                        alpha_lw=0.965, # Skin absorbance (long wave) (Buckley 2007)
                        alpha_s=0.9,    # Skin absorbance (short wave) (Buckley 2007)
                        eps=0.965,      # Skin emissinvity (Buckley 2007)
                        Fa=0.5,         # View factor from sky (Algar et al. 2018)
                        Fg=0.5){        # View factor from the ground (Algar et al. 2018)
  
  ## Emission and absorption of long-wave radiation
  sigma = 5.67e-8 # W m-2 K-4, Stefan-Boltzmann constant
  eps_sky = 9.2e-6 * (Ta + 273)^2 # Clear sky emissivity of long wave radiation (Buckley 2007)
  La = eps_sky * sigma * (Ta + 273)^4 # long wave radiation from the sky
  
  eps_g = 0.965 # emisivity of the soil surface (Algar et al. 2018)
  Lg = eps_g * sigma * (Tg + 273)^4 # long wave radiation from the ground
  
  Rlw = alpha_lw * (Fa * La + Fg * Lg) # absorbed long-wave radiation
  
  ## Absorption of short-wave solar radiation
  # This is a simplification of Lauren's Buckley's model, which is focused on the geometry of a lizard. 
  # Here we just assume that the the upper half of the body received direct and scattered solar radiation
  Rsol = alpha_s * Fa * S 
  
  ## Operative temperature:
  cp = 29.3 # J mol-1 K-1, specific heat of the air
  Te = Ta + (Rsol + Rlw - eps * sigma * (Ta + 273)^4) / (4 * sigma * (Ta + 273)^3 + cp*(1.4 + 0.135*sqrt(v/d)))
  
  return(Te)
}

###### Using NicheMapR to compute body temperatures (from the data we simulated before):

month = 7
day = unique(micro_sun$DOY)[month]

AIRT <- subset(micro_sun, DOY == day)$TALOC # air T 
SOLR <- subset(micro_sun, DOY == day)$SOLR # solar radiation
SOILT <- subset(micro_soil, DOY == day)$D0 # solar radiation
VLOC <- subset(micro_sun, DOY == day)$VLOC  # wind speed

d = 0.05 # body length (m)
Te <- Te_function(S=SOLR, Ta=AIRT, Tg=SOILT, v=VLOC, d=d)

plot(Te, ylab="Operative temperature, ºC", xlab="Time, hour") # Operative temperatures in the sun (month 7)
points(AIRT, col="blue")

# The operative temperature is going to be higher than air T, especially when solar radiation is high. 

############################################ AIR / OPERATIVE TEMPERATURES -INTRATHERM
require(NicheMapR)

# This function calculates operative temperatures (ºC) of both dry- and wet-skinned animals.

Temodel_ewl <- function(abs=0.9,  # Skin absorbance to short-wave radiation
                        M,        # Body mass (g)
                        A,        # Surface area of skin (m2)
                        L,        # Body length (m)
                        Fo_a=2/3, # View factor from the sky (if exposed)
                        r,        # Skin resistance to water loss (s m-1)
                        S,        # Solar radiation (Wm2)
                        v,        # Wind speed (m s-1)
                        Ta,       # Air temperature (ºC)
                        Tg,       # Soil temperature (ºC)
                        RH,       # Relative humidity (%)
                        exposed=T)# Exposed or burrowed?
{
    ## Convection 
    kf =  1.5207e-11*(Ta+273)^3 - 4.8574e-08*(Ta+273)^2 + 1.0184e-04*(Ta+273) - 3.9333e-04 # Thermal conductivity of air (W m-1 ºC-1)
    Pd = 101325   # Atmosferic pressure (Pa) 
    rho_da = Pd / (287.05 * (Ta+273))  # Density of dry air (kg m-3)
    Pv = exp(77.3450 + 0.0057 * (Ta+273) - 7235 / (Ta+273)) / (Ta+273)^8.2  # Water vapor pressure (Pa)
    RH_prop = RH/100 # ransform relative humidity % to proportion 0-1
    x = RH_prop * 0.62198 * Pv / Pd # Humidity ratio (mass_air_water / mas_dry_air)
    rho = rho_da * (1 + x) / (1 + 1.609 * x) # Density of moist air (Kg m-3)
    mu = 1.458e-6 * (Ta+273)^(3/2) / ((Ta+273) + 110.4)  # Air dynamic viscosity (Pa s)
    cp = 1000*(-3.46607e-20*Ta^6+9.121847e-17*Ta^5+1.079642e-13*Ta^4-5.714484e-10*Ta^3+5.773354e-07*Ta^2+8.976385e-06*Ta+1.00528623891845) # Specific heat capacity air (J Kg-1 ºC-1)
    
    Re = rho * v * L / mu # Reynolds number
    Nu = 0.34 * Re^0.6 # Nusselt number (Mitchell 1976; Stevenson 1985)
    hc = Nu * kf/L # Convection coefficient
    
    ## Conduction 
    ks = 0.5 # thermal conductivity of the skin (W m-1 ºC-1) (Porter et al. 1973)
    ts = 0.025 * ((M/1000) / (3.1416 * 1000))^0.2 # Skin thikness (Stevenson 1985)
    hg = ks / ts # Conduction coefficient (W m-2 ºC-1)
    
    ## Thermal radiation
    sigma = 5.67e-8 # Stefan-Boltzmann constant (W m-2 K-4)
    eps = 0.95 # Emissivity of thermal radiation
    
    Ra = 4 * eps * sigma * (Ta+273)^3 # IR coefficients for the sky and soil surface (Bakken et al. 1975)
    Rg = 4 * eps * sigma * (Tg+273)^3
    
    ## Evaporative cooling
    ps_a = exp(77.3450 + 0.0057 * (Ta+273) - 7235 / (Ta+273)) / (Ta+273)^8.2  # Air vapor pressure (Pa)
    rho_saturated = 2.2 * ps_a / (Ta+273) # Trasform into density (g m-3)
    
    EWL = (rho_saturated - RH_prop * rho_saturated) / r  # Evaporative water loss (g s-1) (Spotila and Berman 1976)
    Qewl = 2257 * EWL # Evaporative cooling (W) = latent heat of vaporization of water (J g-1) x EWL (g s-1)
    
    ## Operative temperatures
    if(exposed){
      # If the animal is exposed we compute the heat balance considering absorbed solar radiation, convective cooling, 
      # exchange of thermal radiation with the ground and sky, conduction with the soil surface, and evaporative cooling. 
      Te <- (Fo_a * abs * S + Fo_a * Ta * (Ra + hc) + (1-Fo_a) * Tg * (Rg + hg) - Qewl) / (Fo_a * (Ra + hc) + (1-Fo_a) * (Rg + hg))
    } else { 
      # If it is burrowed, heat will only be exchanged by conduction with substrate
      Fo_a = 0 # the whole body is in contact with the substrate
      Te <- ((1-Fo_a) * Tg * (Rg + hg) - Qewl) / ((1-Fo_a) * (Rg + hg))
    }
  return(Te)
}

# Example: Frog of 5cm length
L = 5 / 100 # body length (m)
M = 10^(-4.298)*(L*1000)^3.181 # Body mass (Santini et al. 2018)
A = 9.8537*M^0.6745 * 1e-4 # Skin surface area (Klein et al. 2016)
r = 300 # resistance to water loss (ca. 300 sm-1 for amphibians, and 6e5 sm-1 for lizards and other wet-skinned ectotherms)

Temodel_ewl(abs=0.9, M=M, A=A, L=L, Fo_a=0.5, r=r, S=400, v=0.5, Ta=25, Tg=30, RH=30, exposed=T) # Exposed to the sun (summer at 12pm in a warm place) 
Temodel_ewl(abs=0.9, M=M, A=A, L=L, Fo_a=0.5, r=r, S=400, v=0.5, Ta=25, Tg=30, RH=30, exposed=F) # Burrowed

### INTRATHERM 
data <- read.csv(".../intratherm-may-2020-squeaky-clean.txt", header = T, sep=",")
data_terr <- subset(data, realm_general2=="Terrestrial")

loc <- data.frame("ID"=data_terr$intratherm_id, "LON"=data_terr$longitude, "LAT"=data_terr$latitude, "INACTIV"=data_terr$season_inactive)

# Wrong class?
data_terr$class[156] <- "Insecta"
data_terr$class[157] <- "Insecta"

# Locations and activity times
loc <- data.frame("ID"=data_terr$intratherm_id, "LON"=data_terr$longitude, "LAT"=data_terr$latitude, "INACTIV"=data_terr$season_inactive)

# Months of activity
activ <- array(NA, dim=c(nrow(loc),12))
for(i in 1:nrow(loc)){
  if(loc$LAT[i] > 0){ # Northern hemisphere
    if(loc$INACTIV[i] == "Spring and Winter"){ # i.e., active in summer and fall
      activ[i,] <- c(NA, NA, NA, NA, NA, 6, 7, 8, 9, 10, 11, NA)
    }
    if(loc$INACTIV[i] == "Summer"){ # i.e., active in winter, spring and fall
      activ[i,] <- c(1, 2, 3, 4, 5, NA, NA, NA, 9, 10, 11, 12)
    }
    if(loc$INACTIV[i] == "Winter"){ # i.e., active in summer, spring and fall
      activ[i,] <- c(NA, NA, 3, 4, 5, 6, 7, 8, 9, 10, 11, NA)
    }
    if(loc$INACTIV[i] == "Oct-Apr"){ # i.e., active in 5, 6, 7, 8, 9
      activ[i,] <- c(NA, NA, NA, NA, 5, 6, 7, 8, 9, NA, NA, NA)
    }
    if(loc$INACTIV[i] == "Summer and Winter"){ # i.e., active spring and fall
      activ[i,] <- c(NA, NA, 3, 4, 5, NA, NA, NA, 9, 10, 11, NA)
    }
    if(loc$INACTIV[i] == "Oct-Mar"){ # i.e., active 4, 5, 6, 7, 8, 9
      activ[i,] <- c(NA, NA, NA, 4, 5, 6, 7, 8, 9, NA, NA, NA)
    }
    if(loc$INACTIV[i] == "none"){ # i.e., active all the year
      activ[i,] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    }
    if(loc$INACTIV[i] == "unk" || loc$INACTIV[i] == "kunk"){ # if unknown, calculate every month
      activ[i,] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    }
  }
  
  if(loc$LAT[i] < 0){ # Southern hemisphere
    if(loc$INACTIV[i] == "Spring and Winter"){ # i.e., active in summer and fall
      activ[i,] <- c(1, 2, 3, 4, 5, NA, NA, NA, NA, NA, NA, 12)
    }
    if(loc$INACTIV[i] == "Summer"){ # i.e., active in winter, spring and fall
      activ[i,] <- c(NA, NA, 3, 4, 5, 6, 7, 8, 9, 10, 11, NA)
    }
    if(loc$INACTIV[i] == "Winter"){ # i.e., active in summer, spring and fall
      activ[i,] <- c(1, 2, 3, 4, 5, NA, NA, NA, 9, 10, 11, 12)
    }
    if(loc$INACTIV[i] == "Oct-Apr"){ # i.e., active in 5, 6, 7, 8, 9
      activ[i,] <- c(NA, NA, NA, NA, 5, 6, 7, 8, 9, NA, NA, NA)
    }
    if(loc$INACTIV[i] == "Summer and Winter"){ # i.e., active spring and fall
      activ[i,] <- c(NA, NA, 3, 4, 5, NA, NA, NA, 9, 10, 11, NA)
    }
    if(loc$INACTIV[i] == "Oct-Mar"){ # i.e., active 4, 5, 6, 7, 8, 9
      activ[i,] <- c(NA, NA, NA, 4, 5, 6, 7, 8, 9, NA, NA, NA)
    }
    if(loc$INACTIV[i] == "none"){ # i.e., active all the year
      activ[i,] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    }
    if(loc$INACTIV[i] == "unk" || loc$INACTIV[i] == "kunk"){ # if unknown, calculate every month
      activ[i,] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    }
  }
}

# Compute temperatures
AIRTempts <- BURRTemps <- OPTemps_SUN <- OPTemps_BURR <- list()
for(i in 1:nrow(loc)){
  # Extract microclimatic data from NicheMapR at this location
  latlon <- loc[i,2:3]
  micro <- micro_global(loc = latlon, minshade = 0, maxshade = 100, Usrhyt = 0.10) 
  
  micro_sun <- as.data.frame(micro$metout) # meteorological conditions in the sun
  soil_sun <- as.data.frame(micro$soil) # soil temperatures
  
  active = activ[i,which(!is.na(activ[i,]))] # determine the months of activity
  Mat_TeSUN <- Mat_TeBURR <- Mat_Ta <- Mat_Tburrow <- array(NA, dim=c(24,12))
  for(month in active){ # For each month within the activity period
    # NicheMapR microclimatic variables
    day = unique(micro_sun$DOY)[month] # get julian day (middle day of the month)
    
    # SUN (open areas)
    AIRT <- subset(micro_sun, DOY == day)$TALOC  # air T 
    SOLR <- subset(micro_sun, DOY == day)$SOLR   # solar radiation
    SOILT_0 <- subset(soil_sun, DOY == day)$D0   # soil surface temperature
    VLOC <- subset(micro_sun, DOY == day)$VLOC   # wind speed
    RHLOC <- subset(micro_sun, DOY == day)$RHLOC # relative humidity
    
    # BURROWS
    SOILT_5 <- subset(soil_sun, DOY == day)$D5cm   # soil temperature, 5cm depth
    
    # OPERATIVE TEMPERATURES (open areas / burrows)
    L = as.numeric(data_terr$maximum_body_size_svl_hbl_cm[i]) * 1e-3 # body length (m)
    if(data_terr$class[i]=="Amphibia"){r=300}else{r=6e4} # Total resistance to water loss (s m-1)
    
    # Mass (g) from length (convert units)
    if(data_terr$class[i]=="Reptilia") M = 10^(-4.852+3.022*log10(L*1000)) # (length in mm) Lizards (Meiri 2010)
    if(data_terr$class[i]=="Amphibia") M = 10^(-4.298)*(L*1000)^3.181 # (length in mm) terrestrial anurans (Santini et al. 2018)
    if(data_terr$class[i]=="Insecta") M = exp(-3.628)*(L*1000)^2.494 / 1000 # (length in mm) Insects (Sample et al 1993)
    if(data_terr$class[i]=="Collembola") M = exp(-3.628)*(L*1000)^2.494 / 1000 # I didn't find a relationship for collembola (n=40), used insects

    # Surface area (m2) from mass (convert units)
    if(data_terr$class[i]=="Reptilia") A=0.0314*pi*(M/1000)^(2/3) # (mass in kg) O'Connor (1999)
    if(data_terr$class[i]=="Amphibia") A=9.8537*M^0.6745 * 1e-4 # Anurans (Klein et al. 2016).
    if(data_terr$class[i]=="Insecta") A=0.0013*M^0.8 # Insects (Lactin and Johnson, 1997)
    if(data_terr$class[i]=="Collembola") A=0.0013*M^0.8 # = insects?
    
    # Calculate operative tempeature in the sun
    Mat_TeSUN[,month] <- Temodel_ewl(abs=0.9, M=M, A=A, L=L, Fo_a=0.5, r=r, S=SOLR, v=VLOC, Ta=AIRT, Tg=SOILT_0, RH=RHLOC, exposed=T)
    # Operative temperature in the burrow
    Mat_TeBURR[,month] <- Temodel_ewl(abs=0.9, M=M, A=A, L=L, Fo_a=0.5, r=r, S=SOLR, v=VLOC, Ta=AIRT, Tg=SOILT_5, RH=RHLOC, exposed=F)
    # Air temperature
    Mat_Ta[,month] <- AIRT
    # Soil temperature (in the burrow) 
    Mat_Tburrow[,month] <- SOILT_5
  }
  
  AIRTempts[[i]] <- Mat_Ta
  BURRTemps[[i]] <- Mat_Tburrow
  OPTemps_SUN[[i]] <- Mat_TeSUN
  OPTemps_BURR[[i]] <- Mat_TeBURR
  
  print(paste0(round(100 * i/nrow(loc),2), " %"))
}

# AIRTempts, BURRTemps, OPTemps_SUN & OPTemps_BURR are 4 lists with as many matrices as cases we have in intratherm. 
# Each matrix contains 24 rows (hours) and 12 colums (months: some of them may be NA because the we didn't compute temperatures for the inactive months). 
# AIRTempts and BURRTemps contain air temperatures and soil (5cm depth) temperatures. 
# OPTemps_SUN and OPTemps_BURR are operative temperatures

# For example:
case = 300 # case in intratherm
month = 5

data_terr[case,1:6]
loc[case,]

plot(AIRTempts[[case]][,month], ylim=c(0,60)) # Daily air temperatures
points(BURRTemps[[case]][,month], pch=20) # daily temps in a burrow
points(OPTemps_SUN[[case]][,month], col="red") # daily operative temperature
points(OPTemps_BURR[[case]][,month], pch=20, col="red") # oper temps in a burrow

### EXTRACT MAX TEMPERATURES

# load(".../AIRTempts.Rdata")
# load(".../BURRTemps.Rdata")
# load(".../OPTemps_SUN.Rdata")
# load(".../OPTemps_BURR.Rdata")

Temp_matrix <- data.frame(array(NA, dim=c(nrow(loc),5)))
Temp_matrix[,1] <- loc$ID
colnames(Temp_matrix) <- c("ID", "AIRTQ75", "BURRTQ75", "TeSunQ75", "TeBurrQ75")
for(i in 1:nrow(loc)){
  warmest_month <- which.max(colMeans(AIRTempts[[i]], na.rm=T))
  Temp_matrix[i,2] <- quantile(AIRTempts[[i]][,warmest_month])[4]
  Temp_matrix[i,3]  <- quantile(BURRTemps[[i]][,warmest_month])[4]
  Temp_matrix[i,4]  <- quantile(OPTemps_SUN[[i]][,warmest_month], na.rm=T)[4]
  Temp_matrix[i,5]  <- quantile(OPTemps_BURR[[i]][,warmest_month], na.rm=T)[4]
}
