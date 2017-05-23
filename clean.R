# standardized cleaning routines -- apply to both training 
# and test datasets

library(dplyr)
library(lubridate)

russia_clean <- function(df) {
  res <- df %>%
    mutate(ts=ymd(timestamp),
           # Remove strange build years
           build_year=ifelse(build_year < 1600 | build_year > 2018,NA,build_year),
           # Convert ecology from factor to numeric scale
           eco_scale=NA,
           eco_scale=ifelse(ecology=='poor',0,eco_scale),
           eco_scale=ifelse(ecology=='satisfactory',1,eco_scale),
           eco_scale=ifelse(ecology=='good',2,eco_scale),
           eco_scale=ifelse(ecology=='excellent',3,eco_scale),
           # Fix weird floor areas by replacing with NA
           full_sq=ifelse(full_sq>5000,NA,full_sq),
           full_sq=ifelse(full_sq<=1,NA,full_sq),
           life_sq=ifelse(life_sq>5000,NA,life_sq),
           life_sq=ifelse(life_sq<=1,NA,life_sq),
           # Some kitchen sizes mixed up with year
           kitch_sq=ifelse(kitch_sq>5000,NA,kitch_sq))
  
  # Fix cases where life_sq > full_sq
  fix_us <- res %>% 
    filter(life_sq > full_sq) %>%
    select(id,life_sq,full_sq) %>%
    mutate(life_diff=abs(life_sq-30), # median in training set=30
           full_diff=abs(full_sq-50)) # median in training set=50
  # If life_sq is more typical that full_sq, then set full_sq to NA
  full_na <- fix_us %>%
    filter(life_diff < full_diff) %>%
    select(id)
  res$full_sq[res$id %in% full_na$id] <- NA
  # if full_sq is more typical than life_sq, then set life_sq to NA
  life_na <- fix_us %>%
    filter(life_diff > full_diff) %>%
    select(id)
  res$life_sq[res$id %in% life_na$id] <- NA
  
  # kitchen size shouldn't be bigger than total
  bad_kitch <- res %>% filter(kitch_sq > full_sq | kitch_sq > life_sq)
  res$kitch_sq[res$id %in% bad_kitch$id] <- NA
  
  res <- res %>%
    mutate(
           # Add log-scales for skewed variables
           ln_life_sq=log1p(life_sq),
           ln_public_transport_station_min_walk=log1p(public_transport_station_min_walk),
           ln_public_healthcare_km=log1p(public_healthcare_km),
           ln_public_transport_station_km=log1p(public_transport_station_km),
           ln_full_sq=log1p(full_sq),
           ln_kindergarten_km=log1p(kindergarten_km),
           ln_preschool_km=log1p(preschool_km),
           ln_school_km=log1p(school_km),
           ln_fitness_km=log1p(fitness_km),
           ln_hospice_morgue_km=log1p(hospice_morgue_km),
           ln_big_church_km=log1p(big_church_km),
           ln_additional_education_km=log1p(additional_education_km),
           ln_church_synagogue_km=log1p(church_synagogue_km),
           ln_price_doc=log1p(price_doc),
           ln_trc_sqm_500=log1p(trc_sqm_500),
           # Convert binary factor variables to 0/1
           culture_objects_top_25 = ifelse(culture_objects_top_25 == 'yes',1,0),
           thermal_power_plant_raion = ifelse(thermal_power_plant_raion == 'yes',1,0),
           incineration_raion = ifelse(incineration_raion == 'yes',1,0),
           oil_chemistry_raion = ifelse(oil_chemistry_raion == 'yes',1,0),
           radiation_raion = ifelse(radiation_raion == 'yes',1,0),
           railroad_terminal_raion = ifelse(railroad_terminal_raion == 'yes',1,0),
           big_market_raion = ifelse(big_market_raion == 'yes',1,0),
           nuclear_reactor_raion = ifelse(nuclear_reactor_raion == 'yes',1,0),
           detention_facility_raion = ifelse(detention_facility_raion == 'yes',1,0),
           water_1line = ifelse(water_1line == 'yes',1,0),
           big_road1_1line = ifelse(big_road1_1line == 'yes',1,0),
           railroad_1line = ifelse(railroad_1line == 'yes',1,0),
           # Turn all "ID" variables to factors
           ID_metro=as.factor(ID_metro),
           ID_railroad_station_walk = as.factor(ID_railroad_station_walk),
           ID_railroad_station_avto = as.factor(ID_railroad_station_avto),
           ID_big_road1 = as.factor(ID_big_road1),
           ID_big_road2 = as.factor(ID_big_road2),
           ID_railroad_terminal = as.factor(ID_railroad_terminal),
           ID_bus_terminal = as.factor(ID_bus_terminal),
           # Material should be a factor
           material=as.factor(material),
           # Training set contains one state=33; looks like a 1-4 scale
           state=ifelse(state>4,NA,state)
          )
  # TODO: remove variables that got rescaled
}