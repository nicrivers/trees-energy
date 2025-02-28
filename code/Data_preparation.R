#Urban tree canopy and energy consumption
#This script has 4 sections:
#1.Preparation of the data for statistical analysis, including the estimation of the LASSO model 
#1.1. Prepared data sets are saved in 'intermediate_data' folder
#2.Descriptive statistics including tables and plots 
#3.Predicted electricity savings from the LASSO model 
#4.Regression models 

#load the required packages
library(tidyverse)
library(sf)
library(weathercan)
library(suntools)
library(car)
library(glmnet)
library(xgboost)
library(caret)
library(mlr)
library(lubridate)
library(fixest)
library(modelsummary)
library(scales)
library(ggpubr)
library(cowplot)
library(forcats)

############
######################################################## SECTION ONE #######################################################################
#############

#DATA PREPERATION
#read the data
elecdat1 <- read_delim("../raw_data/LEAF.txt", delim = "|")
elecdat2 <- read_delim("../raw_data/LEAF2.txt", delim = "|")
treedat1 <- read_csv("../raw_data/data_FINAL_2022_12_19.csv")
treedat2 <- read_csv("../raw_data/data_random_tree.csv")

#Prepare the Data Sets for Analysis

#join the hydro data sets 
# Put the two electricity files together
elecdat <- bind_rows(elecdat1, elecdat2)

elecdat <- elecdat %>%
  mutate(FULLADDR = trimws(str_split_i(PREM_ADDRESS, "  ", i=1)))

# Aggregate any duplicates
elecdat <- elecdat %>%
  group_by(FULLADDR, START_INTERVAL_READ_DTTM) %>%
  summarise(READ_VALUE = sum(READ_VALUE)) %>%
  ungroup()

# Drop any households without 8760 observations
elecdat <- elecdat %>%
  group_by(FULLADDR) %>%
  summarise(count = n()) %>%
  filter(count == 8760) %>%
  dplyr::select(FULLADDR) %>%
  inner_join(elecdat)

# Create an anonymous house index
set.seed(1234)
house_id_crosswalk <- tibble(
  FULLADDR = sample(unique(elecdat$FULLADDR))
) %>%
  mutate(
    address_index = row_number()
  )

# Write the electricity data
elecdat_anon <- elecdat %>%
  inner_join(house_id_crosswalk) %>%
  select(-FULLADDR) %>%
  rename(elec = READ_VALUE,
         time = START_INTERVAL_READ_DTTM) %>%
  filter(elec > 0) 

# Note this is in UTC
write_csv(elecdat_anon, "../intermediate_data/elecdat.csv")

# Data on electricity prices
pricedat <- bind_rows(elecdat1, elecdat2) %>%
  group_by(time=START_INTERVAL_READ_DTTM) %>%
  summarise(TOU_CALC = first(TOU_CALC))

write_csv(pricedat, "../intermediate_data/pricedat.csv")

#join the tree data

treedat <- read_csv("../raw_data/data_FINAL_2022_12_19.csv") %>%
  # And from the households sampled completely at random
  bind_rows(read_csv("../raw_data/data_random_tree.csv", 
                     col_names = c("direction", "buffer", "area_canopy", "total_area_quadrant", "fraction_treed", "FULLADDR"))) %>%
  filter(buffer == 12.5) %>%
  group_by(FULLADDR, street_index) %>%
  summarise(tree_percent = sum(area_canopy) / sum(total_area_quadrant)  ) %>%
  # Remove houses that are observed twice
  group_by(FULLADDR, tree_percent) %>%
  summarise(street_index = mean(street_index, na.rm=T))

treedat_anon <- treedat %>%
  inner_join(house_id_crosswalk) %>%
  ungroup() %>%
  dplyr::select(-FULLADDR)

write_csv(treedat_anon, "../intermediate_data/treedat.csv")

# Round for additional anonymity
treedat_anon_rounded <- treedat_anon %>%
  mutate(tree_percent = round(tree_percent,2))

write_csv(treedat_anon_rounded, "../intermediate_data/treedat_rounded.csv")

#prepare the sample and prediction dataset
# Just the households that were selected completely at random
prediction_hh <- treedat %>%
  filter(is.na(street_index)) %>%
  inner_join(house_id_crosswalk) %>%
  ungroup() %>%
  dplyr::select(address_index)

write_csv(prediction_hh, "../intermediate_data/prediction_hh.csv")

# Just the households that were selected by street pairs
estimation_hh <- treedat %>%
  filter(!is.na(street_index)) %>%
  inner_join(house_id_crosswalk) %>%
  ungroup() %>%
  dplyr::select(address_index)

write_csv(estimation_hh, "../intermediate_data/estimation_hh.csv")

#weather and sun position data
# Load weather data and prepare the weather data
yow_weather <- weather_dl(station_ids = 49568, start = "2017-12-31", end = "2019-01-01", time_disp = "UTC") %>%
  dplyr::select(time, pressure, rel_hum, temp, visib, wind_dir, wind_spd) %>%
  # There are a very small number of NAs. Fill with prior hour values
  fill(pressure:wind_spd) %>%
  mutate(hdd = pmax(0, 18-temp),
         cdd = pmax(0, temp-18)) 

# Load and prepare the sun position data
sun_position <- as_tibble(solarpos(crds = matrix(c(-75.6972, 45.4215), nrow=1),
                                   dateTime = seq(
                                     as.POSIXct("2017-12-31 00:00:00", tz = "UTC"),
                                     as.POSIXct("2019-01-01 23:00:00", tz = "UTC"),
                                     by="hours"
                                   ))) %>%
  bind_cols(time = seq(
    as.POSIXct("2017-12-31 00:00:00", tz = "UTC"),
    as.POSIXct("2019-01-01 23:00:00", tz = "UTC"),
    by="hours"
  ))

names(sun_position) = c("azimuth","elevation", "time")

sun_position <- sun_position %>%
  # add daylight
  mutate(daylight = as.numeric(elevation > 0)) %>%
  # deal with azimuth trigonometry
  mutate(cos_az = cos(azimuth*pi/180),
         sin_az = sin(azimuth*pi/180))

#save the prepared data sets
write_csv(yow_weather,"../intermediate_data/weather.csv")
write_csv(sun_position, "../intermediate_data/sun-pos.csv")



#Prepare data for tree canopy cover within all the buffers
###########################################################################
#square meter of tree canopy within all the buffers
treeData <- read_csv("../raw_data/data_FINAL_2022_12_19.csv") %>%
  # And from the households sampled completely at random
  bind_rows(read_csv("../raw_data/data_random_tree.csv", 
                     col_names = c("direction", "buffer", "area_canopy", "total_area_quadrant", "fraction_treed", "FULLADDR")))

net_buff <- treeData %>% 
  group_by(FULLADDR, direction, street_index) %>%
  arrange(FULLADDR, desc(buffer)) %>%
  mutate(net_tree = if_else(buffer == min(buffer), area_canopy, area_canopy - lead(area_canopy, default = 0))) %>%
  mutate(net_area_quad = if_else(buffer == min(buffer), total_area_quadrant, total_area_quadrant - lead(total_area_quadrant, default = 0))) %>%
  group_by(FULLADDR,street_index,buffer) %>%
  summarize(sum_canopy=sum(net_tree*1000000,na.rm = TRUE),sum_area=sum(net_area_quad))

net_buff <- net_buff %>%
  # Remove houses that are observed twice
  group_by(FULLADDR, buffer,sum_canopy) %>%
  summarise(street_index = mean(street_index, na.rm=T)) %>%
  pivot_wider(names_from = buffer, values_from = c(sum_canopy), names_prefix = "buff_") %>%
  inner_join(house_id_crosswalk) %>%
  ungroup() %>%
  dplyr::select(-FULLADDR)

write_csv(net_buff,"../intermediate_data/net_buffer_sqm.csv")

# Round for anonymity
net_buff_rounded <- net_buff %>%
  mutate(buff_5 = round(buff_5, -6),
         buff_12.5 = round(buff_12.5, -6),
         buff_20 = round(buff_20, -6))

write_csv(net_buff_rounded,"../intermediate_data/net_buffer_sqm_rounded.csv")



#prepare data for all directions
################################
#prepare tree canopy measure within all directions 
treeData <- read_csv("../raw_data/data_FINAL_2022_12_19.csv") %>%
  # And from the households sampled completely at random
  bind_rows(read_csv("../raw_data/data_random_tree.csv", 
                     col_names = c("direction", "buffer", "area_canopy", "total_area_quadrant", "fraction_treed", "FULLADDR")))

direction<- treeData %>%
  filter(buffer == 12.5) %>%
  #make groups for each unique address,direction, and street index
  group_by(FULLADDR, street_index,direction) %>% 
  #summarise the tree percent for each unique address and street index pair
  summarise(tree_percent = sum(area_canopy) / sum(total_area_quadrant)  ) %>%
  # Remove houses that are observed twice (very important)
  group_by(FULLADDR, direction,tree_percent) %>%
  summarise(street_index = mean(street_index, na.rm=T)) %>%
  group_by(FULLADDR,direction, street_index) %>%
  summarise(tree_percent = mean(tree_percent)) %>%
  pivot_wider(names_from = direction, values_from = c(tree_percent), names_prefix = "dir_") %>%
  inner_join(house_id_crosswalk) %>%
  ungroup() %>%
  dplyr::select(-FULLADDR)

write_csv(direction,"../intermediate_data/direction.csv")

direction_rounded <- direction %>%
  mutate(dir_east = round(dir_east, 2),
         dir_north = round(dir_north, 2), 
         dir_south = round(dir_south, 2),
         dir_west = round(dir_west, 2))

write_csv(direction_rounded,"../intermediate_data/direction_rounded.csv")

#########
# Jitter city address points data

impervious_clip <- read_sf("../raw_data/U_Ottawa_ClimateResiliency.gdb", layer = "Impervious_clip") %>%
  filter(Type=="Building")

address_points <- read_csv("../raw_data/Municipal_Address_Points.csv") %>%
  st_as_sf(coords = c("X","Y")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(impervious_clip)) %>%
  inner_join(house_id_crosswalk) %>%
  select(address_index, geometry)

# How much should I move houses: +- 100 metres
jitter_amount <- 100

address_points_jittered <- address_points %>%
  st_jitter(jitter_amount)

write_sf(address_points_jittered, "../intermediate_data/address_points_jittered.shp")
