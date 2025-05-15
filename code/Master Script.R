#Urban tree canopy and energy consumption
#This script has 3 sections:
#1.Descriptive statistics including tables and plots 
#2.Estimation of LASSO model and predicted electricity savings from the LASSO model 
#3.Regression models 

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

# Indicate whether the analysis uses rounded (for anonymity) data or original non-rounded data
# Rounded data will fail to exactly replicate results in paper
# One of the two statements below should be commented out
#use_rounded_data <- 1 # Comment this statement out to use original (non-rounded data)
use_rounded_data <- 0 # Comment this statement out to use rounded data

if (use_rounded_data == 0) {
  treedat <- "../intermediate_data/treedat.csv"
  bufferdat <- "../intermediate_data/net_buffer_sqm.csv"
  directiondat <- "../intermediate_data/direction.csv"
} else if (use_rounded_data == 1) {
  treedat <- "../intermediate_data/treedat_rounded.csv"
  bufferdat <- "../intermediate_data/net_buffer_sqm_rounded.csv"
  directiondat <- "../intermediate_data/direction_rounded.csv"
}


                                                        ############
######################################################## SECTION 1 #######################################################################
                                                       #############
#DESCRIPTIVE STATITICS, PLOTS, and TABLES 

#read only the required data sets  
dat <- read_csv("../intermediate_data/elecdat.csv") %>%
  # Create time variables in correct time zone
  mutate(time = force_tz(time, "EST")) %>%
  mutate(month = as.character(month(time, label=TRUE)), 
         hour = hour(time), 
         weekday = weekdays(time)) %>%
  mutate(weekend = as.numeric(weekday=="Saturday" | weekday == "Sunday")) %>%
  mutate(summer = as.numeric(month %in% c("May", "Jun", "Jul", "Aug", "Sep"))) %>%
  # Join with tree cover
  inner_join(
    read_csv(treedat)
  ) %>%
  # Join with weather and sun data
  inner_join(
    read_csv("../intermediate_data/weather.csv")
  ) %>%
  inner_join(
    read_csv("../intermediate_data/sun-pos.csv")
  )

est_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/estimation_hh.csv")
  )

pred_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/prediction_hh.csv")
  )

zoning <- read_sf("../raw_data/U_Ottawa_ClimateResiliency.gdb", layer = "zoning__May30_2022")

address_points_jittered <- read_sf("../intermediate_data/address_points_jittered.shp") %>%
  rename(address_index = addrss_)



#set a uniform theme for all the plots
journal_theme <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = "black", size = base_size, family = base_family),
      axis.title = element_text(face = "bold", color = "black"),
      axis.text = element_text(color = "black"),
      legend.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.ticks = element_line(color = "black"),  # Ensure tick marks are visible
      axis.ticks.length = unit(0.2, "cm"),  # Length of the tick marks
      axis.line = element_line(color = "black")  # Ensure axis lines are visible
    )
}

#Uni variate Statistics
########################
calculate_stats <- function(data, continuous_vars, binary_vars) {
  continuous_stats <- data %>%
    summarise(across(all_of(continuous_vars), list(
      mean = ~mean(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}"))
  
  binary_stats <- data %>%
    summarise(across(all_of(binary_vars), list(
      count_0 = ~sum(. == 0, na.rm = TRUE),
      count_1 = ~sum(. == 1, na.rm = TRUE),
      #how frequently 1 appears compared to 0
      proportion_1 = ~mean(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}"))
  
  combined_stats <- bind_rows(Continuous = continuous_stats, Binary = binary_stats)
  combined_stats
}

continuous_vars <- c("elec", "tree_percent", "rel_hum", "temp", "wind_spd", "azimuth", "elevation", "hdd", "cdd")
binary_vars <- c("weekend", "summer","daylight")

stat <- calculate_stats(dat, continuous_vars, binary_vars)
write.csv(stat,file = "../intermediate_data/uni-stat.csv")

stat_est <- calculate_stats(est_12.5, continuous_vars, binary_vars)
write.csv(stat_est,file = "../intermediate_data/uni-stat-estimate.csv")

stat_pred <- calculate_stats(pred_12.5, continuous_vars, binary_vars)
write.csv(stat_pred,file = "../intermediate_data/uni-stat-predict.csv")


#Spatial Visualizations
#################################
#ottawa zoning map
#assign full names to zones
zoning<- zoning %>%
  mutate(
    full_zone = case_when(
      ZONE_MAIN == "RU" ~ "Rural Country Side",
      ZONE_MAIN %in% c("AG","EP","O1")~ "Agriculture, Environmental Protection, Parks",
      ZONE_MAIN == "AM" ~ "Arterial Mainstreet",
      ZONE_MAIN == "GM" ~ "General Mixed Use",
      ZONE_MAIN %in% c("IG", "IH") ~ "Industrial Zone",
      ZONE_MAIN=="R1"~"Residential First Density (R1)",
      ZONE_MAIN %in% c("R2","R3","R4","R5") ~ "Residental Second to Fifth Density",
      TRUE ~ "Other"  # Default value for unmapped zones
    )
  )

zone <- ggplot()+
  geom_sf(data = zoning, aes(fill=full_zone))+
  scale_fill_manual(values = c(
    "Rural Country Side" = "lightgreen",
    "Agriculture, Environmental Protection, Parks" = "forestgreen",
    "Arterial Mainstreet" = "coral",
    "General Mixed Use" = "magenta",
    "Industrial Zone" = "steelblue",
    "Residential First Density (R1)" = "red",
    "Residental Second to Fifth Density"="lightyellow",
    "Other"="lightgrey"),
    name = "Ottawa Zoning Map")+
  journal_theme()+
  theme_void()

ggsave("../figures_tables_output/zoning.jpg",width = 20,height = 16,units = c("cm"),dpi=300)


#Ottawa's tree canopy Map
#data set with all houses (random and paired)
dat <- dat %>%
  select(address_index, tree_percent, street_index) %>% 
  group_by(address_index, street_index) %>% 
  summarise(tree_percent=mean(tree_percent)) 

#join address points and tree data
address_trees <- full_join(address_points_jittered, dat, by = "address_index") %>% filter(!is.na(tree_percent))
address_trees <- st_as_sf(address_trees)

tree <-ggplot()+
  geom_sf(data = zoning)+
  geom_sf(data = address_trees, aes(color = 100*tree_percent), size=1, alpha=0.5) +
  scale_color_viridis_b() +  # This provides a nice color scale
  labs(color = "Tree Canopy Coverage (%)",
       size = "Tree Canopy Coverage (%)") +
  coord_sf()+
  journal_theme()+
  theme_void()

ggsave("../figures_tables_output/tree.jpg",width = 20,height = 16,units = c("cm"),dpi=300)

ggarrange(zone, tree, 
          labels = c("A", "B"), 
          nrow = 2)

ggsave("../figures_tables_output/zone_tree.jpg",width = 20,height = 25,units = c("cm"),dpi=300)

#Density Plots
###########################

#density plots in estimation and prediction samples 
# Prediction sample: Tree Canopy Coverage
mean_df <- pred_12.5 %>% summarise(mean_T = mean(tree_percent))
p_t <- ggplot(pred_12.5, aes(x = tree_percent)) +
  geom_density(fill = "steelblue", alpha = 0.5, adjust = 6) +
  geom_vline(data = mean_df, aes(xintercept = mean_T), color = "red", linetype = "dashed", size = 0.5) +
  geom_text(data = mean_df, aes(x = mean_T, y = after_stat(density) * 1.1, label = "Sample Mean ~ 25%"),
            stat = "density", color = "red", hjust = -0.1) +
  labs(x = "Tree Canopy Coverage (%)", y = "Prediction Sample Density") +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1), expand = c(0, 0.02)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  journal_theme()

# Prediction sample: Electricity Consumption
mean_df <- pred_12.5 %>% summarise(mean_E = mean(elec))
p_e <- ggplot(pred_12.5, aes(x = elec)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(data = mean_df, aes(xintercept = mean_E), color = "red", linetype = "dashed", size = 0.5) +
  geom_text(data = mean_df, aes(x = mean_E, y = after_stat(density) * 1.1, label = "Sample Mean ~ 1.07 kWh"),
            stat = "density", color = "red", hjust = -0.1) +
  labs(x = "Electricity Consumption (kWh/h)", y = "Prediction Sample Density") +
  scale_x_log10(breaks = c(0.1,0.5,1,5,10), limits=c(0.01,50)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  journal_theme()

# Estimation sample: Tree Canopy Coverage
mean_df <- est_12.5 %>% summarise(mean_T = mean(tree_percent))
e_t <- ggplot(est_12.5, aes(x = tree_percent)) +
  geom_density(fill = "steelblue", alpha = 0.5, adjust = 6) +
  geom_vline(data = mean_df, aes(xintercept = mean_T), color = "red", linetype = "dashed", size = 0.5) +
  geom_text(data = mean_df, aes(x = mean_T, y = after_stat(density) * 1.1, label = "Sample Mean ~ 28%"),
            stat = "density", color = "red", hjust = -0.1) +
  labs(x = "Tree Canopy Coverage (%)", y = "Estimation Sample Density") +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1), expand = c(0, 0.02)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  journal_theme()

# Estimation sample: Electricity Consumption
mean_df <- est_12.5 %>% summarise(mean_E = mean(elec))
e_e <- ggplot(est_12.5, aes(x = elec)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(data = mean_df, aes(xintercept = mean_E), color = "red", linetype = "dashed", size = 0.5) +
  geom_text(data = mean_df, aes(x = mean_E, y = after_stat(density) * 1.1, label = "Sample Mean ~ 1.13 kWh"),
            stat = "density", color = "red", hjust = -0.1) +
  labs(x = "Electricity Consumption (kWh/h)", y = "Prediction Sample Density") +
  scale_x_log10(breaks = c(0.1,0.5,1,5,10), limits=c(0.01,50)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  journal_theme()

# Arrange the plots
ggarrange(p_t, p_e, e_t, e_e, 
          labels = c("A", "B", "C", "D"), 
          ncol = 2, nrow = 2)

ggsave("../figures_tables_output/density-plot.jpg",width = 20,height = 16,units = c("cm"),dpi=300)


#Energy consumption variation in summer and winter per tree canopy groups
####################################################
#average of Energy consumption in each UTC category in each panel 

t <- est_12.5%>%
  mutate(tree_discrete = cut(tree_percent, breaks = c(0.0, 0.25, 1.0), 
                             labels=c("0%-25% Canopy","25%-100% Canopy"), 
                             include.lowest = TRUE,
                             right = FALSE))%>% 
  group_by(tree_discrete,summer)%>%
  summarise(avr_E=mean(elec,na.rm=TRUE))



#plot
ggplot(data = est_12.5%>%
         mutate(tree_discrete = cut(tree_percent, breaks = c(0.0, 0.25, 1.0), 
                                    labels=c("0%-25% Canopy","25%-100% Canopy"), 
                                    include.lowest = TRUE,
                                    right = FALSE))%>% 
         group_by(tree_discrete, hour, summer) %>%
         summarise(avr_E=mean(elec, na.rm=TRUE), .groups = "drop"), 
       aes(x = hour, y = avr_E, color = tree_discrete)) +
  scale_color_manual(name="Tree Canopy Groups", values=c("0%-25% Canopy" = "firebrick", "25%-100% Canopy" = "steelblue")) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(0, 6,12,18)) +
  facet_wrap(~summer, labeller = labeller(
    summer = as_labeller(c("1" = "In-leaf", "0" = "Off-leaf")))) +
  labs(
    x = "Hour",
    y = "Average Electricity Consumption (kWh/h)"
    ) +
  journal_theme() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) 

ggsave("../figures_tables_output/E-by-canopy-season2.jpg",width = 15,height = 15,units = c("cm"),dpi=300)

#Energy consumption by weather and tree canopy group
#####################################################
temp <-ggplot(data =est_12.5%>%
                mutate(tree_discrete = cut(tree_percent, breaks = c(0.0, 0.25, 1.0), 
                                           labels=c("0%-25% Canopy","25%-100% Canopy"), 
                                           include.lowest = TRUE,
                                           right = FALSE))%>% 
                group_by(tree_discrete,temp)%>%
                summarise(avr_E=mean(elec,na.rm=TRUE)), aes(x = temp, y = avr_E, color=tree_discrete))+
  geom_point(alpha=0.65)+
  scale_color_manual(name="Tree Canopy Groups",values=c("0%-25% Canopy" = "red", 
                                                        "25%-100% Canopy" = "steelblue"))+
  scale_x_continuous(breaks = seq(-30,40, by = 5))+
  geom_smooth(method = loess, color="navy", size=1)+
  labs(
    x=expression(paste("Temperature (", degree, "C)")),
    y = "Average Eelectricty Consumption (kW/h)") +
  journal_theme() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

temp

hum <- ggplot(data =est_12.5%>%
                mutate(tree_discrete = cut(tree_percent, breaks = c(0.0, 0.25, 1.0), 
                                           labels=c("0%-25% Canopy","25%-100% Canopy"), 
                                           include.lowest = TRUE,
                                           right = FALSE))%>% 
                group_by(tree_discrete,rel_hum)%>%
                summarise(avr_E=mean(elec,na.rm=TRUE)), aes(x = rel_hum, y = avr_E, color=tree_discrete))+
  geom_point(alpha=0.65)+
  scale_color_manual(name="Tree Canopy Groups",values=c("0%-25% Canopy" = "red", 
                                                        "25%-100% Canopy" = "steelblue"))+
  scale_x_continuous(breaks = seq(0,100, by = 10))+
  geom_smooth(method = loess, color="navy", size=1)+
  labs(
    x="Relative Humidity %",
    y = "Average Eelectricty Consumption (kW/h)") +
  journal_theme() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

hum

wind <- ggplot(data =est_12.5%>%
                 mutate(tree_discrete = cut(tree_percent, breaks = c(0.0, 0.25, 1.0), 
                                            labels=c("0%-25% Canopy","25%-100% Canopy"), 
                                            include.lowest = TRUE,
                                            right = FALSE))%>% 
                 group_by(tree_discrete,wind_spd)%>%
                 summarise(avr_E=mean(elec,na.rm=TRUE)), aes(x = wind_spd, y = avr_E, color=tree_discrete))+
  geom_point(alpha=0.65)+
  scale_color_manual(name="Tree Canopy Groups",values=c("0%-25% Canopy" = "red", 
                                                        "25%-100% Canopy" = "steelblue"))+
  scale_x_continuous(breaks = seq(0,60, by = 5))+
  geom_smooth(method = loess, color="navy", size=1)+
  labs(
    x="Wind Speed (km/h)",
    y = "Average Eelectricty Consumption (kW/h)") +
  journal_theme() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

wind

#plot all together
ggarrange( temp,hum,wind,
           labels = c("A","B","C"),
           nrow = 3, ncol = 1)

ggsave("../figures_tables_output/temp-wind-humid.jpg",width = 35,height = 32,units = c("cm"),dpi=300)


#Energy consumption by weather and month
#####################################################
#average E by temp
temp <-ggplot(data = est_12.5 %>% group_by(temp) %>% 
                summarise(mean_E=mean(elec)), 
              aes(x=temp, y=mean_E),color="temp")+
  geom_point(aes(colour=temp), size=1) +
  scale_colour_gradient(name="Temperature (°C)", low = "steelblue", high = "red")+
  journal_theme()+
  labs(
    x=expression(paste("Temperature (", degree, "C)")),
    y="Average Electricity Consumption (kWh)")+
  scale_x_continuous(breaks = seq(-30, 40, by = 5))

#average E by month and temp
month_temp <-ggplot(data = est_12.5%>% mutate(month=month(time))%>%group_by(month) %>% 
                      summarise(mean_E=mean(elec), mean_T=mean(temp), .groups = 'drop'),aes(x=month))+
  geom_line(aes(y=mean_E*1000, color="Electricity Consumption"))+
  geom_line(aes(y = mean_T*100, color="Temperature"), linetype = "dashed") +  # Scaling temperature for display
  scale_y_continuous(
    name = "Average Electricity Consumption (Wh)",
    sec.axis = sec_axis(~. /100,name = "Average Temperature (°C)")) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(name="",values = c("Electricity Consumption"="grey", "Temperature"="steelblue"))+
  labs(
    x="Month") +
  journal_theme()+
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))

#humidity
#average E by rel_hum
hum<-ggplot(data = est_12.5 %>% group_by(rel_hum) %>% 
              summarise(mean_E=mean(elec)), 
            aes(x=rel_hum, y=mean_E),color="rel_hum")+
  geom_point(aes(colour=rel_hum), size=1) +
  scale_x_continuous(breaks = seq(10,100, by = 10))+
  scale_colour_gradient(name="Relative Humidity %", low = "steelblue", high = "red")+
  journal_theme()+
  labs(
    x="Relative Humidity %",
    y="Average Electricity Consumption (kWh)")

#month and humidity
month_hum <-ggplot(data = est_12.5%>% mutate(month=month(time))%>%group_by(month) %>% 
                     summarise(mean_E=mean(elec), mean_h=mean(rel_hum), .groups = 'drop'),aes(x=month))+
  geom_line(aes(y=mean_E*1000, color="Electricity Consumption"))+
  geom_line(aes(y = mean_h*100, color="Average Relative Humidity"), linetype = "dashed") +  
  scale_y_continuous(
    name = "Average Electricity Consumption (Wh)",
    sec.axis = sec_axis(~./100,name = "Average Relative Humidity(%)")) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(name="",values = c("Electricity Consumption"="grey", "Average Relative Humidity"="steelblue"))+
  labs(
    x="Month") +
  journal_theme()+
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid"))))

month_hum
#wind
#average E by rel_hum
wind <-ggplot(data = est_12.5 %>% group_by(wind_spd) %>% 
                summarise(mean_E=mean(elec)), 
              aes(x=wind_spd, y=mean_E),color="wind_spd")+
  geom_point(aes(colour=wind_spd), size=1) +
  scale_x_continuous(breaks = seq(0,60, by = 10))+
  scale_colour_gradient(name="Wind Speed (km/h)", low = "steelblue", high = "red")+
  journal_theme()+
  labs(
    x="Wind Speed (km/h)",
    y="Average Electricity Consumption (kWh)")

#
#month and wind
month_wind <-ggplot(data = est_12.5%>% mutate(month=month(time))%>%group_by(month) %>% 
                      summarise(mean_E=mean(elec), mean_w=mean(wind_spd), .groups = 'drop'),aes(x=month))+
  geom_line(aes(y=mean_E*1000, color="Electricity Consumption"))+
  geom_line(aes(y = mean_w*100, color="Average Wind Speed"), linetype = "dashed") +  
  scale_y_continuous(
    name = "Average Electricity Consumption (Wh)",
    sec.axis = sec_axis(~./100,name = "Average Wind Speed (km/h)")) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(name="",values = c("Electricity Consumption"="grey", "Average Wind Speed"="steelblue"))+
  labs(
    x="Month") +
  journal_theme()+
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid"))))

month_wind

ggarrange(temp,month_temp,hum, month_hum,wind,month_wind,
          labels = c("A","B","C","D","E","F"),
          nrow = 3,ncol = 2)


ggsave("../figures_tables_output/wind-hum-month.jpg",width = 27,height = 30,units = c("cm"),dpi=300)

                                                              ################
############################################################### SECTION 2 #########################################################
                                                              ################

# Estimate the LASSO model 
########################################################################################

#read the prepared estimation and prediction sample 

#read only the required data sets  
dat <- read_csv("../intermediate_data/elecdat.csv") %>%
  # Create time variables in correct time zone
  mutate(time = force_tz(time, "EST")) %>%
  mutate(month = as.character(month(time, label=TRUE)), 
         hour = hour(time), 
         weekday = weekdays(time)) %>%
  mutate(weekend = as.numeric(weekday=="Saturday" | weekday == "Sunday")) %>%
  mutate(summer = as.numeric(month %in% c("May", "Jun", "Jul", "Aug", "Sep"))) %>%
  # Join with tree cover
  inner_join(
    read_csv(treedat)
  ) %>%
  # Join with weather and sun data
  inner_join(
    read_csv("../intermediate_data/weather.csv")
  ) %>%
  inner_join(
    read_csv("../intermediate_data/sun-pos.csv")
  )

est_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/estimation_hh.csv")
  )

pred_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/prediction_hh.csv")
  )

#1.Frisch-Waugh-Lovell partialling out
estimation_sample <- est_12.5

#Residuals of electricity (partial out the variation in energy consumption not explained by fixed effects)
estimation_sample$elec_dot <- feols(log(elec) ~ 0 |  address_index^hour + address_index^weekend + street_index^time, data=estimation_sample) %>% resid()

#Residuals of tree x summer
# Create the variable tree x summer
estimation_sample <- estimation_sample %>%
  mutate(tree_summer = tree_percent * summer)
# Residuals (partial out the variation in tree canopy during in-leaf period not explained by fixed effects)
estimation_sample$tree_summer_dot <- feols(tree_summer ~ 0 |  address_index^hour + address_index^weekend + street_index^time, data=estimation_sample) %>% resid()

#2.Create the training data set
# This section of the code interacts tree x summer with weather variables and demeans each variable
# we consider all two-way interactions between weather variables, quadratic terms, and linear terms
# The loop below constructs these variables, demeans them, and creates a model formula

model.matrix.train.formula <- as.formula("~ tree_summer_dot")

# List of weather variables
vars <- c("cdd", "hdd", "rel_hum", "wind_spd", "elevation", "weekend", "daylight", "cos_az", "sin_az", "1")

#################################################################################
# This is all combinations of variables                                         #
combinations_no_repetition <- t(combn(vars, 2))                                 #
# Generate combinations with repetition                                         #
repeated_combinations <- t(sapply(vars[-length(vars)], function(x) c(x, x)))    #
rownames(repeated_combinations) <- NULL
# Combine both sets of combinations                                             #
all_combinations <- rbind(combinations_no_repetition, repeated_combinations)    #
# Now loop over this.                                                           #
#################################################################################

#################################################################################
# This generates the model matrices (data)
for (i in 1:nrow(all_combinations)) {
  w = all_combinations[i,1]
  v = all_combinations[i,2]
  
  print(paste("Done",i,"out of",nrow(all_combinations),"with size",object.size(estimation_sample)/1e9,"GB"))
  flush.console()
  
  # Only include if there is variation
  if (v == "1") {
    if (var(estimation_sample[[w]]) == 0) next
  } else {
    if (var(estimation_sample[[v]] * estimation_sample[[w]]) == 0) next
  }
  
  new_var_name <- paste("tree_summer",v,w,"dot",sep="_")
  formula <- as.formula(paste("I(tree_summer *", v, "*", w, ") ~ 0 |  address_index^hour + address_index^weekend + street_index^time"))
  estimation_sample[[new_var_name]] = feols(formula, data=estimation_sample) %>% resid()
  
  model.matrix.train.formula <- update(model.matrix.train.formula, as.formula(paste("~ . +", new_var_name)))
}


# Remove the linear term
model.matrix.train.formula <- update(model.matrix.train.formula, "~ . - tree_summer_1_1_dot - 1")
# Save the formula
saveRDS(model.matrix.train.formula, "../intermediate_data/model_matrix_train_formula.rds")

model.matrix.train <- model.matrix(model.matrix.train.formula, data=estimation_sample)

train_y <- estimation_sample$elec_dot

#3. Estimate the LASSO model
###########################
# This section estimates the model above and saves it so that it can be used again for prediction

lambdas_to_try <- 10^seq(-5, 0, length.out = 20) # this is the penalty parameter. We figure out which one to use by cross-validation
lasso.elec.cv <- cv.glmnet(y = train_y, x = model.matrix.train, alpha=1, lambda=lambdas_to_try, family = "gaussian", type.measure = "mse", nfolds=5)
bestlam = lasso.elec.cv$lambda.min
lasso.elec <- glmnet(y = train_y, x = model.matrix.train, alpha=1, lambda = bestlam, family="gaussian")
lasso.coef <- predict(lasso.elec, type = "coefficients")

# Save the model
saveRDS(lasso.elec, file="../intermediate_data/final_lasso_model.rds")

#Prediction of Electricity savings with LASSO estimates
##############################################################################################

# Read the data

#read only the required data sets  
dat <- read_csv("../intermediate_data/elecdat.csv") %>%
  # Create time variables in correct time zone
  mutate(time = force_tz(time, "EST")) %>%
  mutate(month = as.character(month(time, label=TRUE)), 
         hour = hour(time), 
         weekday = weekdays(time)) %>%
  mutate(weekend = as.numeric(weekday=="Saturday" | weekday == "Sunday")) %>%
  mutate(summer = as.numeric(month %in% c("May", "Jun", "Jul", "Aug", "Sep"))) %>%
  # Join with tree cover
  inner_join(
    read_csv(treedat)
  ) %>%
  # Join with weather and sun data
  inner_join(
    read_csv("../intermediate_data/weather.csv")
  ) %>%
  inner_join(
    read_csv("../intermediate_data/sun-pos.csv")
  )

est_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/estimation_hh.csv")
  )

pred_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/prediction_hh.csv")
  )

## load the model objects
#########################
# Trained LASSO model
lasso.elec <- readRDS("../intermediate_data/final_lasso_model.rds")

# LASSO model formula
model.matrix.train.formula <- readRDS("../intermediate_data/model_matrix_train_formula.rds")

# Weather variables
# These need to be exactly the same as in estimate_lasso_final.R
vars <- c("cdd", "hdd", "rel_hum", "wind_spd", "elevation", "weekend", "daylight", "cos_az", "sin_az", "1")



## Create all required variables
################################
# This section of code creates the same variables in the prediction sample as were used to train the model

# Create tree by summer variable
prediction_sample <- pred_12.5
prediction_sample <- prediction_sample %>%
  mutate(tree_summer = tree_percent * summer)

#################################################################################
# This is all combinations of variables                                         #
combinations_no_repetition <- t(combn(vars, 2))                                 #
# Generate combinations with repetition                                         #
repeated_combinations <- t(sapply(vars[-length(vars)], function(x) c(x, x)))    #
rownames(repeated_combinations) <- NULL
# Combine both sets of combinations                                             #
all_combinations <- rbind(combinations_no_repetition, repeated_combinations)    #
# Now loop over this.                                                           #
#################################################################################

# This generates the model matrices (data)
for (i in 1:nrow(all_combinations)) {
  w = all_combinations[i,1]
  v = all_combinations[i,2]
  
  print(paste("Done",i,"out of",nrow(all_combinations)))
  flush.console()
  
  new_var_name <- paste("tree_summer",v,w,sep="_")
  
  if (v == "1") {
    prediction_sample[[new_var_name]] = prediction_sample$tree_summer * prediction_sample[[w]] 
  } else {
    prediction_sample[[new_var_name]] = prediction_sample$tree_summer * prediction_sample[[v]] * prediction_sample[[w]]
  }
  
  
}


# Rename the coefficients in LASSO model to remove dots
row.names(lasso.elec$beta) = gsub("_dot","", x=row.names(lasso.elec$beta))

# Same model matrix as above, but without dots
model.matrix.saving <- model.matrix(formula(paste(gsub("_dot","",model.matrix.train.formula), collapse=" ")) , prediction_sample)

# Predictions
prediction_sample$tree_saving_predicted = predict(lasso.elec, model.matrix.saving) %>% as.numeric()
# This is in log points. Convert to percent using (exp(log_saving)-1)
prediction_sample$tree_saving_predicted <- exp(prediction_sample$tree_saving_predicted) - 1

#Save the Predictions 
write_csv(prediction_sample,"../intermediate_data/Lasso-prediction.csv")


#PLOTS FOR ELECTERICITY SAVINGS ESTIMATED FROM THE LASSO MODEL 



#uniform plot theme
journal_theme <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = "black", size = base_size, family = base_family),
      axis.title = element_text(face = "bold", color = "black"),
      axis.text = element_text(color = "black"),
      legend.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.ticks = element_line(color = "black"),  # Ensure tick marks are visible
      axis.ticks.length = unit(0.2, "cm"),  # Length of the tick marks
      axis.line = element_line(color = "black")  # Ensure axis lines are visible
    )
}

#read the Lasso predicted data
prediction_sample_lasso <- read_csv("../intermediate_data/Lasso-prediction.csv")


#Predicted savings by canopy percent
####################################
canopy_save <- prediction_sample_lasso %>%
  group_by(address_index, tree_percent) %>%
  summarise(tree_saving_predicted = mean(tree_saving_predicted)) %>%
  ggplot(aes(x=tree_percent, y=tree_saving_predicted)) +
  geom_smooth(method="lm", se=F, colour="firebrick") +
  scale_x_continuous(name="Tree Canopy Coverage Within 12.5-meter", labels=scales::percent_format()) +
  scale_y_continuous(name="Predicted Electricity Savings", labels=scales::percent_format())+
  journal_theme()

ggsave("../figures_tables_output/save-tree.jpg",width = 20,height = 16,units = c("cm"),dpi=300)

#Average monthly saving and monthly price (in-leaf period)
########################################################
#create the variable month 
prediction_sample_lasso <- prediction_sample_lasso %>% 
  mutate(month=month(time))
prediction_sample_lasso$month <- as.character(prediction_sample_lasso$month)

save <- ggplot(data = prediction_sample_lasso %>% 
               filter(month %in% c("5", "6", "7", "8", "9")) %>% #in-leaf period
               group_by(hour, month) %>% 
               summarise(avr_save = mean(tree_saving_predicted))) +
  geom_line(aes(x = hour, y = avr_save, color = month, group = month)) +
  geom_point(aes(x = hour, y = avr_save, color = month, group = month), size = 1) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(-0.14, 0, by = 0.02)  # Custom breaks within the range of avr_save
  ) +
  scale_x_continuous(breaks = seq(0, 24,by=2)) +
  scale_color_manual(
    name = "Month",
    values = c(
      "5" = "#5CB85C",
      "6" = "blue",
      "7" = "red",
      "8" = "#F0AD4E",
      "9" = "black"
    ), labels = c(
      "May",
      "June",
      "July",
      "August",
      "September"
    )
  ) +
  labs(
    y = "Average Electricity Savings",
    x="",
    color = "Month"
  ) +
  journal_theme()

#prepare data for hourly energy price
# Electricity price data
pricedat <- read_csv("../intermediate_data/pricedat.csv") %>%
  mutate(time = force_tz(time, "EST"))

#electricity prices 
#prices from "https://www.oeb.ca/consumer-information-and-protection/electricity-rates/historical-electricity-rates" for May 2018 in CAD
prices <- tibble(TOU_CALC = c("L","M","H"), price = c(0.065,0.094,0.132)) %>%
#change CAD to $, exchange rate of CAD to USD from May-Nov 2018 is ~0.76
  mutate(price = price * 0.76)

#Join the hourly electricity consumption data with hourly electricity price data
elec_price <- inner_join(pricedat,prices) %>%
  mutate(month=month(time), 
         hour=hour(time))

#change the month variable to character 
elec_price$month <- as.character(elec_price$month)

#we are interested in summer weekdays 
elec_price <- elec_price %>%
  mutate(weekday = weekdays(time)) 
#remove the weekends
elec_price <- elec_price %>% 
  filter(!weekday%in%c("Saturday","Sunday")) 

# a plot with average hourly electricity price during in-leaf period
elec <-ggplot(data =  elec_price%>% 
                filter(month %in% c("5", "6", "7", "8", "9")) %>%
                group_by(hour) %>% 
                summarise(avr_price = mean(price))) +
  geom_point(aes(x=hour, y=avr_price))+
  geom_line(aes(x=hour, y=avr_price))+
  scale_x_continuous(breaks = seq(0, 24,by=2)) +
  labs(
    y = "Average Electricity Price (USD/kWh)",
    x = "Hour"
  ) +
  journal_theme()

elec

#hourly electricity price based on the time of the use chart
#consumption peak chart according to "https://www.oeb.ca/consumer-information-and-protection/electricity-rates/managing-costs-time-use-rates"

tou_categories <- data.frame(
  TOU_Level = c("Off-Peak", "Mid-Peak", "On-Peak", "Mid-Peak", "Off-Peak"),
  start_time = c(0, 6, 10, 16, 18),
  end_time = c(6, 10, 16, 18, 24),  # Adjusted to match the next start_time
  fill_color = c("#5CB85C", "#F0AD4E", "red", "#F0AD4E", "#5CB85C")) # Colors for Off-Peak, Mid-Peak, and On-Peak

# Create the plot
elec_with_shading <- ggplot(data = elec_price %>%
                              filter(month %in% c("5", "6", "7", "8", "9")) %>%
                              group_by(hour) %>%
                              summarise(avr_price = mean(price))) +
  # Add shaded areas for TOU categories
  geom_rect(data = tou_categories,
            aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf, fill = TOU_Level),
            alpha = 0.5, inherit.aes = FALSE) +
  # Add average price line and points
  geom_line(aes(x = hour, y = avr_price), color = "black") +
  geom_point(aes(x = hour, y = avr_price), color = "black") +
  # Customize x-axis and y-axis
  scale_x_continuous(breaks = seq(0, 24, by = 2)) +
  labs(
    y = "Electricity Price (USD/kWh)",
    x = "Hour"
  ) +
  # Add legend for TOU levels and their colors
  scale_fill_manual(
    name = "  TOU in Summer Weekdays",
    values = setNames(tou_categories$fill_color, tou_categories$TOU_Level)
  ) +
  # Apply theme
  journal_theme()+
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.8, 'cm'),,  # Add space between legend keys
    legend.key.width = unit(0.8, "cm")  # Adjust the width of the legend color boxes
    
  )

#Electricity savings and price together
ggarrange(save + scale_x_continuous(limits = c(0, 24), breaks = c(0, 6, 12, 18)),
          elec_with_shading+scale_x_continuous(limits = c(0, 24), breaks = c(0, 6, 12, 18)),
          labels = c("A", "B"),
          nrow = 2,ncol = 1,
          align="v",
          label.x = 0.78,   # 1 = right edge
          label.y = 1,      # 1 = top
          hjust = 1,        # right-align text
          vjust = 1)

ggsave("../figures_tables_output/save-price-peak3.jpg",width = 15,height = 15,units = c("cm"),dpi=300)

#annual savings by tree canopy
##############################

prediction_sample_lasso %>%
  group_by(address_index, tree_percent) %>%
  inner_join(elec_price) %>%
  inner_join(prices) %>%
  summarise(tree_saving_predicted_dollars = sum(tree_saving_predicted * elec * price)) %>%
  ggplot(aes(x=tree_percent, y=tree_saving_predicted_dollars)) +
  geom_point() +  
  geom_smooth(method="lm", se=F, colour="firebrick") +
  journal_theme()+
  scale_x_continuous(name="Urban Tree Canopy Coverage", labels=scales::percent_format()) +
  scale_y_continuous(name="Annual Electricity Savings (USD)", labels=scales::dollar_format())

ggsave("../figures_tables_output/annual save.jpg",width = 8,height = 8,units = c("cm"),dpi=300)


#consumption by load duration curve and between scenarios
####################################################################
#define scenarios and calculate the predicted consumption within scenarios
t<- prediction_sample_lasso %>%
  mutate(actual_consumption = elec,
         predicted_consumption_notree = elec / (1 + tree_saving_predicted)) %>%
  group_by(time) %>%
  summarise(
    predicted_consumption_notree = mean(predicted_consumption_notree)) %>%
  arrange(desc(predicted_consumption_notree)) %>%
  mutate(hour = row_number()) %>%
  select(hour, predicted_consumption_notree) %>%
  inner_join(
    prediction_sample_lasso %>%
      mutate(actual_consumption = elec) %>%
      group_by(time) %>%
      summarise(
        actual_consumption = mean(actual_consumption)) %>%
      arrange(desc(actual_consumption)) %>%
      mutate(hour = row_number()) %>%
      select(hour, actual_consumption)
  )

#Load duration curve for each scenario
# Get the values at hour == 1
t1 <- t %>%
  pivot_longer(cols = -1) %>%
  filter(hour == 1) %>%
  select(name, value)

# Calculate % difference (relative to "no tree")
tree_val <- t1$value[t1$name == "actual_consumption"]
no_tree_val <- t1$value[t1$name == "predicted_consumption_notree"]
perc_diff <- 100 * (no_tree_val - tree_val) / no_tree_val

ggplot(data=t %>% pivot_longer(cols=-1), aes(x=hour, y=value, colour=name, linetype=name)) +
  geom_line(linewidth=1.2) +
  journal_theme() +
  labs(
    x="Hour",
    y="Electricity Consumption (kWh/h)") +  
  scale_color_manual(name="Scenario", values = c("actual_consumption" = "#228B22", "predicted_consumption_notree" = "#E69F00"), labels = c("actual_consumption" = "Current UTC", "predicted_consumption_notree" = "No UTC")) + 
  scale_linetype_manual(name="Scenario", values=c("actual_consumption" = "solid", "predicted_consumption_notree" = "dotdash"), labels = c("actual_consumption" = "Current UTC", "predicted_consumption_notree" = "No UTC")) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.95, 0.6),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  geom_segment(data = t1,
               aes(x = 1, xend = 2000, y = value, yend = value),
               linetype = "dotted",
               inherit.aes = FALSE) +
  # Text annotation showing difference
  annotate("text",
           x = 4500,
           y = mean(c(tree_val, no_tree_val)),
           label = paste0("Δ Consumption = ", round(perc_diff, 1), "%"),
           size = 4, fontface = "italic")


ggsave("../figures_tables_output/load-duration.jpg",width = 8,height = 8,units = c("cm"),dpi=300)



#predicted savings by weather variables (when there is no correlation between variables) 
#######################################################################################
## load the model objects
#########################


journal_theme <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = "black", size = base_size, family = base_family),
      axis.title = element_text(face = "bold", color = "black"),
      axis.text = element_text(color = "black"),
      legend.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.ticks = element_line(color = "black"),  # Ensure tick marks are visible
      axis.ticks.length = unit(0.2, "cm"),  # Length of the tick marks
      axis.line = element_line(color = "black")  # Ensure axis lines are visible
    )
}

dat <- read_csv("../intermediate_data/elecdat.csv") %>%
  # Create time variables in correct time zone
  mutate(time = force_tz(time, "EST")) %>%
  mutate(month = as.character(month(time, label=TRUE)), 
         hour = hour(time), 
         weekday = weekdays(time)) %>%
  mutate(weekend = as.numeric(weekday=="Saturday" | weekday == "Sunday")) %>%
  mutate(summer = as.numeric(month %in% c("May", "Jun", "Jul", "Aug", "Sep"))) %>%
  # Join with tree cover
  inner_join(
    read_csv(treedat)
  ) %>%
  # Join with weather and sun data
  inner_join(
    read_csv("../intermediate_data/weather.csv")
  ) %>%
  inner_join(
    read_csv("../intermediate_data/sun-pos.csv")
  )

est_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/estimation_hh.csv")
  )

pred_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/prediction_hh.csv")
  )

# Trained LASSO model
lasso.elec <- readRDS("../intermediate_data/final_lasso_model.rds")
# LASSO model formula
model.matrix.train.formula <- readRDS("../intermediate_data/model_matrix_train_formula.rds")
# Weather variables
# These need to be exactly the same as in estimate_lasso_final.R
vars <- c("cdd", "hdd", "rel_hum", "wind_spd", "elevation", "weekend", "daylight", "cos_az", "sin_az", "1")

#################################################################################
# This is all combinations of variables                                         #
combinations_no_repetition <- t(combn(vars, 2))                                 #
# Generate combinations with repetition                                         #
repeated_combinations <- t(sapply(vars[-length(vars)], function(x) c(x, x)))    #
rownames(repeated_combinations) <- NULL
# Combine both sets of combinations                                             #
all_combinations <- rbind(combinations_no_repetition, repeated_combinations)    #
# Now loop over this.                                                           #
#################################################################################
#Electricity savings by LASSO when temperature is the only variable that is not constant at its mean

#create the prediction sample in which all variables except for temp are at mean
new_sample <- pred_12.5 %>%
  # keep in-leaf period only
  filter(summer == 1) %>%
  # remove temperature variable
  select(-temp) %>%
  summarise(across(everything(), mean)) %>%
  expand_grid(temp = 0:35) %>%
  mutate(tree_summer = tree_percent * summer) %>%
  mutate(hdd = pmax(0, 18-temp),
         cdd = pmax(0, temp-18))


#creating new interaction variables (with tree_summer and weather variables) 
for (i in 1:nrow(all_combinations)) {
  w = all_combinations[i,1]
  v = all_combinations[i,2]
  
  print(paste("Done",i,"out of",length(vars)^2-length(vars)))
  flush.console()
  
  new_var_name <- paste("tree_summer",v,w,sep="_")
  
  if (v == "1") {
    new_sample[[new_var_name]] = new_sample$tree_summer * new_sample[[w]] 
  } else {
    new_sample[[new_var_name]] = new_sample$tree_summer * new_sample[[v]] * new_sample[[w]]
  }
}



#remove the dots in the name of demeaned variables 
# Same model matrix as above, but without dots
model.matrix.saving <- model.matrix(formula(paste(gsub("_dot","",model.matrix.train.formula), collapse=" ")) , new_sample)


# Predictions (temperature is the only variable that changes over time)
new_sample$tree_saving_predicted = predict(lasso.elec, model.matrix.saving) %>% as.numeric()
# This is in log points. Convert to percent using (exp(log_saving)-1)
new_sample$tree_saving_predicted <- exp(new_sample$tree_saving_predicted) - 1

temp <-new_sample %>% 
  group_by(temp) %>% 
  summarise(tree_saving_predicted=mean(tree_saving_predicted)) %>% 
  ggplot(aes(x=temp, y=tree_saving_predicted)) + 
  geom_line(colour="red") +
  journal_theme() +
  scale_y_continuous(name="Predicted Electricty Savings", labels=scales::percent_format()) +
  labs(x="Temp. (°C)")

temp

#add the density plot to temp plot
# Density plot of Temperature
dens_plot_temp <- ggplot(pred_12.5 %>% filter(summer==1), aes(x = temp)) +
  geom_density(fill = "gray", color = "black", alpha = 0.4) +
  journal_theme() +
  labs(x = NULL, y = "Density")  

# Arrange plots: Place density plot above the main plot
combined_plot_temp <- ggarrange(
  dens_plot_temp, temp,
  ncol = 1, nrow = 2, align = "v", heights = c(1, 2)  # Histogram smaller than main plot
)

#repeat the same steps for relative humidity
#humidity is the only variable not constant at its mean

#create the prediction sample in which all variables except for humidity are constant at thier mean
new_sample_hum <- pred_12.5 %>%
  # keep in-leaf period only
  filter(summer == 1) %>%
  # remove temperature variable
  select(-rel_hum) %>%
  summarise(across(everything(), mean)) %>%
  expand_grid(rel_hum = 10:100) %>%
  mutate(tree_summer = tree_percent * summer) 

#creating new interaction variables (with tree_summer and weather variables) 

for (i in 1:nrow(all_combinations)) {
  w = all_combinations[i,1]
  v = all_combinations[i,2]
  
  print(paste("Done",i,"out of",length(vars)^2-length(vars)))
  flush.console()
  
  new_var_name <- paste("tree_summer",v,w,sep="_")
  
  if (v == "1") {
    new_sample_hum[[new_var_name]] = new_sample_hum$tree_summer * new_sample_hum[[w]] 
  } else {
    new_sample_hum[[new_var_name]] = new_sample_hum$tree_summer * new_sample_hum[[v]] * new_sample_hum[[w]]
  }
}



#remove the dots in the name of demeaned variables 
# Same model matrix as above, but without dots
model.matrix.saving <- model.matrix(formula(paste(gsub("_dot","",model.matrix.train.formula), collapse=" ")) , new_sample_hum)


# Predictions (Humidity is the only variable that changes over time)
new_sample_hum$tree_saving_predicted = predict(lasso.elec, model.matrix.saving) %>% as.numeric()
# This is in log points. Convert to percent using (exp(log_saving)-1)
new_sample_hum$tree_saving_predicted <- exp(new_sample_hum$tree_saving_predicted) - 1

hum <-new_sample_hum %>% 
  group_by(rel_hum) %>% 
  summarise(tree_saving_predicted=mean(tree_saving_predicted)) %>% 
  ggplot(aes(x=rel_hum, y=tree_saving_predicted)) + 
  geom_line(colour="red") +
  journal_theme() +
  scale_y_continuous(name="Predicted Electricty Savings", labels=scales::percent_format())+
  labs(x="Rel. Humidity (%)")


#add the density plot to humidity plot
# Density plot of  of humidity
dens_plot_hum <- ggplot(pred_12.5 %>% filter(summer == 1), aes(x = rel_hum)) +
  geom_density(fill = "gray", color = "black", alpha = 0.4) +
  journal_theme() +
  labs(x = NULL, y = "Density") 

# Arrange plots: Place histogram above the main plot
combined_plot_hum <- ggarrange(
  dens_plot_hum, hum,
  ncol = 1, nrow = 2, align = "v", heights = c(1, 2)
)

#wind is the only variable not constant at its mean

#create the prediction sample in which all variables except for wind are constant at their mean
new_sample_wind <- pred_12.5 %>%
  # keep in-leaf period only
  filter(summer == 1) %>%
  # remove temperature variable
  select(-wind_spd) %>%
  summarise(across(everything(), mean)) %>%
  expand_grid(wind_spd = 0:60) %>%
  mutate(tree_summer = tree_percent * summer) 


#creating new interaction variables (with tree_summer and weather variables) 

for (i in 1:nrow(all_combinations)) {
  w = all_combinations[i,1]
  v = all_combinations[i,2]
  
  print(paste("Done",i,"out of",length(vars)^2-length(vars)))
  flush.console()
  
  new_var_name <- paste("tree_summer",v,w,sep="_")
  
  if (v == "1") {
    new_sample_wind[[new_var_name]] = new_sample_wind$tree_summer * new_sample_wind[[w]] 
  } else {
    new_sample_wind[[new_var_name]] = new_sample_wind$tree_summer * new_sample_wind[[v]] * new_sample_wind[[w]]
  }
}



#remove the dots in the name of demeaned variables 
# Same model matrix as above, but without dots
model.matrix.saving <- model.matrix(formula(paste(gsub("_dot","",model.matrix.train.formula), collapse=" ")) , new_sample_wind)


# Predictions (wind is the only variable that changes over time)
new_sample_wind$tree_saving_predicted = predict(lasso.elec, model.matrix.saving) %>% as.numeric()
# This is in log points. Convert to percent using (exp(log_saving)-1)
new_sample_wind$tree_saving_predicted <- exp(new_sample_wind$tree_saving_predicted) - 1

wind <-new_sample_wind %>% 
  group_by(wind_spd) %>% 
  summarise(tree_saving_predicted=mean(tree_saving_predicted)) %>% 
  ggplot(aes(x=wind_spd, y=tree_saving_predicted)) + 
  geom_line(colour="red") +
  journal_theme() +
  scale_y_continuous(name="Predicted Electricty Savings", labels=scales::percent_format()) +
  labs(x="Wind Spd. (km/h)")

#add the density plot to wind plot 

dens_plot_wind<- ggplot(pred_12.5 %>% filter(summer == 1), aes(x = wind_spd)) +
  geom_density(fill = "gray", color = "black", alpha = 0.4) +
  journal_theme() +
  labs(x = NULL, y = "Density")  # Remove x label for cleaner alignment

# Arrange plots: Place density above the main plot
combined_plot_wind <- ggarrange(
  dens_plot_wind, wind,
  ncol = 1, nrow = 2, align = "v", heights = c(1, 2)  # Histogram smaller than main plot
)

#plot all weather variables together
ggarrange(combined_plot_temp,
          combined_plot_hum,
          combined_plot_wind,
          labels = c("A","B","C"),
          nrow = 1,ncol = 3)

ggsave("../figures_tables_output/tree-weather-interaction.jpg",width = 15,height = 15,units = c("cm"),dpi=300)

#Comparison of savings from current UTC and adjusted UTC
########################################################

prediction_sample_lasso <- read_csv("../intermediate_data/Lasso-prediction.csv")

# Trained LASSO model
lasso.elec <- readRDS("../intermediate_data/final_lasso_model.rds")
# LASSO model formula
model.matrix.train.formula <- readRDS("../intermediate_data/model_matrix_train_formula.rds")
# Weather variables
# These need to be exactly the same as in estimate_lasso_final.R
vars <- c("cdd", "hdd", "rel_hum", "wind_spd", "elevation", "weekend", "daylight", "cos_az", "sin_az", "1")


# Predicted savings from adjusted tree canopy 
prediction_sample_adj <- pred_12.5 %>%
  mutate(
    # Calculate the needed amount to reach canopy of 1
    needed_tree = 1 - tree_percent,
    
    # Calculate a quarter of this amount
    add_tree = needed_tree * 0.25,
    
    # Add this quarter amount to the existing tree canopy
    tree_percent = pmin(tree_percent + add_tree, 1)) %>% 
  mutate(tree_summer = tree_percent * summer)


#################################################################################
# This is all combinations of variables                                         #
combinations_no_repetition <- t(combn(vars, 2))                                 #
# Generate combinations with repetition                                         #
repeated_combinations <- t(sapply(vars[-length(vars)], function(x) c(x, x)))    #
rownames(repeated_combinations) <- NULL
# Combine both sets of combinations                                             #
all_combinations <- rbind(combinations_no_repetition, repeated_combinations)    #
# Now loop over this.                                                           #
#################################################################################

# This generates the model matrices (data)
for (i in 1:nrow(all_combinations)) {
  w = all_combinations[i,1]
  v = all_combinations[i,2]
  
  print(paste("Done",i,"out of",nrow(all_combinations),"with size",object.size(prediction_sample_adj)/1e9,"GB"))
  flush.console()
  
  new_var_name <- paste("tree_summer",v,w,sep="_")
  
  if (v == "1") {
    prediction_sample_adj[[new_var_name]] = prediction_sample_adj$tree_summer * prediction_sample_adj[[w]] 
  } else {
    prediction_sample_adj[[new_var_name]] = prediction_sample_adj$tree_summer * prediction_sample_adj[[v]] * prediction_sample_adj[[w]]
  }
  
  
}


# Rename the coefficients in LASSO model to remove dots
row.names(lasso.elec$beta) = gsub("_dot","", x=row.names(lasso.elec$beta))


# Same model matrix as above, but without dots
model.matrix.saving <- model.matrix(formula(paste(gsub("_dot","",model.matrix.train.formula), collapse=" ")) , prediction_sample_adj)


# Predictions
prediction_sample_adj$tree_saving_predicted = predict(lasso.elec, model.matrix.saving) %>% as.numeric()
# This is in log points. Convert to percent using (exp(log_saving)-1)
prediction_sample_adj$tree_saving_predicted <- exp(prediction_sample_adj$tree_saving_predicted) - 1

prediction_sample_adj <- prediction_sample_adj %>% select(-needed_tree,-add_tree)


#Annual average savings from current canopy
avr_saving_current <- prediction_sample_lasso %>% group_by(time) %>%
  summarise(avr_saving=mean(tree_saving_predicted)) %>%
  mutate(scenario=c("Current UTC")) 

#annual average savings from adjusted canopy
avr_saving_adj <- prediction_sample_adj %>% group_by(time) %>%
  summarise(avr_saving=mean(tree_saving_predicted)) %>%
  mutate(scenario=c("Adjusted UTC"))

#Join the two scenarios
avr_save_current_adj <- bind_rows(avr_saving_current, avr_saving_adj)

#define the levels 
avr_save_current_adj <- avr_save_current_adj %>%
  mutate(
    scenario = factor(scenario, levels = c("Current UTC", "Adjusted UTC")),  # Ensure order
    scenario_order = factor(scenario, levels = c("Current UTC", "Adjusted UTC"), labels = c("1", "2"))  # Numeric labels
  )

scenario_labels <- c("1" = "Current UTC", "2" = "Adjusted UTC")  # Define custom labels

#dashed line indicating the average annual predicted savings from each scenario
thresholds <- tibble::tibble(
  scenario_order = factor(c("1", "2"), levels = c("1", "2")),  # Match factor labels
  yintercept = c(-0.029, -0.050)  # Keep original threshold values
)

scenario_labels <- c("1" = "Current UTC", "2" = "Adjusted UTC")  # Labels for facet strips

ggplot(data = avr_save_current_adj, aes(x = time, y = avr_saving, color = scenario)) +
  geom_line() +
  geom_hline(data = thresholds, aes(yintercept = yintercept), 
             color = "black", linetype = "dashed", size = 0.7) +
  facet_wrap(~ scenario_order, labeller = labeller(scenario_order = scenario_labels)) +  # Use updated facet labels
  scale_y_continuous(name = "Average Predicted Electricity Savings", labels = percent_format()) +
  scale_x_datetime(
    name = "Time",
    date_labels = "%b",
    date_breaks = "2 month"
  ) +
  scale_color_manual(
    name = "Scenarios",
    values = c("Current UTC" = "red", "Adjusted UTC" = "steelblue")
  ) +
  labs(x = "Time") +
  journal_theme() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    strip.text = element_text(size = 12, face = "bold"),  # Adjust facet strip labels
    legend.position = "none"
  )


ggsave("../figures_tables_output/adj-current-scenario.jpg",width = 15,height = 15,units = c("cm"),dpi=300)

                                                               ###############
################################################################ SECTION 3 #####################################################
                                                               ###############
#REGRESSION MODELS 

#clear all data frames from environment

#read the estimation sample for:
#1-tree canopy measures (%) within 12.5 meter
#2-tree canopy measures (sq-m) within all buffers (5, 12.5, 20)
#3-tree canopy measures (%) within all directions (N,E,S,W)

dat <- read_csv("../intermediate_data/elecdat.csv") %>%
  # Create time variables in correct time zone
  mutate(time = force_tz(time, "EST")) %>%
  mutate(month = as.character(month(time, label=TRUE)), 
         hour = hour(time), 
         weekday = weekdays(time)) %>%
  mutate(weekend = as.numeric(weekday=="Saturday" | weekday == "Sunday")) %>%
  mutate(summer = as.numeric(month %in% c("May", "Jun", "Jul", "Aug", "Sep"))) %>%
  # Join with tree cover
  inner_join(
    read_csv(treedat)
  ) %>%
  # Join with weather and sun data
  inner_join(
    read_csv("../intermediate_data/weather.csv")
  ) %>%
  inner_join(
    read_csv("../intermediate_data/sun-pos.csv")
  )

est_12.5 <- dat %>%
  inner_join(
    read_csv("../intermediate_data/estimation_hh.csv")
  )


buffer_est <- est_12.5 %>%
  inner_join(
    read_csv(bufferdat)
  )

direction_est <- est_12.5 %>%
  inner_join(
    read_csv(directiondat)
  )

#Regression models for tree canopy measures within 12.5 meter buffer
#Base regression with address and time Fixed effects
formula1 <- log(elec)~ tree_percent*summer|address_index+time
model1 <- feols(formula1, data = est_12.5,cluster = ~ address_index + time)

#Regression models with higher resolution Fixed Effects
#house by hour Fixed Effect
formula2 <- log(elec)~ tree_percent *summer|address_index^hour + time
model2 <- feols(formula2, data = est_12.5, cluster = ~ address_index + time)

#house by weekday Fixed Effects
formula3 <- log(elec)~ tree_percent *summer|address_index^weekend + time
model3 <- feols(formula3, data = est_12.5,cluster = ~ address_index + time)

#time by street index Fixed Effects
formula4 <- log(elec)~ tree_percent *summer|address_index + time^street_index
model4 <- feols(formula4, data = est_12.5,cluster = ~ address_index + time)

#house by weekday and hour Fixed Effects
formula5 <- log(elec)~ tree_percent *summer|address_index^weekend+address_index^hour+ time
model5 <- feols(formula5, data = est_12.5,cluster = ~ address_index + time)

#house by weekday and house, time by street index Fixed Effects
formula6 <- log(elec)~ tree_percent *summer|address_index^weekend+ address_index^hour+ time^street_index
model6 <- feols(formula6, data = est_12.5,cluster = ~ address_index + time)

#Regression models for net canopy measures within all buffers 
#model is based on the squared meter of tree canopy within each buffer
#model is for preferred regression model: address by hour, address by weekday, time by street-index Fixed effects 

formula8 <- log(elec)~ I((buff_5+buff_12.5)/1e9):summer + I((buff_20)/1e9):summer|address_index^weekend+ address_index^hour+ time^street_index
model8 <- feols(formula8, data = buffer_est,cluster = ~address_index+time)

#Regression models for canopy measures within 12.5 meter of all directions
#model is based on the percentage of tree canopy within each direction
#model is for preferred regression model: address by hour, address by weekday, time by street-index Fixed effects 

formula9 <- log(elec)~ dir_north*summer + dir_east*summer+dir_west*summer+dir_south*summer|
  address_index^weekend+ address_index^hour+ time^street_index
model9 <- feols(formula9, data = direction_est,cluster = ~ address_index + time)

#The results of all regression models in a table
results_df <- modelsummary(list("Model 1"=model1,"Model 2"=model2,"Model 3"=model3,"Model 4"=model4,
                  "Model 5"=model5,"Model 6"=model6,"Model 7"=model8,"Model 8"=model9),
             stars = c('*' = .1, '**' = .05, '***' = .01),output = "data.frame")


write.csv(results_df, "../intermediate_data/regression_results.csv", row.names = FALSE)
