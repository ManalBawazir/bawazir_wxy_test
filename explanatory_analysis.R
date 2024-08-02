
# At a minimum:
# Create a summary table with key metrics
# Visually explore key metrics, including at least one time series graph and one map
# Perform a spatial join against another layer of your choice (e.g. Census layers, transit walksheds, etc.)
# Create a summary table making use of the joined data points
# Export one graph and one chart that are presentation-ready or could be slightly refined in Adobe CC
# Export your final data to excel and shapefile

# Optionally, include some extra analysis or functionalities of your choice. A few ideas:
# Perform a statistical test on the data (e.g. linear regression, clustering) and characterize results
# Scale/normalize key metrics and create a composite indicator of development activity
# Identify existing parcel land uses and buildings to understand how the build landscape is changing

# Reread the code
# rename the r file to "Exploratory" 
# HEADER -------------------------------------------------

# PROJECT: Geospatial Analysis
# AUTHOR: Manal Bawazir
# DATE: 2024-07-31

# PURPOSE: Explore and analyze issued permits for the construction of new buildings in NYC

# SET UP -------------------------------------------------
library(tidyverse)
library(skimr)
library(janitor)
library(ggplot2)
library(sf)
library(lubridate)
library(maps)
out_dic <- "C:/Users/manal/Documents/dob_permit/output"
raw_data <- read_csv("C:/Users/manal/Documents/dob_permit/raw/dob_permit_issuance.csv")

# EXPLORING THE RAW DATASET -------------------------------------------------
# Taking a look at the data
data_head <- raw_data %>%
  head(10)

# skimming the data
data_summary <- raw_data %>%
  clean_names() %>%
  skim()
# Exploring Job Type values to identify major construction/ new Building permits 
unique_values <- raw_data %>%
  clean_names() %>%
  select(job_type) %>%
  unique()

# CLEANING The RAW DATASET -----------------------------------------------------
cleaned_data <- raw_data %>%
  clean_names() %>%
  mutate(issuance_date = mdy(issuance_date),
         issuance_year = year(issuance_date),
         expiration_date = mdy(expiration_date),
         expiration_year = year(expiration_date),
         active_status = case_when(expiration_date > '2024-07-31' ~ "Active",
                                   expiration_date < '2024-07-31' ~ "Expired"))

# Creating a summary table
summary_table_1 <- cleaned_data %>%
  group_by(borough, issuance_year) %>%
  summarise(count = n(),
            mean = mean(job_type == "NB"),
            sd = sd(job_type == "NB")) %>%
  ungroup() %>%
  drop_na()

# Visualizing issued new building permits by year 
plot_1 <- ggplot(summary_table_1,                            
                 aes(x = issuance_year,
                     y = mean,
                     col = borough)) + 
          geom_line() +
  labs(x = "Permit Issuance Year", y = "Mean of New Building Permits", caption = "Source: NYC Open Data") +
  theme_minimal()
print(plot_1)

  
# Mapping active and expired permits in staten Island

si_active_permits <-  cleaned_data %>%
  select(job_type, borough, active_status, latitude, longitude) %>%
  filter(job_type == "NB",
         borough == "STATEN ISLAND",
         active_status == "Active") %>%
  drop_na() 

si_expired_permits <- cleaned_data %>%
  select(job_type, borough, active_status, latitude, longitude) %>%
  filter(job_type == "NB",
         borough == "STATEN ISLAND",
         active_status == "Expired") %>%
  drop_na() 

world <- map_data("world")
si_map <- ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  geom_point(data=si_active_permits, aes(x=longitude, y=latitude), colour="blue4", pch=20, size=2) +
  geom_point(data=si_expired_permits, aes(x=longitude, y=latitude), colour="red4", pch=20, size=2) +
  coord_sf(xlim =c(-73.151535,-75.151535), ylim=c(41.579021,42.579021))

print(si_map)


# SPATIAL JOIN --------------------------------------------------
nta_sf <- read_sf("C:/Users/manal/Documents/dob_permit/raw/nta_map") %>%
  rename(nta_name = ntaname)

joined_data <- left_join(cleaned_data, nta_sf, by ="nta_name")

summary_table_2 <- joined_data %>%
  filter(job_type == "NB",
         ) %>%
  group_by( nta_name, shape_area_)
  
export_png


How many new building permits are issued for every 1 meter in each nta?
  whats th eocncerntion of active new building permits in each nta?
# EXPORTING ---------------------------------------------------
# Figures
ggsave(irma_fatalities_plot, 
       filename = "irma_fatalities.pdf",
       device = "png",
       height = 6, width = 5, units = "in")

# Data
