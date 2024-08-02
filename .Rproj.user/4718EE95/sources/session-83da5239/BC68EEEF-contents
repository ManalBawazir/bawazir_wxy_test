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
library(openxlsx)

out_dic <- "C:/Users/manal/Documents/dob_permit/output"
raw_data <- read_csv("C:/Users/manal/Documents/dob_permit/raw/dob_permit_issuance.csv")

# EXPLORING THE RAW DATASET -------------------------------
# Taking a look at the data
data_head <- raw_data %>%
  head(10)

# Skimming the data
data_summary <- raw_data %>%
  clean_names() %>%
  skim()
# Exploring job type values to identify major construction/ new Building permits 
unique_values <- raw_data %>%
  clean_names() %>%
  select(job_type) %>%
  unique()

# CLEANING The RAW DATASET -------------------------------
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
  labs(x = "Permit Issuance Year", 
       y = "Mean of New Building Permits", 
       caption = "Source: NYC Open Data") +
  theme_minimal()
print(plot_1)

# Mapping active and expired permits in Staten Island

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
  coord_sf(xlim =c(-70.151535,-75.151535), ylim=c(40.579021,44.579021))

print(si_map)


# SPATIAL JOIN  -------------------------------
nta_sf <- read_sf("C:/Users/manal/Documents/dob_permit/raw/nta_map") %>%
  rename(nta_name = ntaname)

joined_data <- left_join(cleaned_data, nta_sf, by ="nta_name")

summary_table_2 <- joined_data %>%
  group_by(nta_name, active_status, issuance_year) %>%
  summarise(count = n(),
            mean = mean(job_type == "NB"),
            sd = sd(job_type == "NB")) %>%
  ungroup() %>%
  drop_na()

summary_table_2023 <- summary_table_2 %>%
  filter(issuance_year == "2023")

plot_2 <- ggplot(summary_table_2023, aes(x = active_status,
                                         y= mean)) +
  geom_col() +
  labs(title = "New Building Permits Status in 2023",
       x = "Permit Status",
       y = "Averge New Building Permits",
       caption = "Source: NYC Open Data") +
  theme_minimal()

print(plot_2)

summary_table_midtown_south <- summary_table_2 %>%
  filter(nta_name == "Midtown-Midtown South")

plot_3 <- ggplot(summary_table_midtown_south, aes(x = issuance_year,
                                                  y = count,
                                                 col = active_status)) +
                   geom_point() +
  labs(title = "Summary of Issued New Building Permits in Midtown South By Year",
       x = "Issuance Year",
       y = "Number of New Building Permits",
       Caption = "Source: NYC Open Data") +
  theme_minimal()
      
print(plot_3)

# EXPORTING -------------------------------
# Figures

ggsave(plot_2, 
       filename = "permit_status.png",
       device = "png",
       path = out_dic,
       height = 6, width = 5, units = "in")

ggsave(plot_3,
       file_name = "midtown_permit.png",
       device = "png",
       path = out_dic,
       height = 6, width = 5, units = "in")

# Data
st_write(summary_table_2, "C:/Users/manal/Documents/dob_permit/output/permit_nta.shp")
write.xlsx(summary_table_2,"C:/Users/manal/Documents/dob_permit/output/permit_nta.xlsx")

