library(metaDigitise)
library(here)
library(tidyverse)
library(stringr)
library(broom)
library(nlraa)
library(stringi)

### Collate all published data on temperature, light and oxygen from degradation experiments when averages not reported within study  

### 1. Import datasets from tables, downloaded from published articles.  

wright22 <- read_csv(here::here("data_toextract/temperature","Wright22_tempdata.csv"))
wright22 <- wright22 %>% 
  summarise(temperature_degree = mean(temp))

filbeedexter22 <- read_csv(here::here("data_toextract/temperature","filbeedexter22_tempdata.csv"))

short_experiment <- filbeedexter22  %>%  # some treatments finished early due to lost bags etc so need to calculate temperature for first half of experiment only i.e. until retrieval 1
  filter(Region == "Portugal" | Region == "Rhode I"| Region == "New England"| Site =="Batton Bay" |  Site =="Paddys Head"|  Site =="Site 1") %>% 
  filter (retrieval == 1) %>% 
  group_by(Region, Species, Site, retrieval)%>% 
  summarise( experiment_temp = mean(MeanTemperature), experiment_light = mean(MeanLight)) %>% 
  select (-retrieval)

long_experiment <- filbeedexter22  %>% 
  filter(Region != "Portugal" & Region != "Rhode I"& Region != "New England" & Site !="Batton Bay" &  Site !="Paddys Head" & Site !="Site 1") %>% 
  group_by(Region, Species, Site, retrieval)%>% 
  summarise( pooled_mean_temp = mean(MeanTemperature*Days), pooled_mean_light = mean(MeanLight*Days), Days = unique(Days)) %>% 
  group_by(Region, Species, Site) %>% 
  summarise(new_mean_temp = sum(pooled_mean_temp), new_mean_light = sum(pooled_mean_light), total_days = sum(Days), 
            experiment_temp = new_mean_temp/total_days, experiment_light = new_mean_light/total_days ) %>% 
  select (-total_days, -new_mean_temp, -new_mean_light)

all <- rbind(short_experiment,long_experiment)

### 2. Import extracted datasets from figures, downloaded from published articles.  

# load extracted data using metaDigitise, it locates calibrated data stored in "caldat" file to each figure in "figures_toextract" folder
# metaDigitise allows users to extract information from figures directly within the R environment
# just need to provide the directory path name of the folder with the image to the metaDigise() function 

# Pick, J.L., Nakagawa, S., Noble D.W.A. (2018) 
# Reproducible, flexible and high-throughput data extraction from primary 
# literature: The metaDigitise R package. Biorxiv, https://doi.org/10.1101/247775

rawdata <- metaDigitise(dir = here::here ("figures_toextract"), summary = FALSE)
2
1

scatter <- rawdata[[3]] # third list is a list of scatter plot data 
scatter_df <- scatter %>% bind_rows(.id = "filename")
unique(scatter_df$filename) 

# tidying up

# select temperature data that i extracted 
scatter_temperature <- scatter_df %>% 
  filter(filename == "conover_temperature.png" )

# select oxygen data that i extracted 

scatter_DO <- scatter_df %>% 
  filter(filename == "74_yang_temperature.png" | filename == "576_luo_temperature.png")
