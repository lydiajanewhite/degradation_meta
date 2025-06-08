library(tidyverse)
library(here)
library(stringr)
library(worrms)
library(ggpubr)
library(patchwork)

### Explore how k varies with temperature and latitude 

### 1. Import data

prelim <- read_csv(here::here("data","database.csv")) # corresponds to "database" sheet from published excel file 

prelim <- prelim |> 
  mutate(
    across(where(is.character), ~ str_replace_all(.x, "\\s", " ")),
    across(where(is.character), ~ str_remove(.x, " $")), #trimming end space
    depth_m = as.numeric(depth_m),
    mesh_size_mm = as.numeric (mesh_size_mm),
    duration_days = as.numeric(duration_days), 
    size_g = as.numeric(size_g),
    `k (d-1)` = as.numeric(`k (d-1)`)
  ) |> rename(k = `k (d-1)`) 


# update the names for old species names that have a been renamed/reclassified

# fix spelling

prelim <- prelim %>%
  mutate(
    species = tolower(species),
    species = case_match(species, "gracilaria tikvahia" ~ "gracilaria tikvahiae", .default = species),
    species = case_match(species, "laminaria ochroelucha" ~ "laminaria ochroleuca", .default = species)
  )

prelim_names <- prelim %>% 
  group_by(species) %>% 
  summarise(n = n()) %>% 
  print(n = 65) 

# check what current species name is on World Register of Marine Species and replace those that are no longer valid 

full <- wm_records_names(name = prelim_names$species)
names(full) <-prelim_names$species

name_comparison <- bind_rows(full, .id = "lydia_species") |> select (lydia_species, valid_name) %>%  # cant check name for 8 entries i.e. where only genus is recorded
  mutate (valid_name = tolower(valid_name)) %>% 
  mutate (same = (lydia_species == valid_name)) %>% 
  filter(same == "FALSE")

prelim$species <-gsub("agarophyton vermiculophyllum", "gracilaria vermiculophylla", prelim$species)
prelim$species <-gsub("agarum fimbriatum", "neoagarum fimbriatum", prelim$species) 
prelim$species <-gsub("ceramium rubrum", "ceramium virgatum", prelim$species)
prelim$species <-gsub("enteromorpha intestinalis", "ulva intestinalis", prelim$species)
prelim$species <-gsub("enteromorpha sp", "ulva sp", prelim$species)
prelim$species <-gsub("gigartina papillata", "mastocarpus papillatus", prelim$species)
prelim$species <-gsub("gracilaria lemaneiformis", "gracilariopsis lemaneiformis", prelim$species)
prelim$species <-gsub("gracilaria lichenoides", "hydropuntia edulis", prelim$species)
prelim$species <-gsub("gracilaria verrucosa", "gracilariopsis longissima", prelim$species)
prelim$species <-gsub("macrocystis integrifolia", "macrocystis pyrifera", prelim$species)
prelim$species <-gsub("monostroma obscurum", "ulvaria obscura", prelim$species)
prelim$species <-gsub("pilayella littoralis", "pylaiella littoralis", prelim$species)
prelim$species <-gsub("rhodomela larix", "neorhodomela larix", prelim$species)

# remove study that did not allow data extraction due to non-compatible figures

prelim <- prelim %>%  
  filter(study != "Goldbold et al 2009")


prelim <- prelim %>% unite("study_exp", "study", "experiment", remove = F)
prelim <- prelim %>% unite("sample", "study", "experiment", "species", "variable_1","variable_2" , "variable_3", remove = F)

# k is not available for all treatment due to a) not being reported, b) data not available, c) not modellable (r-square value ~ 0) or d) biomass did not decrease over duration of experiment 
# 38 instances where no meaningful k could be fitted or extracted 

kdata <- prelim %>%filter(!is.na (k)) 

### Tidy categories

kdata <- kdata %>%  
  mutate(depth_cat = ifelse(depth_m > 10, "y", "n"), 
         duration_cat = ifelse(duration_days > 90, "y", "n"),
         subtidal_cat = ifelse(depth_m > 0, "y", "n"),
         subtidal_cat = case_when(study_exp == "Franzitta et al 2015_1" ~ "y", study_exp == "Yang et al 2021_1" ~ "y", TRUE  ~ subtidal_cat), #	Franzitta et al 2015_1 is shallow subtidal but doesn't specify the depth and yang 21 uses in situ floating mesocosm so algae is submerged
         fresh_cat = case_match(fresh, "y" ~ "fresh", .default = "not fresh"),
         category = case_when(subtidal_cat == "n" ~ "intertidal" , subtidal_cat == "y" ~ "subtidal", TRUE  ~ set_up),
         temperature_degrees = as.numeric(temperature_degrees),
         latitude = abs(as.numeric(Lat))) 

 
k_temp_data <-filter(kdata, temperature_degrees != 'NA' )
  

x <- ggplot(k_temp_data, aes(x=temperature_degrees)) + 
  geom_histogram(binwidth=1) 
x + facet_wrap (~category) 

y <- ggplot(k_temp_data, aes(x=k)) + 
  geom_histogram(binwidth=0.1) 
y + facet_wrap (~category) 

# temperature is reasonably normal, but k not normal, k is very skewed, so use spearmans ranks correlation 

filter(k_temp_data) %>% 
  group_by(study_exp, category) %>% 
  summarise(n = n()) %>%
  group_by(category) %>%  # 43 experiments reported temperature 
  summarise(n = n())  # 20 in situ experiments reported temperature, 9 intertidal, 11 subtidal, 23 aquaria experiments reported temperature

filter(k_temp_data) %>% 
  group_by(category) %>% # 196 measurements of k also reported temperature 
  summarise(n = n())  # 104 in situ measures reported temperature, 22 intertidal, 82 subtidal, 92 aquaria measurements reported temperature

intertidal <- ggscatter(filter(k_temp_data, category == "intertidal"), x =  "temperature_degrees", y = "k",
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. 
          cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
) + ggtitle("intertidal") + 
  theme(legend.title=element_blank())


subtidal <- ggscatter(filter(k_temp_data, category == "subtidal"), x =  "temperature_degrees", y = "k",
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          cor.coef = TRUE, # Add correlation coefficient. 
          cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
) + ggtitle("subtidal")+
  theme(legend.title=element_blank())

aquaria <- ggscatter(filter(k_temp_data, category == "aquaria"), x =  "temperature_degrees", y = "k") + 
  ggtitle("aquaria")+
  theme(legend.title=element_blank())

# exclude one study due to extremely small fragments used (i.e. 500 times smaller than experiment mean) has extremely high k at low temperature
filter(k_temp_data, set_up == "aquaria") %>% 
  mutate(size_g = as.numeric(size_g)) %>% 
  filter(size_g != 'NA') %>% 
  summarise(mean = mean(size_g), min = min(size_g))

aquaria <- ggscatter(filter(k_temp_data, category == "aquaria" & study_exp != "Braeckman et al 2019_1" ), x ="temperature_degrees", y = "k",
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                     cor.coef = TRUE, # Add correlation coefficient. 
                     cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
) + ggtitle("aquaria")+
  theme(legend.title=element_blank())


intertidal + subtidal + aquaria 

# Latitude 

k_lat_data <- filter(kdata, set_up == "in situ" & latitude != 'NA' )

filter(k_lat_data) %>% 
  group_by(study_exp) %>% 
  summarise(n = n()) 
           
           
filter(k_lat_data) %>% 
  group_by(study_exp, category) %>% 
  summarise(n = n()) %>% 
  group_by(category) %>% 
  summarise(n = n())   # 26 intertidal experiments and 25 subtidal experiments report lattitude, so total of 50 in situ experiment (1 compares subtidal vs intertidal)  


filter(k_lat_data) %>% 
  group_by(category) %>% 
  summarise(n = n()) # 109 intertidal measurements and 127 subtidal measurements report latitude 

intertidal <- ggscatter(filter(k_lat_data, category == "intertidal"), x =  "latitude", y = "k",
                        add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                        cor.coef = TRUE, # Add correlation coefficient. 
                        cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
) + ggtitle("intertidal") +
  theme(legend.title=element_blank())


subtidal <- ggscatter(filter(k_lat_data, category == "subtidal"), x =  "latitude", y = "k",
                      add = "reg.line",  # Add regressin line
                      add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                      cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                      cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
) + ggtitle("subtidal")+
  theme(legend.title=element_blank())


intertidal + subtidal

