library(tidyverse)
library(here)
library(stringr)
library(worrms)

### Explore how k varies with different biological, environmental and experimental variables 
# at the data set level i.e. each unique treatment combination within an experiment has a degradation rate k 

### 1. Import data

prelim <- read_csv(here::here("data","database.csv")) # corresponds to "database" sheet from published excel file 

prelim %>%  filter (`k (d-1)` == "increased biomass") # 50 records increased biomass

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

# but for each of 85 experiments k data is available for at least one treatment or species of algae 
unique(kdata$study_exp) # 85 studies

# presence of outliers 
kdata %>% filter(k > 1) %>% 
  select (sample, species, k, size_g, duration_days)
kdata %>% filter(duration_days <5) # mews is the only study that is on time scale of less than 5 days   

# their removal does not influence results of statistical comparisons, 
# so leave them but use medians instead of means, and non-paramteric tests throughout analysis 

### Convert continuous variables into categories that were used in script 3 

kdata <- kdata %>%  
  mutate(depth_cat = ifelse(depth_m > 10, "y", "n"), 
         duration_cat = ifelse(duration_days > 90, "y", "n"),
         subtidal_cat = ifelse(depth_m > 0, "y", "n"),
         subtidal_cat = case_when(study_exp == "Franzitta et al 2015_1" ~ "y", TRUE  ~ subtidal_cat), # 	Franzitta et al 2015_1 is shallow subtidal but doesn't specify the depth.
         subtidal_cat = case_when(study_exp == "Yang et al 2021_1" ~ "y", TRUE ~ subtidal_cat), # in situ floating mesocosm so algae is submerged
         fresh_cat = case_match(fresh, "y" ~ "fresh", .default = "not fresh"))


### 2. Biological drivers of degradation

unique(kdata$species) # 51 different species have k values fitted to them 

# summarise across different phyla of macroalgae

kdata %>% group_by(colour) %>% 
  summarise (n = n())

kdata %>% group_by(colour) %>% 
  summarise (median = median(k), n = n())

kruskal.test(k ~ colour, data = kdata ) # p = 2.513e-05

pairwise.wilcox.test(kdata$k, kdata$colour,
                     p.adjust.method = "BH") # k lower for browns compared to green (p = 0.00061) and reds (p = 0.00193)

# how many experiments provide fauna access to degrading macroalgae

kdata %>% group_by(animal_consumers_present) %>% 
  summarise (n = n())

kdata %>% group_by(study_exp, animal_consumers_present) %>% 
  summarise (n = n()) %>% 
  group_by(animal_consumers_present) %>% 
  summarise (n = n()) 

kdata %>% group_by(animal_consumers_present) %>% 
  summarise (median = median(k), n = n())

kruskal.test(k ~ animal_consumers_present, data = filter(kdata, animal_consumers_present !="no access")) # p > 0.05


# what mesh size do most experiments use 

kdata %>% 
  group_by(mesh_size_mm) %>% 
  summarise (n = n()) %>%  # 10mm is most common 
  print(n = 21)


### 3. Environmental drivers of degradation

# light 
kdata %>% 
  filter(light_reported == "y") %>% 
  group_by(study_exp, set_up) %>% 
  summarise (n = n())

# oxygen  
kdata %>% 
  filter(oxygen_reported == "y") %>% 
  group_by(study_exp, set_up) %>% 
  summarise (n = n())

# temperature
kdata %>% 
  filter(temperature_reported == "y") %>% 
  group_by(study_exp, set_up) %>% 
  summarise (n = n()) %>% 
  group_by(set_up) %>% 
  summarise (n = n())

### 4. Experimental drivers of degradation

# how does k vary between habitats #

# intertidal vs subtidal 

kdata %>% group_by(subtidal_cat) %>% 
  summarise (median = median(k), n = n())

data1 <- kdata %>% 
  filter (!is.na (subtidal_cat)) # remove aquaria data 

kruskal.test(k ~ subtidal_cat, data = data1)

# excluding outlier gives same results 
# data1a <- data1 %>% 
#  filter (study != "Mews et al 2006")
#kruskal.test(k ~ subtidal_cat, data = data1a)

# deeper vs shallower than 10m  

kdata %>% filter(depth_cat == "y") %>% 
  summarise (median = median(k), n = n())

data1b <- data1 %>% 
  filter(subtidal_cat == "y") 

kruskal.test(k ~ depth_cat, data = data1b) # NO DIFF

# how does k vary with experimental approach #

kdata %>%  group_by(subtidal_cat, fresh_cat) %>% 
  summarise (median = median(k), n = n())

data2 <- kdata %>% 
  filter (subtidal_cat == "y")

kruskal.test(k ~ fresh_cat, data = data2) # fresh material degrades more slowly, p = 2.014e-10 

# how does k vary with modelling approach 

# first explore full data set using the best fitted models, exclude start-end experiments (i.e. only look at time series) 

kdata %>%  group_by(refractory_component_inmodel) %>% 
  summarise (median = median(k), n = n()) # if r is zero or negative can assume there is no sig R 

kdata %>%  filter(refractory_component_inmodel != "no access" & refractory_component_inmodel != "start_end") %>% 
  mutate(refractory = ifelse(refractory_component_inmodel  == "y", "y", "n")) %>% 
  group_by(refractory) %>% 
  summarise (median = median(k), n = n())

data3 <- kdata %>% filter(refractory_component_inmodel != "no access" & refractory_component_inmodel != "start_end") %>% 
  mutate(refractory = ifelse(refractory_component_inmodel  == "y", "y", "n"))

kruskal.test(k ~ refractory, data = data3)

# k is greater in refractory models, although note smaller sample size 

# check on subset of data, where a refractory and non-refractory model could both be fitted to each dataset, i.e. underlying data is indentical
# need to import data from script 3 

modelled_dual <- read_rds(here::here("output","dualmodelled_kdata.rds"))

modelled_dual %>% group_by(refractory) %>% 
  summarise (median = median(k), n = n()) 

kruskal.test(k ~ refractory, data = modelled_dual) # gives consistent results, 

# extra stuff reported in the paper 

# check how many datasets with less than 90 day duration have lost all biomass  (i.e. > 95%) 

kdata %>% mutate(duration_cat = ifelse(duration_days > 90, "y", "n"), proportion_cat  = ifelse(remaining_proportion < 0.05, "y", "n")) %>% 
  group_by(duration_cat, proportion_cat) %>% 
  summarise(n = n ()) 
# 18 datasets that are longer than 90 days have lost almost all (95%) their mass/carbon by the end of the experiment
# whereas 60 have more than 95% remaining at the end of the experiments

# check how many datasets sample at least 4 timepoints 
kdata %>% group_by(atleast_four_timepoints) %>% 
  summarise (median = median(k), n = n())
# 161 measurements (from total of 364) with atleast 4 timepoints 
 