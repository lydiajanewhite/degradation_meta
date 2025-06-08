library(tidyverse)
library(here)
library(stringr)
library(worrms)

### Data synthesis at the study and experiment level summmarising experiment duration, depth, sampling frequency, habitat etc

### 1. Import data

prelim <- read_csv(here::here("data","database.csv"))  

prelim <- prelim |> 
  mutate(
    across(where(is.character), ~ str_replace_all(.x, "\\s", " ")),
    across(where(is.character), ~ str_remove(.x, " $")), #trimming end space
    depth_m = as.numeric(depth_m),
    duration_days = as.numeric(duration_days), 
    size_g = as.numeric(size_g),
    `k (d-1)` = as.numeric(`k (d-1)`)
  ) |> rename(k = `k (d-1)`, four_timepoints = atleast_four_timepoints) 


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

### summarise number of studies, experiments, species etc 

sp <- c(str_sort(unique(prelim$species)))  # 58 unique species used in degradation experiments 

unique(prelim$study) # 71 peer-reviewed studies 

prelim %>% 
  mutate (ref_category = case_when(record =="not there" ~ "extra" , record != "not there" ~ "db")) %>% 
  group_by(ref_category, study) %>% 
  summarise (n = n()) %>% 
  group_by(ref_category) %>% 
  summarise (n = n()) # 46 studies came from WOS search and 25 studies came from reviews and prior knowledge, rather than WOS search 

prelim <- prelim %>% unite("study_exp", "study", "experiment", remove = F) 

unique(prelim$study_exp) # 85 unique experiments, some studies have multiple experiments within single publications 


### 2. Convert continuous variables into categories for Figure 3 and for additional summary information

prelim %>% 
  group_by(study_exp) %>% 
  reframe(max = max(duration_days), min = min(duration_days), diff = max - min) %>%
  filter (diff > 1) # 14 experiments had different duration for different treatments e.g. due to complete degradation before end or unforeseen circumstances 

prelim %>% 
  group_by(study_exp) %>% 
  reframe(max = max(depth_m), min = min(depth_m), diff = max - min) %>%
  filter (diff > 1)  # 4 experiments included a depth gradient, Smale et al 2021_1 had some variability between sites but experiment was not designed to test for effect of depth on degradation

prelim %>% 
  group_by(study_exp) %>% 
  reframe(mtp = unique(multiple_timepoints)) %>%
  group_by(study_exp) %>%
  summarise (n = n())  %>% 
  filter (n > 1)  # 2 experiments sampled at multiple time points for some treatments but not all e.g. due to lost bags or unforeseen circumstances 

prelim %>% 
  filter(study_exp =="Filbee-dexter et al 2022_1") %>% # more than half the datasets sampled at multiple time points, only 7 of 53 were start end
  group_by(multiple_timepoints) %>% 
  summarise (n = n()) 

prelim %>% 
  filter(study_exp =="Pedersen et al 2021_2") %>% # deep site only had one sampling event due to issues with being in shipping lane
  group_by(multiple_timepoints) %>% 
  summarise (n = n())

# Summarise data at the experiment level based on categorical criteria 

x <- prelim %>%  mutate (multiple_timepoints = case_when(study_exp == "Pedersen et al 2021_2" ~ "n", # only half treatments sampled multiple times so experiment doesn't satisfy multiple_timepoints category 
                                                         study_exp == "Filbee-dexter et al 2022_1" ~ "y",  # more than half treatments sampled multiple times so does satisfy multiple_timepoints category 
                                                         TRUE ~ multiple_timepoints), 
                        four_timepoints = case_when(study_exp == "Buchsbaum et al 1991_1" ~ "both", 
                                                    study_exp == "Catenazzi et al 2007_1" ~ "both",
                                                    study_exp == "Salovius & Bonsdorff 2004_1" ~ "both",
                                                    study_exp == "Paalme et al 2002_1" ~ "both",
                                                    study_exp == "Smith and Foreman 1984_1" ~ "both",
                                                    study_exp == "Vandendriessche et al 2007_1" ~ "both",
                                                    TRUE ~ four_timepoints), # create both category for four_timepoints 
                        subtidal_cat = ifelse(depth_m > 0, "y", "n"),
                        subtidal_cat = case_when(study_exp == "Franzitta et al 2015_1" ~ "y", # Franzitta et al 2015_1 is shallow subtidal but doesn't specify depth. 
                                                 study_exp == "Josselyn & Mathieson 1980_1" ~ "both", # create both category for subtidal (study compared intertidal vs subtidal)
                                                 study_exp == "Yang et al 2021_1" ~ "floating", # in situ floating mesocosm
                                                  TRUE  ~ subtidal_cat),  
                        fresh_cat = case_match(fresh, "y" ~ "fresh", .default = "not fresh"),
                        fresh_cat = case_when(study_exp == "Brouwer 1996_1" ~ "both", 
                                              study_exp == "Lastra  et al 2018_1" ~ "both",
                                              study_exp == "Luo et al 2022_1" ~ "both",
                                              TRUE ~ fresh_cat)) %>%  # create both category for fresh tissue category 
  group_by(study_exp) %>% 
  reframe(duration_days = max(duration_days), # duration often limited by complete degradation of biomass which is outside of authors control, so we take the maximum duration of any treatment as the experiment duration
         depth_m = max(depth_m), # any data collected at depth should be commended, so we take the maximum depth of any treatment in experiment as the max depth
         multiple_timepoints = unique(multiple_timepoints), 
         multiple_depths = unique(multiple_depths), 
         four_timepoints = unique(four_timepoints),  # 4 time points really crucial to identify potential presence of refractory material refractory 
         set_up = unique(set_up), 
         n = n(),
         subtidal_cat= unique(subtidal_cat), 
         fresh_cat = unique(fresh_cat)) %>% 
  mutate(depth_cat = ifelse(depth_m > 10, "y", "n"), 
         duration_cat = ifelse(duration_days > 90, "y", "n"))
  

x %>% group_by(set_up) %>% 
  count() # aquaria 33, in situ 52 

x %>% group_by(set_up, subtidal_cat) %>% 
  count() # subtidal = 26 (one is floating so algae is submerged , one is both subtidal and intertidal, ie experiment contrasts the two habitats), intertidal or supratidal = 27 

x %>% filter(set_up == "in situ" & depth_m > 0) %>% 
  summarise(median_depth = median(depth_m)) # median depth = 4.25

x %>% group_by(fresh_cat) %>% 
  count() # 38 not fresh, 50 fresh, 3 experiments explicitly contrast freshness variable 

x %>% group_by(multiple_timepoints) %>% 
  count()  # 62 experiments with multiple timepoints, 21 start and end 

### 3. Make plot 

test <- x %>% 
  select (study_exp, set_up, multiple_depths, depth_cat, duration_cat, multiple_timepoints )

# summary information about aquaria-based experiments 

test %>%  filter (set_up == "aquaria") %>% 
  group_by(duration_cat) %>% 
  summarise (n = n())

test %>%  filter (set_up == "aquaria") %>% 
  group_by(multiple_timepoints) %>% 
  summarise (n = n())

### summary info about in situ experiments 

in_situ <- test %>%  filter (set_up == "in situ")

filter (in_situ, duration_cat == "y") # 9
filter (in_situ, depth_cat == "y") # 6
filter (in_situ, multiple_timepoints =="y") # 39
filter (in_situ, multiple_depths == "y") # 4

filter (in_situ, duration_cat == "y" & depth_cat == "y") # 4
filter (in_situ, duration_cat == "y" & multiple_timepoints =="y") # 6
filter (in_situ, duration_cat == "y" & multiple_depths == "y") # 2

filter (in_situ, depth_cat == "y" & multiple_timepoints =="y") # 4
filter (in_situ, depth_cat == "y" & multiple_depths == "y") # 2
filter (in_situ, multiple_timepoints =="y" & multiple_depths == "y") # 3

filter (in_situ, duration_cat == "y" & depth_cat == "y" & multiple_timepoints =="y") # 3
filter (in_situ, duration_cat == "y" & depth_cat == "y" & multiple_depths == "y") # 2
filter (in_situ, duration_cat == "y" & multiple_timepoints =="y" & multiple_depths == "y") # 1
filter (in_situ, depth_cat == "y" & multiple_timepoints =="y" & multiple_depths == "y") # 1

filter (in_situ, duration_cat == "y" & depth_cat == "y" & multiple_timepoints =="y" & multiple_depths == "y") # 1

library(UpSetR)

# Figure 3. Distribution of in situ macroalgal degradation experiment types, defined by all possible combinations of four criteria: (a) over multiple depths, (b) deeper than 10 metres, (c) duration longer than 90 days, or (d) sampled multiple times (i.e. not just beginning and end). 

expressionInput <- c( long = 9, deep = 6, timeseries = 39, depthseries = 4, 
                      `long&deep` = 4, `long&timeseries` = 6, `long&depthseries` = 2,
                      `deep&timeseries` = 4, `deep&depthseries` = 2, `timeseries&depthseries` = 3,
                      `long&deep&timeseries` = 3,  `long&deep&depthseries` = 2, `long&timeseries&depthseries` = 1, `deep&timeseries&depthseries` = 1, 
                      `long&deep&timeseries&depthseries` = 1)

upset(fromExpression(expressionInput), 
      order.by = "freq")
