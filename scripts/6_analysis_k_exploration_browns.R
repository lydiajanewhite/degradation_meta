library(tidyverse)
library(here)
library(stringr)
library(worrms)

### Explore how k of brown forest forming macroalgae varies and sensitivity of carbon export models to k 

### 1. Import data

prelim <- read_csv(here::here("data","database.csv"))  # corresponds to "database" sheet from published excel file 

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


### 2. Implications for modelling carbon export 

# Export.S = e−CRT×k × 0.71.  for sinking seaweeds, from: Filbee-Dexter, K., Pessarrodona, A., Pedersen, M.F. et al. Carbon export from seaweed forests to deep ocean sinks. Nat. Geosci. 17, 552–559 (2024). https://doi.org/10.1038/s41561-024-01449-7
# Where 0.71 is the proportion of NPP that entered the water column as detritus 
# CRT is the coastal residence time (median CRT across all EEZs reported in main text of paper = 75 days) and k is the decomposition rate.  
# note. median CRT reported in supplementary data is reported as 642 days, main text reports 75 days which i have used in my calcs, assume difference is driven by a couple of EEZs with extremely high CRT and large areas driving up median CRT reported across all cells of water

# Authors highlight that the model is very sensitive to deviations in k,
# "Upper and lower bounds were based on 25% and 75% quartiles for decomposition rates, which lead to 14 to 124 TgC y-1 of POC export
# To capture the range of decomposition rates for brown seaweed detritus, they used a weighted quantile estimate of k values for ten quantiles in the global dataset of decomposition. 
# Selected ten quantiles to exclude the upper and lower 5% of these decomposition rates to ensure calculations were not influenced by outlier data."

# We trim our data set to include brown canopy forming algae only i.e. not filamentous, gives 24 species 

kdata <- kdata %>%  
  filter (colour =="brown" & species != "pylaiella littoralis"  ) %>% 
  mutate(depth_cat = ifelse(depth_m > 10, "y", "n"), 
         duration_cat = ifelse(duration_days > 90, "y", "n"),
         subtidal_cat = ifelse(depth_m > 0, "y", "n"),
         subtidal_cat = case_when(study_exp == "Franzitta et al 2015_1" ~ "y", TRUE  ~ subtidal_cat),  ### 	Franzitta et al 2015_1 is shallow subtidal but doesnt specify depth. 
         fresh_cat = case_match(fresh, "y" ~ "fresh", .default = "not fresh"))

kdata %>%  group_by(species) %>%
  summarise (n = n()) 


n_FD <- 124 # sample size of degreadtion rates 
CRT <- 75 # median coastal residence time across EEZs from paper main text
k_FD <- 0.02 # median k in paper

export_FD <-(exp(-CRT*k_FD))*0.71
export_FD_reported <- 0.148


# subtidal vs intertidal 

kdata %>% 
  group_by(set_up, subtidal_cat) %>% 
  summarise (median = median(k), n = n())

k_sub <- 0.00964   
k_int <- 0.0665 
n_sub <- 106
n_int <- 76

export_sub <- (exp(-CRT*k_sub))*0.71
export_int <-(exp(-CRT*k_int))*0.71 # smaller export potential due to faster degradation 

export_sub/export_int # 71 times bigger 

kdata %>%  group_by(subtidal_cat, fresh_cat) %>% 
  summarise (median = median(k), n = n()) 

k_fresh <- 0.00820  
k_nfresh <- 0.0822
n_fresh <- 90
n_nfresh <- 16

export_fresh <- (exp(-CRT*k_fresh))*0.71
export_nfresh <-(exp(-CRT*k_nfresh))*0.71 # smaller export potential due to faster degradation 
export_fresh/export_nfresh # 257

# refractory 

kdata %>%  group_by(refractory_component_inmodel) %>% 
  summarise (median = median(k), n = n()) # if r is zero or negative can assume there is no sig R 

kdata %>%  filter(refractory_component_inmodel != "no access" & refractory_component_inmodel != "start_end") %>% 
  mutate(refractory = ifelse(refractory_component_inmodel  == "y", "y", "n")) %>% 
  group_by(refractory) %>% 
  summarise (median = median(k), n = n())

k_R <- 0.0423  
k_NR <- 0.022 
n_R <- 26   
n_NR <- 131

export_R <- (exp(-CRT*k_R))*0.71 # smaller export potential due to faster degradation 
export_NR <-(exp(-CRT*k_NR))*0.71 
export_NR/export_R # 5 

test <- data.frame(export = c(export_FD_reported, export_int, export_sub, export_fresh, export_nfresh, export_R, export_NR), 
                   sample_size = c(n_FD, n_int, n_sub, n_fresh, n_nfresh, n_R, n_NR),
                   category = c("Filbee-Dexter et al 24","intertidal/supratidal", "subtidal",  "fresh", "treated","studies with refractory", "studies without refractory")) %>% 
  mutate(category = fct_relevel(category, 
                                "Filbee-Dexter et al 24","intertidal/supratidal", "subtidal",  "treated", "fresh", "studies with refractory", "studies without refractory"))


ggplot(test, aes(x=export, y = category )) +  
  geom_col(width = 0.1, fill = "grey", position = position_dodge()) +
  geom_point(aes(fill= "black"), shape = 21, size = 3) +
  geom_text(aes(label = str_c("n = ", sample_size)), hjust = 0, nudge_x = .010) +
  theme_classic() +
  theme(legend.position="none") +
  scale_y_discrete(limits = rev(levels(test$category)))



