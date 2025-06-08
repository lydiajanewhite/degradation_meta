library(metaDigitise)
library(here)
library(tidyverse)
library(stringr)
library(broom)
library(nlraa)
library(stringi)

source("scripts/1_dataextract_functions.R")

### Collate all published data on macroalgal degradation rates

### 1. Import datasets from tables, downloaded from published articles. All time series data, for which we can fit exponential decay model and approximate k. 

frontier21 <- read_csv(here::here("data_toextract/timeseries","92_frontier21_rawdata.csv"))
frontier21 <- frontier21 %>% 
  group_by(Species, Depth, Days) %>% 
  summarise(percent_remaining = mean(mass)) %>% 
  rename(time_days = Days)  %>% 
  mutate(dataset = "frontier21") %>% 
  unite(name, dataset, Species, Depth, remove = F) 

gomez19 <- read_csv(here::here("data_toextract/timeseries","200_gomez19_data.csv"))
gomez19 <- gomez19 %>%  mutate(percent_remaining = 100 - as.numeric(mean_percent_lost)) %>% 
  mutate(dataset = "gomez19") %>% 
  unite(name, dataset, treatment, remove = F) 

nitsche15 <- read_csv(here::here("data_toextract/timeseries","287_nitsche15_data.csv"))
nitsche15 <- nitsche15 %>%  rename(percent_remaining = median_percent_remaining) %>% 
  mutate(name = "nitsche15") 

braekman19 <- read_csv(here::here("data_toextract/timeseries","165_braekman19.csv"))
braekman19 <- braekman19  %>%  rename(percent_remaining = `percent remaining`) %>% 
  mutate(dataset = "braekman19") %>% 
  drop_na() %>% 
  unite(name, dataset, species, remove = F) 

rossi07 <- read_csv(here::here("data_toextract/timeseries","761_rossi07.csv")) 
rossi07 <- rossi07 %>% mutate(percent_remaining = biomass/max(biomass)*100) %>% 
  mutate(name = "rossi07") 

mylist_tables <- list(braekman19, gomez19, frontier21, rossi07, nitsche15)
common_cols <- Reduce(intersect, lapply(mylist_tables, colnames))
mydf <- purrr::map_df(mylist_tables, `[`, common_cols)

mylist_tables <- mydf %>% group_by(name) %>% 
  group_split()

### 2. Import extracted datasets from figures, downloaded from published articles.  

# load extracted data using metaDigitise, it locates calibrated data stored in "caldat" file to each figure in "figures_toextract" folder
# metaDigitise allows users to extract information from figures directly within the R environment
# you just need to provide the directory path name of the folder with the image to the metaDigise() function 

# Pick, J.L., Nakagawa, S., Noble D.W.A. (2018) Reproducible, flexible and high-throughput data extraction from primary literature: The metaDigitise R package. Biorxiv, https://doi.org/10.1101/247775

# scatter plots (53 files, they are all time series)

rawdata <- metaDigitise(dir = here::here ("figures_toextract"), summary = FALSE)
2
1

scatter <- rawdata[[3]] # third list is a list of scatter plot data 
scatter_df <- scatter %>% bind_rows(.id = "filename")
unique(scatter_df$filename) 

# tidying up
# (1) remove temperature data I extracted, (2) convert % biomass loss to % remaining , (3) convert from biomass to % remaining 
# (4) rename misspelled variable, (5) convert from POC_conc to % remaining 

scatter_timeseries <- scatter_df %>% 
  filter(filename != "conover_temperature.png") %>% # exclude temperature dataset that i extracted 
  filter(filename != "74_yang_temperature.png") %>%  # exclude temperature dataset that i extracted 
  filter(filename != "576_luo_temperature.png") %>%  # exclude temperature dataset that i extracted, now there are 50 files remaining
  mutate(filename = str_replace(filename,".png", "")) %>% 
  mutate(filename = str_replace(filename,".jpg", ""))  %>% 
  unite(name, filename, id) %>% 
  mutate(name = str_replace(name,"_$", "")) %>%  
  mutate(y = case_when(y_variable == "percent_loss" ~ 100 -y, TRUE ~ y), 
         y_variable = case_when(y_variable == "percent_loss" ~ "percent_remaining", TRUE ~ y_variable ) ) %>% 
  mutate(y = case_when(y_variable == "wet_weight" ~ (y/2.5)*100, TRUE ~ y ),  
         y_variable = case_when(y_variable == "wet_weight" ~ "percent_remaining", TRUE ~ y_variable ) ) %>% 
  mutate(y_variable = case_when(y_variable == "s" ~ "percent_remaining", TRUE ~ y_variable ) ) %>% 
  mutate(y = case_when(name == "552_Kristensen94_fucus" ~ (y/1.097855)*100, TRUE ~ y ), 
         y_variable = case_when(name == "552_Kristensen94_fucus"~ "percent_remaining", TRUE ~ y_variable ) ) %>%  
  mutate(y = case_when(name == "552_Kristensen94_ulva" ~ (y/1.418421)*100, TRUE ~ y ), 
         y_variable = case_when(name == "552_Kristensen94_ulva"~ "percent_remaining", TRUE ~ y_variable ) ) %>% 
  rename (percent_remaining = y, time_days = x)


#### boxplots  (3 files, 1 is time series, 2 are start_end)

figuredata <- metaDigitise(dir = here::here ("figures_toextract"))
2
1

figuredata %>% filter(variable != "k") %>% 
  filter(plot_type == "boxplot") %>% 
  group_by(filename, plot_type) %>% summarise(filename_n = n())

boxplot <- figuredata %>% filter(variable != "k") %>% 
  filter(plot_type == "boxplot") %>% 
  mutate(filename = str_replace(filename,".png", "")) %>% 
  mutate(filename = str_replace(filename,".jpg", "")) %>% 
  mutate(mean = case_when(mean < 0 ~ 0, TRUE ~ mean )) %>% # remove negatives caused by data extraction
  rename(percent_remaining = mean) 
  
boxplot_timeseries <- boxplot %>% filter(filename == '54_frontier22_exp1') %>% 
  separate(group_id, c("species", "depth", "time_days"), "_") %>% 
  unite("treatment", c("species", "depth")) %>% 
  mutate(time_days = str_replace(time_days,"d", "")) %>% 
  mutate(time_days = as.numeric(time_days)) %>% 
  unite(name, filename, treatment, remove = F) 

# mean and error (13 files)    

figuredata %>% filter(variable != "k") %>% 
  filter(plot_type == "mean_error") %>% 
  group_by(variable) %>% 
  summarise(variable_n = n())  
  
# filter based on whether they have a dx in their legend 

figuredata %>% filter(str_detect(group_id, "(d\\d*\\.?\\d*$)")) # has a d followed by a number that varies in length that could include a decimal point, and is found at the end of the expression 
figuredata %>% filter(!str_detect(group_id, "(d\\d*\\.?\\d*$)"))

# some tidying to do 
mean_error <- figuredata %>% filter(variable != "k") %>% 
  filter(plot_type == "mean_error") %>% 
  mutate(time_days = stri_extract_last_regex(str = group_id, pattern = "(d\\d*\\.?\\d*$)"),  
    group_id = stri_replace_last_regex(str = group_id, pattern = "(d\\d*\\.?\\d*$)", replacement = "")) %>% 
  mutate(filename = str_replace(filename,".png", "")) %>%  
  mutate(filename = str_replace(filename,".jpg", ""))  %>% 
  unite(name, filename, group_id) %>% 
  mutate(name = str_replace(name,"_$", "")) %>% 
  mutate(time_days = case_when(variable == "percent_remaining_d14" ~ "d14", TRUE ~ time_days), # split variable name to get experiment duration 
    variable = case_when(variable == "percent_remaining_d14" ~ "percent_remaining", TRUE ~ variable)) %>% 
  mutate(mean = case_when(variable == "percent_loss" ~ 100 - mean, TRUE ~ mean),  # convert to % remaining 
    variable = case_when(variable == "percent_loss" ~ "percent_remaining", TRUE ~ variable)) %>% 
  mutate(time_days = as.numeric(str_replace(time_days,"d", ""))) %>% 
  mutate(time_days = case_when(variable == "loss_gWW_perday" ~ 28, TRUE ~ time_days), # convert to % remaining 
         mean = case_when(variable == "loss_gWW_perday" ~ 100 - (((mean*28)/600)*100), TRUE ~ mean),
         variable = case_when(variable == "loss_gWW_perday" ~ "percent_remaining", TRUE ~ variable)) %>% 
  mutate(time_days = case_when(str_detect(name, "lastra") ~ 20, TRUE ~ time_days), # add experiment duration to start_end datasets
         time_days = case_when(str_detect(name, "franzitta") ~ 38, TRUE ~ time_days),
         time_days = case_when(str_detect(name, "mews") ~ 1, TRUE ~ time_days)) %>% 
  rename(percent_remaining = mean) %>% 
  mutate(name = str_replace(name,"proctected", "protected"))
  
mean_error %>% filter(is.na(time_days))  # check we have finished                            

# no day zero data for urbanmalinga08 so we can add it as it will be 100% biomass remaining at T0

a <- mean_error %>% filter (name == "431_urbanmalinga08_highshore" & time_days == 10 ) %>% 
  mutate(percent_remaining = 100, sd = 0, se = 0, time_days = 0)
b <- mean_error %>% filter (name == "431_urbanmalinga08_lowshore" & time_days == 10 ) %>% 
  mutate(percent_remaining = 100, sd = 0, se = 0, time_days = 0)

mean_error <- rbind(mean_error,a,b) 


ts_names <- mean_error %>% group_by(name) %>% 
  summarise(n = n()) %>% 
  filter (n > 2) %>% # more than 2 time points i.e. timeseries 
  select(name) %>% 
  dplyr::pull(name)

mean_error_timeseries <- mean_error %>% filter(name %in% ts_names) 

# we pool all the data extracted from different types of figures, and then add them to the data from tables 

mylist_figures <- bind_rows(scatter_timeseries, boxplot_timeseries, mean_error_timeseries) %>% 
  select (name, time_days, percent_remaining)%>% 
  mutate(percent_remaining = case_when(percent_remaining < 0 ~ 0, TRUE ~ percent_remaining )) %>% 
  mutate(time_days = case_when(time_days < 0 ~ 0, TRUE ~ time_days )) %>% 
  group_by(name) %>% 
  arrange((time_days)) %>% 
  group_split()

mylist <- c(mylist_tables, mylist_figures) 

# Run all the functions from script 1 on the pooled data to fit exponential decay model, with and without refractory component, with differnt starting values, and approximate k. 

fun1(mylist[[1]]) # example 

x1<-map(mylist, possibly(fun1)) %>% bind_rows() # 78 of 105 potential models converge 
x2<-map(mylist, possibly(fun2)) %>% bind_rows() # 56 of 105 potential models converge 
x3<-map(mylist, possibly(fun3)) %>% bind_rows() # 104 of 105 potential models converge 
x4<-map(mylist, possibly(fun4)) %>% bind_rows() # 64 of 105 potential models converge 
x5<-map(mylist, possibly(fun5)) %>% bind_rows() # 95 of 105 potential models converge 
x6<-map(mylist, possibly(fun6)) %>% bind_rows() # 63 of 105potential models converge  

full <- rbind(x1,x2,x3,x4,x5,x6) 
  
# check if models converge one way or another for each data set

names_output <- full %>%
  dplyr::pull(name) %>% 
  unique()

names_list <- mylist %>% 
  bind_rows() %>% 
  dplyr::pull(name) %>% 
  unique()
  
unique(names_list [! names_list  %in% names_output])

# need to check how different the non linear model results are 

full %>%
  group_by(name) %>%summarise(n_model = n()) %>% 
  print (n = 105)


# differences between all refractory models and all non-refractory models are very minor, i.e. <0.001 for almost all parameters (k, b, AIC, Rsquared),
# and normally < 0.01 for b (which is the refractory component in percentage, so 0.01%) although up to 0.0319 for vandendriessche07 (which is only 0.0319%)
# we can thus assume all refractory models are equal, and all non-refractory models models are equal

full %>% 
  group_by(name, refractory) %>% 
  summarise(across(k:normalresids, ~ max(.x)-min(.x)), n_model = n()) %>% 
  filter(if_any(c(k,normalresids,AIC, rsquared,normalresids), ~ . > 0.001) | b > 0.0319 ) 

full %>% filter (name == "74_yang21_low_nutrients")
full %>% filter (name == "820_hanisak99_exp2_gracilaria_winter")
full %>% filter (name == "frontier21_Laminaria hyperborea_0m")
# [1] "74_yang21_low_nutrients"       # r squared is negative       
# [2] "820_hanisak99_exp2_gracilaria_winter" # r squared is negative for non-refractory model and and R is > 100 for refractory model 
# [3] "frontier21_Laminaria hyperborea_0m"  # r squared is negative for non-refractory model and and R is 98 for refractory model 


# Need to override sample sizes for all 443_vandendriessche07 files, with those in the summary data, not the raw extracted data points
summarydata <- metaDigitise(dir = here::here ("figures_toextract"), summary = TRUE)
2
1

overide_samplesize <- summarydata %>% select(filename, n) %>% 
  distinct() %>% 
  mutate(filename = str_replace(filename,".png", "")) %>%  
  mutate(filename = str_replace(filename,".jpg", "")) 
  
overide_samplesize <- setNames(overide_samplesize$n, overide_samplesize$filename )

overide_samplesize["443_vandendriessche07_exp1_asc_grazing_10"]


full %>% 
  mutate(new_n= ifelse(str_detect(name, "^443_"), overide_samplesize[name], n)) %>% 
  select (name, n, new_n) 

full <- full %>% 
  mutate(n = ifelse(str_detect(name, "^443_"), overide_samplesize[name], n)) 


# we compare k values between models with and without refractory component for each data set to determine which is better fitting model 
# we subset the data to include only data sets that had models converge for both (refractory and non-refractory)
# models that meet the following criteria are included 
# 1. degradation constant k can been modelled and is positive (i.e. biomass has not increased during experiment)
# 2. refractory component (when modelled) is positive, a model with a negative refractory component clearly doesn't make sense
# 3. refractory must be less than 95% otherwise decomposition has not started yet 
# 4. models must have positve R squared, a negative R squared or R squared value ~ 0 implies that a horizontal line through the mean would provide a better fit
# If models don't meet these criteria for both a refractory and non-refractory candidate model then we don't consider the data set to have both a good fitting refractory and non-refractory model 
# i.e. it is singular and can only be fitted with one type of model, or is potentially not modellable with single phase exponential decay curve. 

duelmodelfits <- full %>% 
  group_by(name, refractory) %>% 
  slice_head(n = 1) %>% 
  select(-model) %>% 
  pivot_wider(names_from = refractory, values_from = c(k, b, AIC, BIC, rsquared, normalresids)) %>% 
  filter(!is.na(k_yes),  # i.e. there is a k output for the refractory model because it could converge, k_no is always a value, i.e the non refractory model always converges 
        b_yes > 0,  # refractory must be positive
         b_yes < 95, # refractory must be less than 95% otherwise decomposition has not started yet 
         rsquared_no > 0.1,    # negative R squared implies that a horizontal line through the mean would provide a better fit, and below 0.1 is bad fit. 
        rsquared_yes > 0.1)    # negative R squared implies that a horizontal line through the mean would provide a better fit, and below 0.1 is bad fit. 


duelmodelfits_long <- duelmodelfits %>% 
  pivot_longer(cols  = c(-name, -n), names_to = c(".value", "refractory"), 
               names_sep = "\\_") %>% 
  filter(k > 0) #  exclude models when biomass increased

saveRDS(duelmodelfits_long, file = "output/dualmodelled_kdata.rds")

# Select datasets that only have a singular fitting model (or meet the exclusion criteria)

singular <- full %>% 
  group_by(name, refractory) %>% 
  slice_head(n = 1) %>% 
  select(-model) %>% 
  filter(!is.na(k), # i.e. there is a k output for the model
  is.na(b) | (b > 0 & b < 95), # refractory must be positive, less than 95% otherwise decomposition has not yet started, but for non-refractory can be NA
  rsquared > 0.1)

tmp <- singular %>% group_by(name) %>% 
  summarise(n = n()) %>% 
  filter(n == 1) %>% 
  pull(name)

singular <- singular %>% 
  filter(name %in% tmp) 

singular %>% 
  filter(k < 0)#  5 negative ks

singular %>% 
  filter(rsquared < 0.1)

singular <- singular %>% 
  filter(k > 0) 

# need to ascertain the 'best' fit based on AIC 
# highest r squared will always be the one with the extra parameter (i.e. refractory model) so cannot use that as determining factor 
# if AIC in non refractory model is > 2 than refractory model AIC, then non refractory is better
# thus if AIC in refractory is > 2 than non-refractory model AIC, then refractory is best 

# check if AIC and BIC give the same results. 

AIC <- duelmodelfits_long %>% 
  group_by(name) %>% 
  filter (AIC == min(AIC))

BIC <- duelmodelfits_long %>% 
  group_by(name) %>% 
  filter (BIC == min(BIC))

all.equal(AIC, BIC) # BIC and AIC give identical results apart from 443_vandendriessche07_exp2_asc_meddens
# however difference in AIC is less than 2 so for this data set so doesn't effect results 

# subset data to select model with AIC difference greater than 2  
not_close <- duelmodelfits_long %>%  # 27 files
  group_by(name) %>% 
  filter((max(AIC) - min(AIC)) > 2) %>% 
  group_by(name) %>% 
  filter(AIC == min(AIC)) 

# for all cases the refractory models performs better .... 
# Inspecting plots always shows evidence of refractory material OR atleast extreme slowing down of matter loss compared to inital phase 

# "443_vandendriessche07_exp2_asc_highdens"          
# "552_Kristensen94_fucus"                     
# "552_Kristensen94_ulva"                            
# "653_liu19"                 
# "74_yang21_high_nutrients"                         
# "780_vichkovitten04"                             
# "789_paalme07_exp1_spring_pilayella"               
# "789_paalme07_exp2_cladophera_anaerobic"           
# "789_paalme07_exp2_pilayella_anaerobic"             
# "78_ischi20_ulva"                                  
# "78_ischi20_undaria"                                
# "braekman19_Palmaria"                              
# "buchsbaum91_fucus"                                
# "chown96_exposedseals"                              
# "craffoldandschulz87_proctectedseals"              
# "dufour12_autumn"                                  
# "dufour12_winter"                                   
# "frontier21_Laminaria hyperborea_15m"              
# "gomez19_fauna excluded"                           
# "gomez19_fauna present"                             
# "hunter76_sediment"                                
# "riceandtenore81_gracilaria"                       
# "riceandtenore81_hypnea"                           
# "riceandtenore81_sargassum"                        
# "riceandtenore81_spatoglossum"                     
# "williams84_anoxicsediment"                        
# "williams84_watercolumn_winter"  

# subset data to select models with AIC difference less than 2, i.e. both types of models perform similarly well 

close <- duelmodelfits_long %>% # 10 files 
  group_by(name) %>% 
  filter((max(AIC) - min(AIC)) < 2) %>% 
  group_by(name) %>% 
  filter(refractory == "no") 

# 11 cases where the refractory model does not perform any better, thus we should choose the simplest i.e. non-refractory model 
# 24_perkins22, no evidence of R in plot, so take the NR
# 443_vandendriessche07_exp1_asc_grazing_18, n = 8, uses raw data not means, because of repeated measurements of same individuals, high variability between just 3 individuals, no evidence of R, so take the NR 
# 443_vandendriessche07_exp2_asc_lowdens, n = 62, uses raw data not means, because of repeated measurements of same individuals, high variability between just 3 individuals, only evidence of R in one individual, so take the NR 
# 479_salovius_04_exp1_8m_sedimentsurface n =  4, no evidence of R in plot, so take the NR
# 479_salovius_04_exp2_pilayella_gammarus_included n =  6, no evidence of R in plot, so take the NR 
# 54_frontier22_exp1_LH_depth3, no evidence of R in plot, so take the NR 
# 689_gladstone-gallagher_16, n = 4, no evidence of R in plot, so take the NR
# 789_paalme07_exp2_cladophera_aerobic -  n = 11 no evidence of R in plot, so take the NR 
# 820_hanisak99_exp2_gracilaria_spring -  n = 5,  not better, no evidence of R in plot, so take the NR 
# williams84_oxicsediment - n = 5, there is limited evidence of R in plot, but sample size is small and figure quality bad, take the NR, shows limitations of small sample size and relying on model results 

# compile a df of the best fitting model for each dataset (i.e. 92 datasets, although we started with 105 datasets)
best_model <- rbind(close, not_close, singular) 

unique(names_output  [! names_output  %in% best_model$name]) 
# 3 datasets couldnt be modelled, due to negative r squared 
# 3 modelled datasets had really low squared values (less than 0.1) and inspecting plots they were not a good fit 
# 7 modelled datasets had negative k values 
# [1] "443_vandendriessche07_exp1_asc_grazing_5"   # negative k values 
# [2] "443_vandendriessche07_exp1_asc_nograzing_5"  # negative k values 
# [3] "443_vandendriessche07_exp1_fuc_grazing_5"   # negative k values 
# [4] "443_vandendriessche07_exp1_fuc_nograzing_18" # rsquared < 0.1
# [5] "443_vandendriessche07_exp1_fuc_nograzing_5"  # negative k values 
# [6] "443_vandendriessche07_exp2_asc_meddens"     # rsquared < 0.1
# [7] "54_frontier22_exp1_LH_depth0.5"              # negative k values 
# [8] "54_frontier22_exp1_LH_depth1.5"             # negative k values 
# [9] "54_frontier22_exp1_LO_depth0.5"  # # rsquared < 0.1 
# [10] "74_yang21_low_nutrients"             # negative r squared, increased biomass
# [13] "789_paalme07_exp1_spring_cladophera"  # negative k values 
# [12] "820_hanisak99_exp2_gracilaria_winter" # negative r squared, biomass stayed the same 
# [13] "frontier21_Laminaria hyperborea_0m"  # negative r squared, increased biomass
#

best_model %>%  filter (refractory == "yes") %>% # refractory models always have atlease 5 timepoints (start plus 4)
  summary(min = min(n))

# check how many studies in total that wree modelled
# naming is a bit annoying so have to count separately when a paper has multiple experiments

sort(best_model$name)
  
best_model %>% 
  mutate(name = str_replace(name,"salovius_04", "salovius04")) %>% 
  filter(grepl('exp[12]',  `name`)) %>% 
  separate(name, c("study", "author", "experiment", NA)) %>% 
  unite(study_experiment, c("study", "author", "experiment"), remove = F) %>% 
  group_by(study_experiment) %>% 
  summarise(study_experiment = unique(study_experiment)) # 10 unique study experiment combinations, frontier and duggins only had 1 of their experiments as a modellable time series 

best_model %>% 
  mutate(name = str_replace(name,"salovius_04", "salovius04")) %>%
  filter(!grepl('exp[12]',  `name`)) %>% 
  separate(name, c("study", "author", "experiment", NA)) %>% 
  unite(study_experiment, c("study", "author", "experiment"), remove = F) %>% 
  group_by(study) %>% 
  summarise(study = unique(study)) %>% 
  print (n = 23) # 23 single experiment studies

# 23 single experiment studies + 6 double experiment studies (but only 4 of these had both experiments modelled) 
# 29 studies, and 33 study/experiment combinations 


### 3. Some k values are extracted directly from figures (3 files)
figuredata_k <- figuredata %>% 
  filter(variable == "k") %>% 
  mutate(filename = str_replace(filename,"kristensen_03", "kristensen03"))

### 4.start_end data cannot be modelled with single phase exponential decay curve. 

boxplot_start_end <- boxplot %>% filter(filename != '54_frontier22_exp1') %>% 
  rename(treatment = group_id) %>% 
  unite(name, filename, treatment, remove = F) %>% 
  mutate(time_days = case_when(filename == "54_frontier22_exp2" ~ 14, TRUE ~ 112 )) %>% 
  select(name, percent_remaining, time_days) %>% 
  mutate(percent_dailyloss = (100-percent_remaining)/time_days)
  
se_names <- mean_error %>% group_by(name) %>% 
  summarise(n = n()) %>% 
  filter (n < 2) %>% # less than 2 time points i.e. start and end
  select(name) %>% 
  dplyr::pull(name)

mean_error_start_end <- mean_error %>% filter(name %in% se_names)

mean_error_start_end <- mean_error_start_end %>% 
  mutate(percent_remaining = case_when(percent_remaining< 0 ~ 0, TRUE ~ percent_remaining )) %>% 
  mutate(percent_dailyloss = (100-percent_remaining)/time_days)

quieros23 <- read_csv(here::here("data_toextract/start_end","1_quieros23_rawdata.csv")) 

table_start_end <- quieros23 %>% 
  select(Species, `Experimental Day`, biomass_FW) %>% 
  pivot_wider(names_from = `Experimental Day`, values_from = biomass_FW) %>% 
  mutate(percent_remaining = (`35`/`0`)*100, 
         percent_dailyloss = (100-percent_remaining)/35,
         time_days = 35,
         file = "quieros23") %>% 
  unite(name, file, Species)

# some treatments have zero mass of macroalgae left at the end of experiments, so these experiments have to be standardized by adding 1% 

# 600_smale21 and mews06 

start_end <- bind_rows(boxplot_start_end, mean_error_start_end, table_start_end) %>% 
  select(name, percent_remaining, time_days, percent_dailyloss) %>% 
  mutate (proportion_remaining = percent_remaining/100, 
          k_start_end = (log(proportion_remaining)/time_days) * -1,
          k_start_end_std = (log(proportion_remaining + 0.01)/time_days) * -1) %>% 
  mutate (k_start_end = case_when(grepl("smale", name) ~ k_start_end_std,
                                  grepl("mews", name) ~ k_start_end_std,
          TRUE ~ k_start_end)) %>% 
  filter(!grepl('phyllospadix',  `name`)) %>% 
  filter(k_start_end > 0) # 9 k values were negative so excluded i.e biomass increased 

allmodelledextracteddata <- list (
  start_end = start_end,
  time_series = best_model,
  k_original = figuredata_k
)  


saveRDS(allmodelledextracteddata, "output/all_modelled_extracted_data.RDS")

# we have our final k database, which is next added to full database that already contains the metadata, and any k values extracted directly from main text or tables.
