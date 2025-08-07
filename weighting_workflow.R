#### copyright ----
## Copyright 2025 Province of British Columbia
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
## http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software distributed under the License 
## is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and limitations under the License.


#### about/notes ----
## To create the weightings, 
##   weights type: 1. design weights, 2. coverage and non-response adjusted weights,
##                 3. final weights,  4. calibrated/raked/trimmed weights
##   values needed to calculate:
##     1. population count in BC, 
##     2. chsa (use chsa_pre_2023 variable), 
##     3. gender group, age group


#### load packages and functions ----
library(tidyverse)
library(lubridate)  ## used for ymd()
library(survey)     ## used in custom function `get_rake_weight_ind()` and for trimWeights(), svydesign()
library(janitor)    ## used for clean_names()

source("functions.R")


#### SET VALUES ----

## path to secure data folder and files
FOLDER_data <- file.path("path_to_data_folder")

## most recent BC Demographic data
PATH_data   <- file.path(FOLDER_data, "name_of_bc_demographic_survey_data.csv")

## where you wish to save results
SAVE_path <- file.path("path_to_save_folder")


#### 1. load the data ----
### * 1.0 load bc demographic survey data ----
data <- read_csv(PATH_data) %>%
  ## ensure all variables are in lower-case
  rename_with(~tolower(.)) %>%
  ## create 'studyid' because that is name of id variable used in get_xxx functions
  mutate(studyid = as.character(id))


### * 1.1 random sample respondents ----
### * 1.11 subset sampled respondents
demo <- data %>%
  ## filter for only SAMPLED respondents (i.e., exclude self-select respondents)
  filter(upld_survey_type == 2) %>%
  ## studyid: unique id variable
  ## upld_survey_type: 1=website, 2=mailed invitation
  ## survey_mode: 1=telephone, 2=internet, 3=paper
  ## q40agen: 1=female, 2=intersex/indeterminate, 3=male, 99=prefer not to answer/unsure
  ## gen: 1=man, 2=woman
  ## chsa, chsa_pre_2023, cdcsd: community health service areas and census division/census subdivision
  ## hh_id: household ID
  ## aq2hh_size_calc: household size
  ## adob: date of birth, YYYYMMDD
  select(studyid, upld_survey_type, survey_mode, q40agen, gen, chsa, 
         chsa_pre_2023, hh_id, aq2hh_size_calc, cdcsd, adob)
dim(demo)[1]  ## tens of thousands fewer than `data` as filtering drops this down


### * 1.12 get and add in gender and age categories (end up smaller than `demo`)
demo_gender <- get_gender_categories(demo, gender_binary = T)

demo_age    <- get_age(demo, age_from = lubridate::ymd('2023-12-31')) ## warning: 14 failed to parse


### * 1.13 join categories into demo
demo_gender_age <- demo %>% 
  left_join(demo_gender %>%
              select(studyid, binary_response_combined_description, sex_response_combined_description),
            by = "studyid") %>% 
  left_join(demo_age %>%
              select(studyid, age),
            by = "studyid")


### * 1.14 some data wrangling
demo_gender_age <- demo_gender_age %>%
  mutate(participant_cnt = 1,
         gender = case_when(binary_response_combined_description == "Man/Boy" ~ "Men+",
                            binary_response_combined_description == "Woman/Girl" ~ "Women+",
                            T ~ binary_response_combined_description)) %>%
  ## following the age groups in census, create an age group variable in demo survey data
  mutate(age_group = cut(age, 
                         breaks = c(0, 15, 65, Inf),
                         labels = c("0 to 14 years", "15 to 64 years", "65 years and over"),
                         right = F)) %>%
  ## aq2hh_size_calc being 0 (which it sometimes is)
  mutate(across(.cols =  c(aq2hh_size_calc), .fns = ~ pmax(1, as.numeric(.x)))) %>%
  mutate(across(.cols =  c(cdcsd, chsa, gender), .fns = as_factor))


## review data
dim(demo_gender_age)[1]  ## same size as `demo`, good
summary(demo_gender_age)
## missing data (NA's) in q40agen, gen and gender (gender vars); adob (birthdate), age and age_group;
##                        chsa, chsa_pre_2023 and cdcsd vars; other vars


### * 1.15 imputation as some variables have missing data
## PATIENCE: this takes @10 minutes, as it goes through multiple iterations
imputed_demo_gender_age <- impute_vars(demo_gender_age,
                                       to_impute = c("cdcsd", "chsa_pre_2023",
                                                     "aq2hh_size_calc", "gender", "age"),
                                       method = "rf")


### * 1.16 more data wrangling, mostly hh_ids

## update age_group for imputed cases
imputed_demo_gender_age <- imputed_demo_gender_age %>%
  mutate(age_group = cut(age, breaks = c(0, 15, 65, Inf),
                         labels = c("0 to 14 years", "15 to 64 years", "65 years and over"),
                         right = F))
summary(imputed_demo_gender_age$age_group)
summary(imputed_demo_gender_age$gender)
summary(imputed_demo_gender_age$chsa_pre_2023)
summary(as.factor(imputed_demo_gender_age$hh_id))

## chsa - chsa_pre_2023 crosswalk (from non-imputed data)
chsa_crosswalk <- data %>% group_by(chsa, chsa_pre_2023) %>% tally() %>% filter(!is.na(chsa_pre_2023))

## deal with hh_ids that cross multiple chsa_pre_2023s (i.e., place them in single chsa_pre_2023))
## get all listed chsa_pre_2023s for hh_ids with more than one chsa_pre_2023
dup_hh_ids <- imputed_demo_gender_age %>%
  group_by(hh_id) %>% mutate(n_chsapre2023 = n_distinct(chsa_pre_2023)) %>% filter(n_chsapre2023 > 1) %>%
  pull(hh_id) %>% unique()

## get all listed chsa_pre_2023s for hh_ids with more than one chsa_pre_2023
dup_chsa_hhid <- imputed_demo_gender_age %>% filter(hh_id %in% dup_hh_ids) %>% select(chsa_pre_2023, hh_id)

## if hh_id has postal code, find other chsa_pre_2023 in data for that postal code;
## else check by city if not NA
chsa_by_addr <- tibble(chsa = as.double(), chsa_pre_2023 = as.double(), n = as.integer(),
                       hh_id = as.character(), note = as.character())
for(i in seq_along(dup_hh_ids)) {
  message(paste0("*** hh_id: ", dup_hh_ids[i]))
  addr_pc <- data %>% filter(hh_id == dup_hh_ids[i]) %>% pull(addr_mpostalcode) %>% unique()
  if(!is.na(addr_pc)) {
    ## find chsa and chsa_pre_2023 for that postal code (or FSA), if not NA
    tmp <- data %>% filter(str_detect(addr_mpostalcode, addr_pc)) %>% group_by(chsa, chsa_pre_2023) %>% tally()
    while(nrow(tmp) < 2 & nchar(addr_pc) >= 3) {
      addr_pc <- str_sub(addr_pc, end = -2)
      tmp <- data %>% filter(str_detect(addr_mpostalcode, addr_pc)) %>% group_by(chsa, chsa_pre_2023) %>% tally()
    }
    tmp <- tmp %>% filter(!is.na(chsa_pre_2023)) %>% mutate(hh_id = dup_hh_ids[i], note = NA) %>% ungroup()
    ## if more than one chsa, note largest one
    if(nrow(tmp) > 1) {
      tmp2 <- tmp %>% filter(max(n) == n) %>% pull(chsa_pre_2023)
      tmp <- tmp %>% mutate(note = case_when(chsa_pre_2023 %in% tmp2 ~ "largest chsa_pre_2023", TRUE ~ note))
      rm(tmp2)
    } else { tmp <- tmp %>% mutate(note = "postal code-based match") }
    chsa_by_addr <- bind_rows(chsa_by_addr, tmp)
    rm(tmp)
  } else { 
    ## if postal code is NA, look at city
    addr_city <- data %>% filter(hh_id == dup_hh_ids[i]) %>% pull(addr_mcity) %>% unique()
    if(!is.na(addr_city)) {
      ## find chsa and chsa_pre_2023 for that city, if not NA
      tmp <- data %>% filter(addr_mcity == addr_city) %>% group_by(chsa, chsa_pre_2023) %>% tally() %>%
        filter(!is.na(chsa_pre_2023)) %>% mutate(hh_id = dup_hh_ids[i], note = NA)
      ## if more than one chsa, note any that match any of hh_id's listed chsas
      if(nrow(tmp) > 1) {
        tmp2 <- intersect(tmp$chsa_pre_2023, dup_chsa_hhid %>% filter(hh_id == dup_hh_ids[i]) %>% pull(chsa_pre_2023))
        if(length(tmp2) > 0) {
          tmp <- tmp %>% mutate(note = case_when(chsa_pre_2023 %in% tmp2 ~ "city-based match; no pc", TRUE ~ note))
        } else {
          tmp <- dup_chsa_hhid %>% filter(hh_id == dup_hh_ids[i]) %>%
            left_join(chsa_crosswalk, by = "chsa_pre_2023") %>%
            filter(n == max(n)) %>% select(chsa, chsa_pre_2023, n, hh_id) %>%
            mutate(note = "largest chsa x chsa_pre_2023; no pc, no city match")
        }
        rm(tmp2)
      }
    } else {
      ## if postal code and city are NA, simply pull in largest chsa
      tmp <- dup_chsa_hhid %>% filter(hh_id == dup_hh_ids[i]) %>%
        # mutate(chsa = as.numeric(as.character(chsa))) %>%
        left_join(chsa_crosswalk, by = "chsa_pre_2023") %>%
        filter(n == max(n)) %>% select(chsa, chsa_pre_2023, n, hh_id) %>%
        mutate(note = "largest chsa x chsa_pre_2023; no pc or city")
    }
    chsa_by_addr <- bind_rows(chsa_by_addr, tmp)
    rm(addr_city, tmp)
  }
}; rm(i, addr_pc)

## save file
write_csv(chsa_by_addr,
          file = file.path(SAVE_path, "Weighting",
                           paste0("chsa_by_address_", Sys.Date(), ".csv")))

tmp <- imputed_demo_gender_age %>%
  filter(hh_id %in% dup_hh_ids) %>%
  select(studyid, hh_id, chsa_pre_2023_orig = chsa_pre_2023) %>%
  left_join(chsa_by_addr %>% filter(!is.na(note)) %>% select(hh_id, chsa_pre_2023), by = "hh_id") %>%
  mutate(impute_chsa_pre_2023 = case_when(!is.na(chsa_pre_2023) ~ chsa_pre_2023, TRUE ~ chsa_pre_2023_orig))
## finally set to single chsa_per_2023 within hh_id
imputed_demo_gender_age <- imputed_demo_gender_age %>%
  left_join(tmp %>% select(studyid, impute_chsa_pre_2023), by = "studyid") %>%
  mutate(chsa_pre_2023_orig = chsa_pre_2023,
         chsa_pre_2023 = case_when(!is.na(impute_chsa_pre_2023) ~ impute_chsa_pre_2023,
                                   TRUE ~ chsa_pre_2023))

names(imputed_demo_gender_age)

rm(tmp, chsa_by_addr, chsa_crosswalk, dup_chsa_hhid)


## save file
##   filtered data with age, gender and chsa_pre_2023 categories
##   with missing values imputed in cdcsd, chsa_pre_2023, aq2hh_size_calc, gender and age group
##   with arbitrary values for NA hh_ids, and re-set chsa_pre_2023 for the few hh_ids with 2+ chsa_pre-2023
write_csv(imputed_demo_gender_age,
          file = file.path(SAVE_path, "Weighting",
                           paste0("1_imputed_gender_age_geog_", Sys.Date(), ".csv")))


### * 1.2 self-selected respondents ----
### * 1.21 subset self-selected respondents
demo_self <- data %>%
  ## filter for only NON-SAMPLED respondents (i.e., include self-select respondents)
  filter(upld_survey_type == 1) %>%
  select(studyid, upld_survey_type, survey_mode, q40agen, gen, chsa, 
         chsa_pre_2023, hh_id, aq2hh_size_calc, cdcsd, adob)

### * 1.22 get and add in gender and age categories (end up smaller than `demo_self`)
demo_self_gender <- get_gender_categories(demo_self, gender_binary = T)
demo_self_age    <- get_age(demo_self, age_from = lubridate::ymd('2023-12-31'))  ## warning: 2 failed to parse


### * 1.23 join categories into demo_self
demo_self_gender_age <- demo_self %>% 
  left_join(demo_self_gender %>%
              select(studyid, binary_response_combined_description, sex_response_combined_description),
            by = "studyid") %>% 
  left_join(demo_self_age %>%
              select(studyid, age),
            by = "studyid")

rm(demo_self_gender, demo_self_age, demo_self)


### * 1.24 some data wrangling
demo_self_gender_age <- demo_self_gender_age %>%
  mutate(participant_cnt = 1,
         gender = case_when(binary_response_combined_description == "Man/Boy" ~ "Men+",
                            binary_response_combined_description == "Woman/Girl" ~ "Women+",
                            T ~ binary_response_combined_description)) %>%
  ## following the age groups in census, create an age group variable in demo survey data
  mutate(age_group = cut(age, 
                         breaks = c(0, 15, 65, Inf),
                         labels = c("0 to 14 years", "15 to 64 years", "65 years and over"),
                         right = F)) %>%
  ## aq2hh_size_calc being 0 (which it sometimes is)
  mutate(across(.cols =  c(aq2hh_size_calc), .fns = ~ pmax(1, as.numeric(.x)))) %>%
  mutate(across(.cols =  c(cdcsd, chsa, gender), .fns = as_factor))


### * 1.25 imputation as some variables have missing data
## PATIENCE: this takes @5 minutes, as it goes through multiple iterations
imputed_demo_self_gender_age <- impute_vars(demo_self_gender_age,
                                            to_impute = c("cdcsd", "chsa_pre_2023",
                                                          "aq2hh_size_calc", "gender", "age"),
                                            method = "rf")

### * 1.26 more data wrangling, mostly hh_ids

## update age_group for imputed cases
imputed_demo_self_gender_age <- imputed_demo_self_gender_age %>%
  mutate(age_group = cut(age, 
                         breaks = c(0, 15, 65, Inf),
                         labels = c("0 to 14 years", "15 to 64 years", "65 years and over"),
                         right = F))

summary(imputed_demo_self_gender_age$age_group)
summary(imputed_demo_self_gender_age$gender)
summary(imputed_demo_self_gender_age$chsa_pre_2023)
summary(as.factor(imputed_demo_self_gender_age$hh_id))


## save file
##   filtered data with age, gender and chsa_pre_2023 categories
##   with missing values imputed in cdcsd, chsa_pre_2023, aq2hh_size_calc, gender and age group
write_csv(imputed_demo_self_gender_age,
          file = file.path(SAVE_path, "Weighting",
                           paste0("1b_imputed_self_gender_age_geog_", Sys.Date(), ".csv")))


### * 1.3 load accompanying data ----
## BC Demographic Survey Technical Report, Appendix 3: Sampling by Community Health Service Areas
## household sampling data (note that chsa is actually chsa_pre_2023)
hh_sampled <- read_csv(file.path("inputs", "2023-bcds-households-sampled-by-chsa.csv")) %>% 
  clean_names() %>% 
  select(chsa = chsa_number, census_hh = x2021_census_households, sampled_hh = sampled_households)

## Census 2021: BC frequencies by gender and by age group
BC_pop <- read_csv(file.path("inputs", "9810002001_databaseLoadingData.csv")) %>%
  rename(gender = `Gender (3a)`,
         age_group = `Age (in single years), average age and median age (128)`)

BC_pop_age_group <- BC_pop %>%
  filter(str_detect(gender, "Total"), !str_detect(age_group, "Total")) %>%
  select(age_group, Freq = VALUE)

BC_pop_gender <- BC_pop %>% 
  filter(!str_detect(gender, "Total"), str_detect(age_group, "Total")) %>%
  select(gender, Freq = VALUE)


#### 2. weight random sample ----

## get initial person design weights

## w1 = 1/(p=1) = 1
w1 <- get_person_weight_chsa(imputed_demo_gender_age) %>%
  rename("wt1" = wt)

## add weight to survey data
imputed_demo_gender_age <- imputed_demo_gender_age %>% left_join(w1, by = "studyid")


## w2 = 1/(sampled_hh/census_hh), based on `chsa_pre_2023`
w2 <- get_person_weight_hh(imputed_demo_gender_age, hh_sampled) %>%
  rename("wt2" = wt)

## add weight to survey data
imputed_demo_gender_age <- imputed_demo_gender_age %>% left_join(w2, by = "studyid")


## w3 = 1/(p=1) = 1
w3 <- get_person_weight_ind(imputed_demo_gender_age) %>%
  rename("wt3" = wt)

## add weight to survey data
imputed_demo_gender_age <- imputed_demo_gender_age %>% left_join(w3, by = "studyid")


## multiply weights to get base weights
imputed_demo_gender_age <- imputed_demo_gender_age %>%
  mutate(base_wt = wt1*wt2*wt3)
## this will be NA for anyone with NA (which is really only possible in wt2)


## get non-response adjustments for hh non-response
c1 <- get_hh_classes(imputed_demo_gender_age, hh_sampled)
w4 <- get_noresponse_weight_hh(imputed_demo_gender_age, c1)


## adjust household non-response
imputed_demo_gender_age <- imputed_demo_gender_age %>% left_join(w4, by = "hh_id") %>%
  rename("adj1" = adj)

## get person level non-response adjustments for each person in hh
## currently following a basic method where each hh is a class
w5 <- get_noresponse_weight_ind(imputed_demo_gender_age)

imputed_demo_gender_age <- imputed_demo_gender_age %>% left_join(w5, by = "studyid") %>%
  rename("adj2" = adj)

imputed_demo_gender_age <- imputed_demo_gender_age %>%
  mutate(final_wt = base_wt*adj1*adj2)


#### 3. rake for the BC population & trim and rescale ----

## frequencies by age group and by gender
pop_variables <- list(
  BC_pop_age_group,
  BC_pop_gender
)


## rake weight and join to data (id = hh_id, strata = chsa)
r1 <- get_rake_weight_ind(imputed_demo_gender_age, pop_variables, var_chsa = "chsa_pre_2023")

raked_demo_gender_age <- imputed_demo_gender_age %>% left_join(r1, by = 'studyid')


## trim outlier weights (e.g., those in the 1st and 99th percentiles)
percentiles <- quantile(raked_demo_gender_age$wt_rake, probs = seq(0, 1, length.out = 101), na.rm = TRUE)
percentiles[c(1,2, 100,101)]

## trim & rescale weights to BC population (will not work on NA weights)
t1 <- trimWeights(svydesign(id = ~hh_id,
                            strata = ~chsa_pre_2023,
                            weights = ~wt_rake,
                            data = raked_demo_gender_age %>% filter(!is.na(wt_rake))),
                  upper = percentiles["99%"], lower = percentiles["1%"])
trimmed_demo_gender_age <- raked_demo_gender_age %>%
  ## drop NA wt_rake rows so column bind matches up
  filter(!is.na(wt_rake)) %>%
  ## bind new weights as "wt_rake_trim"
  bind_cols(tibble(wt_rake_trim = weights(t1))) %>%
  ## bind back dropped NA wt_rake rows
  bind_rows(raked_demo_gender_age %>% filter(is.na(wt_rake)))
## check
sum(trimmed_demo_gender_age$wt_rake_trim, na.rm = TRUE)       ## matches BC pop, good!


## create new pop variables that might be significantly different between the two populations
## (this works because you have a reference population containing all variables, now)
new_pop_variables <- list(
  
  ## frequencies by age group
  trimmed_demo_gender_age %>% filter(!is.na(age_group)) %>%
    group_by(age_group) %>% summarize(Freq = sum(wt_rake_trim, na.rm = TRUE)) %>%
    mutate(Freq = as.numeric(Freq)),
  
  ## frequencies by gender
  trimmed_demo_gender_age %>% filter(!is.na(gender)) %>%
    group_by(gender) %>% summarize(Freq = sum(wt_rake_trim, na.rm = TRUE)) %>%
    mutate(Freq = as.numeric(Freq)),
  
  ## frequencies by chsa_pre_2023
  trimmed_demo_gender_age %>% filter(!is.na(chsa_pre_2023)) %>%
    group_by(chsa_pre_2023) %>% summarize(Freq = sum(wt_rake_trim, na.rm = TRUE)) %>%
    mutate(Freq = as.numeric(Freq)) %>%
    filter(Freq != 0)
  
)
var_list_new <- list(~age_group, ~gender, ~chsa_pre_2023)


#### 4. weight self-selected sample ----

## give all self-selected respondents a weight of 1
w_self_select <- 1

imputed_demo_self_gender_age <- imputed_demo_self_gender_age %>%
  mutate(final_wt = w_self_select) %>%
  ## update some chsa_pre_2023 (because either pop=0 or not in imputed_demo_gender_age data)
  mutate(final_wt = case_when(!chsa_pre_2023 %in% new_pop_variables[[3]]$chsa_pre_2023 ~ NA,
                              TRUE ~ final_wt))
# imputed_demo_self_gender_age %>% filter(is.na(final_wt)) %>% nrow()  ## there are some NAs, ok


#### 5. merge datasets, re-rake, trim and rescale ----

## join random sample respondents and self-selected respondents
all_demo_gender_age <- bind_rows(trimmed_demo_gender_age, imputed_demo_self_gender_age) %>%
  ## create final_wt2 as raked & trimmed weight for random sample and as final_wt (i.e., 1) for self-selected
  mutate(final_wt2 = case_when(upld_survey_type == 2 ~ wt_rake_trim, upld_survey_type == 1 ~ final_wt))


## rake weight and join to merged datasets
r2 <- get_rake_weight_ind(all_demo_gender_age, new_pop_variables, var_chsa = "chsa_pre_2023",
                          weight_var = "final_wt2", var_list = var_list_new)

raked_all_demo_gender_age <- all_demo_gender_age %>%
  left_join(r2 %>% rename(wt_rake2 = wt_rake), by = 'studyid')


## trim outliers & rescale
percentiles2 <- quantile(raked_all_demo_gender_age$wt_rake2, probs = seq(0, 1, length.out = 101), na.rm = TRUE)
percentiles2[c(1,2, 100,101)]

t2 <- trimWeights(svydesign(id = ~hh_id,
                            strata = ~chsa_pre_2023,
                            weights = ~wt_rake2,
                            data = raked_all_demo_gender_age %>% filter(!is.na(wt_rake2)),
                            nest = TRUE),
                  upper = floor(percentiles2["100%"]), lower = round(percentiles2["1%"], digits = 3))

trimmed_all_demo_gender_age <- raked_all_demo_gender_age %>%
  filter(!is.na(wt_rake2)) %>%
  bind_cols(tibble(wt_rake2_trim = weights(t2))) %>%
  bind_rows(raked_all_demo_gender_age %>% filter(is.na(wt_rake2)))

sum(trimmed_all_demo_gender_age$wt_rake2_trim, na.rm = TRUE)            ## matches BC pop, good
trimmed_all_demo_gender_age %>% filter(is.na(gender)) %>% nrow()        ## 0, good
trimmed_all_demo_gender_age %>% filter(is.na(age_group)) %>% nrow()     ## 0, good
trimmed_all_demo_gender_age %>% filter(is.na(wt_rake2_trim)) %>% nrow() ## just a few, ok


## save file
##   non-filtered data with age and gender categories
##   with missing values imputed in cdcsd, chsa+_pre_2023, aq2hh_size_calc, gender and age group
##   with weights added in, including initial weights, base_wt, adj1, adj2, final_wt, 
##   and wt_rake (raked to BC population by gender & age groups), 
##   wt_rake_trim (trimmed & rescaled wt_rake based on percentiles); all for random sample respondents only
##   and final_wt2 (wt_rake_trim for random sample and 1 for self-selected), wt_rake2 (raked final_wt2
##   to BC pop by gender and age_group again as well as chsa_pre_2023), wt_rake2_trim
write_csv(trimmed_all_demo_gender_age,
          file = file.path(SAVE_path, "Weighting",
                           paste0("2_weighted_gender_age_geog_", Sys.Date(), ".csv")))

