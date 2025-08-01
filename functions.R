#### packages ----
library(mice)    ## used in `impute_vars()`
library(ranger)  ## required in `mice`, used in `impute_vars()`
library(survey)  ## used in `get_rake_weight_ind()`


#### get_gender_categories(): gender categorization ----
## Input: 1. a table with wide records                            #
##           for sex at birth (q40agen),                          #
##           gender in the form q41gen_c0N,                       #
##           gen (q41gen_c0N + random assignment of non-binary)   #
##           must also include studyid                            #
##        2. gender_binary includes the male/female+              #
##           GEN column calculation                               #
##                                                                #
## Output: a table with a single record per study id              #
##         flags include:                                         #        
##         - final suggested sex, gender, binary gender codes     #
##         - english translation of                               #
##           sex, gender, binary gender codes                     #
##         - how many sex, gender options were present            #
##         - how many 'dont know' options were present            #
##         - how many 'prefer not to answer' options were present #
##         - a flag for each of the defined                       #
##           sex, gender, binary gender categories                #
get_gender_categories <- function(demo, gender_binary = FALSE) {
  
  ## pivot data to long form and remove duplicate codes from a distinct studyid 
  demo_long <- demo %>%
    select(studyid, q40agen, starts_with('q41gen'), gen) %>%
    pivot_longer(!studyid, names_to = 'original_column', values_to = 'response') %>%
    filter(!is.na(response)) %>%
    distinct(studyid, original_column, response)
  
  ## * sex at birth
  ## identify which types of responses people have within their own set for Sex at Birth
  ## * only one response allowed *
  demo_sex <- demo_long %>%
    filter(original_column == "q40agen") %>%
    distinct(studyid, response) %>%
    mutate(sex_female = response == 1,
           sex_intersex_indeterminate = response == 2,
           sex_male = response == 3,
           sex_pref_no = response == 99) %>%
    group_by(studyid) %>%
    summarize(n = n(),
              sex_female = any(sex_female),
              sex_intersex_indeterminate = any(sex_intersex_indeterminate),
              sex_male = any(sex_male),
              sex_pref_no = any(sex_pref_no)) %>%
    ## categorize as either female, intersex_indeterminate, male or provided more than one response 
    mutate(sex_count = sex_female + sex_intersex_indeterminate + sex_male,
           sex_response_combined = case_when(sex_count > 1 ~ 4,
                                             sex_female ~ 1,
                                             sex_intersex_indeterminate ~ 2,
                                             sex_male ~ 3,
                                             sex_pref_no ~ 99,
                                             TRUE ~ 100),
           sex_response_combined_description = case_when(
             sex_count > 1 ~ 'Reported multiple sex at birth',
             sex_female ~ 'Female',
             sex_intersex_indeterminate ~ 'Intersex/Indeterminate',
             sex_male ~ 'Male',
             sex_pref_no ~ 'Prefer not to answer',
             TRUE ~ 'MISSED A CASE')) %>%
    select(studyid, contains("response_combined"),
           contains("_count"), everything(), -n)
  
  ## * gender
  ## identify which types of responses people have within their own set for Gender
  ## * only one response allowed * but then recoded in q41gen_c02:03 as needed
  demo_gender <- demo_long %>% 
    filter(original_column == "q41gen_c01") %>%
    distinct(studyid, response) %>%
    mutate(gdr_response_combined_description = case_when(
      response == 1   ~ 'Man/Boy',
      response == 3   ~ 'Non-Binary Person',
      response == 2   ~ 'Woman/Girl',
      response == 100 ~ 'Indeterminate',
      response == 88  ~ "I don't know/I am unsure",
      response == 99  ~ 'Prefer not to answer',
      response == 80000 ~ 'Left a comment',
      TRUE ~ 'MISSED A CASE'),
    ) %>%
    rename(gdr_response_combined = response)
  
  demo_temp <- demo_long %>% 
    filter(str_detect(original_column, "q41gen")) %>%
    distinct(studyid, response) %>%
    mutate(gdr_man               = response == 1,
           gdr_non_binary_person = response == 3,
           gdr_other             = response %in% 101:113,
           gdr_woman             = response == 2,
           gdr_indeterminate     = response == 100,        ## indeterminate
           gdr_dont_know         = response == 88,
           gdr_pref_no           = response == 99,
           gdr_comment           = response == 80000) %>%
    group_by(studyid) %>% 
    summarize(gdr_man               = any(gdr_man),
              gdr_non_binary_person = any(gdr_non_binary_person),
              gdr_other             = any(gdr_other),
              gdr_woman             = any(gdr_woman),
              gdr_indeterminate     = any(gdr_indeterminate),
              gdr_dont_know         = any(gdr_dont_know),
              gdr_pref_no           = any(gdr_pref_no),
              gdr_comment           = any(gdr_comment)) %>% 
    ## categorize as either man, non-binary, woman, other, indeterminate, dk, pnta, comment or provided more than one response
    mutate(gdr_count = gdr_man + gdr_non_binary_person + gdr_woman)
  
  demo_gender <- full_join(demo_gender, demo_temp, by = "studyid")
  rm(demo_temp)
  
  
  ## * join data
  ## join sex and gender data
  demo_final <- full_join(demo_sex, demo_gender, by = "studyid")
  
  
  ## * gender as binary
  ## identify which types of responses people have within their own set for Gender
  ## where non-binary were randomly assigned as either man or woman (i.e., binary)
  ## therefore only man or woman responses (no multiples)
  if(gender_binary == TRUE) {
    
    demo_binary <- demo_long %>%
      filter(original_column == "gen") %>%
      distinct(studyid, response) %>%
      mutate(binary_man = response == 1,
             binary_woman = response == 2) %>%
      group_by(studyid) %>%
      summarize(n = n(),
                binary_man = any(binary_man),
                binary_woman = any(binary_woman)) %>%
      ## categorize as either man or woman or provided more than one response
      mutate(gdr_binary_count = binary_man + binary_woman,
             binary_response_combined = case_when(gdr_binary_count > 1 ~ 3,
                                                  binary_man ~ 1,
                                                  binary_woman ~ 2,
                                                  TRUE ~ 100),
             binary_response_combined_description = case_when(
               gdr_binary_count > 1 ~ 'Reported multiple gender',
               binary_man ~ 'Man/Boy',
               binary_woman ~ 'Woman/Girl',
               TRUE ~ 'MISSED A CASE')) %>%
      select(studyid, contains("response_combined"),
             contains("_count"), everything(), -n)
    
    demo_final <- full_join(demo_final, demo_binary, by = "studyid")
    
  }
  
  ## * done
  demo_final
  
}


#### get_age(): age ----
## Input: 1. a table with wide records                            #
##           for birth year and month (adob)                      #
##           set birth day as 15 (half before and half after)     #
##           if only year, set to month and day as June 30        #
##           must also include studyid                            #
##        2. age_from: date from which to calculate an age        #
##                                                                #
## Output: a table with a single record per study id              #
##         flags include:                                         #
##         - final suggested age                                  #
get_age <- function(demo, age_from = ymd('2023-12-31')) {
  
  ## pivot data to long form and remove duplicate codes from a distinct studyid 
  demo_long <- demo %>%
    select(studyid, adob) %>%
    pivot_longer(!studyid, names_to = 'original_column', values_to = 'response') %>%
    filter(!is.na(response)) %>%
    distinct(studyid, response)
  
  ## create age
  demo_final <- demo_long %>%
    mutate(dob_num = response, 
           ## convert adob to yyyymmdd, where day is the 15th for everyone
           response = case_when(nchar(response) == 8 ~ response,
                                nchar(response) == 6 ~ (response*100)+15,
                                nchar(response) == 4 ~ as.numeric(paste0(response, "0630")),
                                TRUE ~ 18000101),
           response = lubridate::ymd(response),
           ## create age as difference from age_from (where default is today())
           ## from: https://stackoverflow.com/questions/27096485/change-a-column-from-birth-date-to-age-in-r
           age_full = lubridate::interval(response, age_from) 
           / lubridate::duration(num = 1, units = "years"),
           age = as.integer(floor(age_full))) %>%
    rename(dob = response)
  
  demo_final
  
}


#### impute_vars() ----

## To impute the demo_race_gender_age dataframe before we calculate the weights
## since the raking needs extra information from respondents' other demographic characteristics such as 
## gender, age, and race. 
## If one respondent has missing value in gender, etc., we are not able to calculate the weight for her/him.
## Pewresearch has a paper which suggests that we could use MICE iterated to get the imputation.

## First, convert "don't know", "dont want to respond", etc. to NA.
## Second, apply mice function to NA columns.
## Third, bind back to original dataframe.

## function: impute the missing value from data that selected
## para: 
##     data object: data
##     selected variable: 'gender', etc.
##     method:  cart: tree, lasso, logreg, rf for random forest (not working),
##     seed: random seed
## outcome: 
##     dataframe: with selected variable fields replaced by imputed value  
## example: 
##     selected variable is age group or gender
##     the outcome will be a dataframe

library(mice)
library(ranger)  ## required in `mice`


impute_vars <- function(.data, to_impute = NULL, method = "ranger", seed = NA, ...) {
  
  varnames <- names(.data)
  
  imp_vars <- .data %>% select_at(.vars = to_impute)
  
  mice_df  <- mice::mice(imp_vars, 
                         m = 1,
                         method = method,
                         seed = seed,
                         ...)
  
  completed_df <- mice::complete(mice_df)
  
  res <- .data %>% 
    select_at(.vars = setdiff(varnames, names(imp_vars))) %>% 
    bind_cols(completed_df) %>% 
    select_at(.vars = varnames)
  
  return(res)
  
}


#### is_unique(): helper function ----

is_unique <- function (x){
  
  if(!is.vector(x)) { stop("x is not a vector") }
  
  return_val <- TRUE
  
  if(length(x) != length(unique(x))) { return_val <- FALSE } 
  
  return_val
}


#### get_person_weight_chsa() ----
## Behavior: The function calculates person-level survey weights
##           taken over CHSA sampling units.  
##
## Input:    an rxc table with a single record per study id (studyid).
##
## Output:   an rx2 table with columns, 'studyid' and 'wt'          
##          'wt' is a numeric value representing the survey weight  
##           for each person, or individual record.     

get_person_weight_chsa <- function(data) {
  
  if(!inherits(data, "data.frame")) {
    stop("data must be a tibble or a dataframe")
  }
  
  if(!any(c("studyid") %in% names(data))) {
    stop("data must contain columns: studyid")
  }
  
  if(!is_unique(data$studyid)) {
    message("duplicate studyid's in data")
  }
  
  ## currently all chsa's are selected with probability 1 
  data %>% 
    select(studyid) %>% 
    mutate(p = 1,
           wt = 1/p) %>% 
    select(studyid, wt)
  
}


#### get_person_weight_hh() ----
## Behavior: The function calculates person-level survey weights
##           taken over households in each chsa
##
## Input:    an rxc table with a single record per study id (studyid)
##           and an 218x2 table with a single record for each chsa.
##
## Output:   an rx2 table with columns, 'studyid' and 'wt'          
##          'wt' is a numeric value representing the survey weight  
##           for each person, or individual record.     

get_person_weight_hh <- function(data, hh_sampled) {
  
  if(!inherits(data, "data.frame")) {
    stop("data must be a tibble or a dataframe")
  }
  
  if(!inherits(hh_sampled, "data.frame")) {
    stop("hh_sampling data must be a tibble or a dataframe")
  }
  
  if(!any(c("studyid", "chsa_pre_2023") %in% names(data))) {
    stop("data must contain columns: studyid, chsa_pre_2023")
  }
  
  if(!any(c("chsa", "census_hh", "sampled_hh") %in% names(hh_sampled))) {
    stop("support data must contain columns: chsa, census_hh, sampled_hh")
  }
  
  if(!is_unique(data$studyid)) {
    message("duplicate studyid's in data")
  }
  
  ## convert both columns to int or factor
  data <- data %>%
    mutate(chsa_pre_2023 = as_factor(chsa_pre_2023))
  hh_sampled <- hh_sampled %>%
    mutate(chsa = as_factor(chsa))
  
  data %>% 
    left_join(hh_sampled, by = c("chsa_pre_2023" = "chsa")) %>%
    mutate(p = sampled_hh/census_hh, 
           wt = 1/p) %>%
    select(studyid, wt)
  
}


#### get_person_weight_ind() ----
## Behavior: The function calculates person-level survey weights
##           for each person in the household. 
##
## Input:    an rxc table with a single record per study id (studyid).  
##
## Output:   an rx2 table with columns, 'studyid' and 'wt'          
##          'wt' is a numeric value representing the survey weight  
##           for each person, or individual record.     

get_person_weight_ind <- function(data) {
  
  if(!inherits(data, "data.frame")) {
    stop("data must be a tibble or a dataframe")
  }
  
  if(!any(c("studyid", "hh_id") %in% names(data))) {
    stop("data must contain columns: studyid, hh_id")
  }
  
  if(!is_unique(data$studyid)) {
    message("duplicate studyid's in data")
  }
  
  ## currently, all persons are selected with probability 1 
  data %>% 
    select(studyid) %>% 
    mutate(p = 1, 
           wt = 1/p) %>%
    select(studyid, wt)
  
}


#### get_hh_classes() ----
## Behavior: The function calculates classes to be used for creating 
##           non-response adjustments for each household.  
##
## Input:    an rxc table with a single record per study id (studyid) and additional 
##           tables.  Currently, the only table contains hh_id and chsa.
##
## Output:   an rx3 table with columns, 'hh_id', 'class' and 'ttl_hh_per_class'          
##          'class' is a numeric value used to bin households.

get_hh_classes <- function(data, hh_sampled, ...) {
  
  hhs <- data %>% 
    distinct(chsa = chsa_pre_2023, hh_id) %>%   ## this step renames chsa_pre_2023 as chsa so join below works
    ## remove some responses with no chsa
    filter(!is.na(chsa))
  
  hh_sampled %>% 
    distinct(chsa, sampled_hh) %>%
    ## assumes distinct chsa's in dataframe
    mutate(class = row_number()) %>%
    ## removes some chsa's with no hh_responses
    right_join(hhs, by = "chsa") %>%
    select(-c(chsa)) 
  
}


#### get_noresponse_weight_hh() ----
## Behavior: The function calculates person-level non-response adjustment
##           weights for each household.  
##
## Input:    an rxc table with a single record per study id (studyid). 
##           and an rx2 table, with a single record for each household (hh_id)
##           and a column indicating the class each household belongs to
##
## Output:   an rx2 table with columns, 'studyid' and 'wt_adj'          
##          'wt_adj' is a numeric value representing the survey weight 
##           adjustment for each person, or individual record.     

get_noresponse_weight_hh <- function(data, hh_class) {
  
  if(!inherits(data, "data.frame")) {
    stop("data must be a tibble or a dataframe")
  }
  
  if(!inherits(hh_class, "data.frame")) {
    stop("hh_responses data must be a tibble or a dataframe")
  }
  
  if(!any(c("studyid", "hh_id", "wt") %in% names(data))) {
    stop("data must contain columns: studyid, hh_id, wt")
  }
  
  if(!any(c("hh_id", "class", "sampled_hhs") %in% names(hh_class))) {
    stop("data must contain columns: hh_id, class, sampled_hh")
  }
  
  if(!is_unique(data$studyid)) {
    message("duplicate studyid's in data")
  }
  
  if(!is_unique(hh_class$hh_id)) {
    message("duplicate household's in data")
  }
  
  ## pull responses 
  ## NOTE: each person in a chsa happens to have the same base_wt, so this code works for now
  wt <- data %>% 
    distinct(hh_id, base_wt) %>% ## see above note
    left_join(hh_class, by = "hh_id") %>%
    mutate(hh_wt = base_wt) 
  
  adj <- wt %>%
    group_by(class) %>%
    mutate(denominator = sum(hh_wt), 
           numerator = sampled_hh*hh_wt,
           adj = numerator/denominator) %>%
    ungroup() 
  
  adj %>% select(hh_id, adj)
  
}


#### get_noresponse_weight_ind() ----
## Behavior: The function calculates person-level non-response adjustment
##           weights for each household.  It is not complete, 
##           just a roughed-in placeholder for now
##
## Input:    an rxc table with a single record per study id (studyid). 
##           
## Output:   an rx2 table with columns, 'studyid' and 'wt_adj'          
##          'wt_adj' is a numeric value representing the survey weight 
##           adjustment for each person, or individual record.     

get_noresponse_weight_ind <- function(data) {
  
  if(!inherits(data, "data.frame")) {
    stop("data must be a tibble or a dataframe")
  }
  
  if(!any(c("studyid", "hh_id", "base_wt", "adj1", "aq2hh_size_calc") %in% names(data))) {
    stop("data must contain columns: studyid, hh_id, base_wt, adj1, aq2hh_size_calc")
  }
  
  if(!is_unique(data$studyid)) {
    message("duplicate studyid's in data")
  }
  
  ## get adjustment, given you live in a household of size aq2hh_size_calc
  data %>% 
    group_by(hh_id) %>% 
    mutate(
      ## sum all eligible respondents
      denominator = sum(base_wt*adj1),
      ## use pmax to avoid aq2hh_size_calc being 0 (which it sometimes is) this should be done in the data process step. 
      ## sum all eligible, given we know their weights
      numerator = pmax(1, aq2hh_size_calc) * base_wt * adj1,
      adj = numerator / denominator
    ) %>% 
    ungroup() %>% 
    select(studyid, adj)
  
}


#### get_rake_weight_ind() ----
## Behavior: The function calculates a new weight for each person 
##           in the study, calibrated to specified population variables
##
## Input:    an rxc table with a single record per study id (studyid) and
##           a list of tables representing the marginal distributions of 
##           population variables to pass calibrate on
##
## Output:   an rx2 table with columns, 'studyid' and 'wt_rake'          
##          'wt_rake' is a numeric value representing a new survey weight 
##           for each person, or individual record. This weight has
##           the final_wt rolled into it.
## weight_var: string to specify which weight should be used for raking
## 
## var_list: a list of variables that need calibrate on.
## 
## subset_ids: when we want to rake the weight to match a subset of the population, instead of the all the population as a whole.

get_rake_weight_ind <- function(data, pop, weight_var = "final_wt", var_chsa = "chsa_pre_2023",
                                var_list = list(~age_group, ~gender), subset_ids = NULL,
                                drop_chsa = c(3370,5160,5180,5190,5246,5233,5331,5332)) {
  
  if(!inherits(data, "data.frame")) {
    stop("data must be a tibble or a dataframe")
  }
  
  vars <- vector()
  for(i in seq_along(var_list)) {  vars <- c(vars, as.character(var_list[[i]][[2]]))  }; rm(i)
  if(!any(c("studyid", weight_var, vars) %in% names(data))) {
    stop(paste0("data must contain columns: studyid, ", paste(c(weight_var, vars), collaspe = ", ")))
  }
  
  if(!is_unique(data$studyid)) {
    message("duplicate studyid's in data")
  }
  
  if(!is.list(pop)) { 
    stop("population variables must be passed as a list") 
  }
  
  ## do some error checking on the pop data frame here if desired
  
  data <- data[!is.na(data[[weight_var]]), ]
  
  data <- data %>% mutate(chsa_var = var_chsa)
  
  data <- data %>%
    filter(!chsa_var %in% drop_chsa)
  
  for(i in seq_along(var_list)) {  data <- data %>% filter(!is.na(!!sym(vars[i])))  }; rm(i)
  
  ## create survey design objects
  survey.lonely.psu = "adjust"
  survey.adjust.domain.lonely = T
  
  design <- svydesign(
    id = ~ hh_id,         ## no clustering
    strata = ~ chsa_var,  ## the strata id
    weights = as.formula(paste0("~", weight_var)),
    data = data
  )
  
  ## use S3 method for class survey.design which subsets a survey design object by
  ## keeping strata information but assigns a weight of 0 to anyone not in the subpopulation
  ## we are interested in.
  
  if(!is.null(subset_ids)){
    designr <- rake(subset(design, studyid %in% subset_ids), var_list, pop, 
                    control = list(maxit = 100, epsilon = 1, verbose = FALSE))
    return_df <- data.frame(studyid = subset(data, studyid %in% subset_ids, studyid),
                            wt_rake = weights(designr))
  } else {
    designr <- rake(design, var_list, pop, 
                    control = list(maxit = 100, epsilon = 1, verbose = FALSE))
    return_df <- data.frame(studyid = data$studyid,
                            wt_rake = weights(designr))
  }
  
  return(return_df)
  
}


