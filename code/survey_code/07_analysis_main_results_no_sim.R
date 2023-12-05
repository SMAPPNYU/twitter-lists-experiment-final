##############################################################################
# File-Name: 07_analysis_main_results.r
# Date: 2023
# author: Tiago Ventura
# Purpose: analysis with raw data
# Data in: "data/pilot/all_data_processed_processed.rds" rds format
# status: ongoing
# Machine: MacOS High Sierra
##############################################################################

# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, qualtRics, 
               #tidylog,
               DeclareDesign, rsample)

source(here("code", "survey_code", "utils_modeling.R"))
# open data ---------------------------------------------------------------

d = readRDS(file = here("data","pilot", "all_data_processed_processed.rds"))

# Create compliance measure -----------------------------------------------
d <- d %>%
      mutate(compliance_one_side=case_when(exp=="treatment" & w3_correct_fct_submissions>9 ~ 1,
                                           TRUE ~ 0))
# compliers
table(d$compliance_one_side[d$exp=="treatment"])

# bootstrap resampling ----------------------------------------------------
#set.seed(1313)
d_s <- bootstrapping_resampling(d, 1200, exp, 1200)
d2 <- d %>% mutate(id_sample=1) %>% group_by(id_sample) %>%nest()

# covariates --------------------------------------------------------------
cov <- c("w1_age", "w1_gender", "w1_race", "w1_education", "w1_politics", 
  "w1_usage_twitter", "w1_tw_freq",
  "w1_tw_usage_family_friends" , 
  "w1_tw_usage_news"      , 
  "w1_tw_usage_sports"  , 
  "w1_tw_usage_interact_politicians", 
  "w1_tw_usage_politics", 
  "w1_tw_usage_share_my_opinion", 
  "w1_tw_following"          , 
  "w1_tw_followers",
  "w2_dl_zcore", # z score for digital literacy
  "w2_trust_zcore", # z score for trust in the news
  "w2_ideo_place_self", # ideological placement
  "w2_party_id_imputation" ,# partisanship
  "w2_affective_polarization", # affective_pol 
  "w2_ingroup_affective_pol",  # ingroup_pol
  "w2_outgroup_affective_pol", #outgroup_pol, 
  "w2_sm_cyn_zcore", # social media cynicism
  "w2_pol_cyn_1" ,# pol_cyn
  "w2_ext_media_score", # media trust
  "w2_belief_accuracy" ,# belief accuracy
  "w2_fake_news_problem", # fake news problem
  "w2_fc_familiarity", # fact-checking familiarity
  "w2_fc_trust", # trust in fct
  "w2_misinfo_literacy_zcore", # misinfo literacy z-score
  "w2_social_media_trust_zcore") # trust in information from social media)

cov_no_post_treatment_bias <- c("w1_age", "w1_gender", "w1_race", "w1_education", "w1_politics", 
         "w1_usage_twitter", "w1_tw_freq",
         "w1_tw_usage_family_friends" , 
         "w1_tw_usage_news"      , 
         "w1_tw_usage_sports"  , 
         "w1_tw_usage_interact_politicians", 
         "w1_tw_usage_politics", 
         "w1_tw_usage_share_my_opinion", 
         "w1_tw_following"          , 
         "w1_tw_followers",
         "w2_dl_zcore", # z score for digital literacy
         "w2_trust_zcore", # z score for trust in the news
         "w2_ideo_place_self", # ideological placement
         "w2_party_id_imputation" ,# partisanship
         "w2_affective_polarization", # affective_pol 
         "w2_ingroup_affective_pol",  # ingroup_pol
         "w2_outgroup_affective_pol", #outgroup_pol, 
          "w2_ext_media_score", # media trust
          "w2_misinfo_literacy_zcore") # trust in information from social media)

# Belief Accuracy ------------------------------------------------------------------

#### DV:  Belief Accuracy ----------------
depvar <- "w3_belief_accuracy"


# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(w3_belief_accuracy_ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         w3_belief_accuracy_caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 




# plot
estimate_no_sim <- function(data, columns){
data %>%
    select(-data) %>%
    unnest(columns) %>%
    select(effect=estimate, everything()) %>%
    mutate(lb95=effect-1.96*std.error, 
           ub95=effect+1.96*std.error)
    
}

output_td <- map(c("w3_belief_accuracy_ittcii", 
                   "w3_belief_accuracy_caceii"), ~
            estimate_no_sim(models, .x)) 

outputs = c(output_td)

models_n = list("Cov-ITT", 
                "CACE")

dep_var_label = c(rep("Truth \n Discernment", 2))


# plot
plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects") 



#### DV:False News Accuracy --------------
depvar <- "w3_false_news_accuracy"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_false <- map(c( 
                   "ittcii", 
                   "caceii"), ~
                   estimate_no_sim(models, .x)) 
 

#### DV: True News Accuracy ----------------
depvar <- "w3_true_news_accuracy"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_true <- map(c("ittcii", 
                     "caceii"), ~
                      estimate_no_sim(models, .x)) 


# combine
outputs = c(output_td, output_false, output_true)
models_n = rep(c("ITT", 
                 "CACE"), 3)

dep_var_label = c(rep("Truth \n Discernment", 2), rep("False Rumors \n Accuracy", 2), 
                  rep("True News \n Accuracy", 2))




# plot
plot_td = plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects") 


# plot
plot_td

save_function("h1_truth_discernment_restricted_no_sims.png")


# Conspiracy Beliefs ------------------------------------------------------

#### DV: Index Conspiracy Belief ----------
depvar <- "w3_cons_zcore"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_cb <- map(c("ittcii", 
                   "caceii"), ~
                      estimate_no_sim(models, .x)) 


# Estimate on raw data

# conver the measures to numeric
d_s <- d_s %>%
  mutate(data=map(data, ~ .x %>% mutate(across(w3_conspiracies_1:w3_conspiracies_4, 
                                               as.numeric))))
d2 <- d2 %>%
  mutate(data=map(data, ~ .x %>% mutate(across(w3_conspiracies_1:w3_conspiracies_4, 
                                               as.numeric))))


#### DV:  Conspiracy Belief 1 -----------
depvar <- "w3_conspiracies_1"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_cb1 <- map(c("ittcii", 
                    "caceii"), ~
                   estimate_no_sim(models, .x)) 

#### DV: Conspiracy Belief 2 -----------
depvar <- "w3_conspiracies_2"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_cb2 <- map(c("ittcii", 
                    "caceii"), ~
                   estimate_no_sim(models, .x)) 

#### DV: Conspiracy Belief 3 -------------
depvar <- "w3_conspiracies_3"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_cb3 <- map(c("ittcii", 
                    "caceii"), ~
                   estimate_no_sim(models, .x)) 

#### DV: Conspiracy Belief 4 -----
depvar <- "w3_conspiracies_4"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_cb4 <- map(c("ittcii", 
                    "caceii"), ~
                   estimate_no_sim(models, .x)) 




# combine

outputs = c(output_cb, output_cb1, output_cb3, output_cb4)
models_n = rep(c("ITT", 
                 "CACE"), 4)
dep_var_label = c(rep("Conspiracy Belief \n Index", 2),
                  rep("Conspiracy Belief \n Item 1", 2), 
                  rep("Conspiracy Belief \n Item 2", 2), 
                  rep("Conspiracy Belief \n Item 3", 2))




# plot
plot_cb = plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects") +
  facet_wrap(~dv, nrow=2)

# save
plot_cb

save_function("h1_consp_belief_restricted_no_sims.png")

# index
outputs = c(output_cb)
models_n = rep(c("ITT", 
                 "CACE"), 1)
dep_var_label = c(rep("Conspiracy Belief \n Index", 2))




# plot
plot_cb = plot_model(outputs, models_n, dep_var_label, 
                     title="", 
                     caption= "Standardized Treatment Effects") +
  facet_wrap(~dv, nrow=2)

# save
plot_cb

save_function("h1_onlyindex_consp_belief_restricted_no_sims.png")


# Misinformation literacy -------------------------------------------------

#### DV: Misinformation literacy score
depvar <- "w3_misinfo_literacy_zcore"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_ml <- map(c("ittcii", 
                   "caceii"), ~
                   estimate_no_sim(models, .x)) 


# Estimate on raw data

# conver the measures to numeric
d2 <- d2 %>%
  mutate(data=map(data, ~ .x %>% mutate(across(w3_misinfo_literacy_1:w3_misinfo_literacy_6, 
                                               as.numeric))))

d_s <- d_s %>%
  mutate(data=map(data, ~ .x %>% mutate(across(w3_misinfo_literacy_1:w3_misinfo_literacy_6, 
                                               as.numeric))))

#### DV: Misinformation literacy 1 -------------

depvar <- "w3_misinfo_literacy_1"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_ml1 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 

#### DV: Misinformation literacy 2 ----------
depvar <- "w3_misinfo_literacy_2"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_ml2 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 

#### DV: Misinformation literacy 3 -----------
depvar <- "w3_misinfo_literacy_3"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_ml3 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 


#### DV: Misinformation literacy ------

depvar <- "w3_misinfo_literacy_4"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_ml4 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 


#### DV: Misinformation literacy --------

depvar <- "w3_misinfo_literacy_5"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_ml5 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 

#### DV: Misinformation literacy 6 ----------

depvar <- "w3_misinfo_literacy_6"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_ml6 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 



# combine
outputs = c(output_ml, output_ml1,output_ml2, output_ml3, output_ml4, 
            output_ml5, output_ml6)

models_n = rep(c("ITT", 
                 "CACE"), 7)

dep_var_label = c(rep("News Media \n Literacy Index", 2),
                  rep("Item 1", 2), 
                  rep("Item 2", 2), 
                  rep("Item 3", 2), 
                  rep("Item 4", 2), 
                  rep("Item 5", 2),
                  rep("Item 6", 2))




# plot
plot_ml = plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects")  +
  facet_wrap(~dv, nrow=2)

# svae
plot_ml 
save_function("h2_misinformation_literacy_restricted_no_sims.png")

# only_index
outputs = c(output_ml)

models_n = rep(c("ITT", 
                 "CACE"), 1)

dep_var_label = c(rep("News Media \n Literacy Index", 2))




# plot
plot_ml = plot_model(outputs, models_n, dep_var_label, 
                     title="", 
                     caption= "Standardized Treatment Effects")  +
  facet_wrap(~dv, nrow=2)

# svae
plot_ml 
save_function("h2_only_index_misinformation_literacy_restricted_no_sims.png")


# Affective Polarization --------------------------------------------------

# recode to get the imputation from pre-treatment for ingroup and outgroup

d2 <- d2 %>%
        mutate(data=map(data, ~ .x %>%
                          mutate(w3_ingroup_affective_pol=case_when(w2_party_id_imputation=="Democrat" ~ w3_ft_democrats, 
                                                                 w2_party_id_imputation=="Republican"~w3_ft_republicans), 
                                 w3_outgroup_affective_pol=case_when(w2_party_id_imputation=="Democrat" ~ w3_ft_republicans, 
                                                                  w2_party_id_imputation=="Republican"~w3_ft_democrats))
        ))

d_s <- d_s %>%
  mutate(data=map(data, ~ .x %>%
                    mutate(w3_ingroup_affective_pol=case_when(w2_party_id_imputation=="Democrat" ~ w3_ft_democrats, 
                                                              w2_party_id_imputation=="Republican"~w3_ft_republicans), 
                           w3_outgroup_affective_pol=case_when(w2_party_id_imputation=="Democrat" ~ w3_ft_republicans, 
                                                               w2_party_id_imputation=="Republican"~w3_ft_democrats))
  ))


## DV: Candidate Affective polarization ---------
depvar <- "w3_affective_party_polarization"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_pol <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 


## dv: outgroup affective pol ---------
depvar <- "w3_outgroup_affective_pol"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_pol_out <- map(c("ittcii", 
                        "caceii"), ~
                    estimate_no_sim(models, .x)) 

## dv: ingroup affective pol ----------------

depvar <- "w3_ingroup_affective_pol"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_pol_in <- map(c("ittcii", 
                       "caceii"), ~
                    estimate_no_sim(models, .x)) 

## dv: party affective polarization ---------

depvar <- "w3_affective_polarization"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_pol_party <- map(c("ittcii", 
                          "caceii"), ~
                       estimate_no_sim(models, .x)) 



# combine
outputs = c(output_pol_party, output_pol, output_pol_out, output_pol_in)

models_n = rep(c("ITT", 
                 "CACE"), 4)

dep_var_label = c(rep("Party Affective \n Polarization", 2),
                  rep("Candidate Affective \n Polarization", 2), 
                  rep("Outgroup Feelings", 2), 
                  rep("Ingroup Feelings", 2))



# plot
plot_pol <- plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects")  


# save
plot_pol
save_function("h3_polarization_restricted_no_sims.png")



# Media Trust -------------------------------------------------------------

# var: sum of three binary measures of media trust
# higher values means higher media trust

depvar <- "w3_ext_media_score"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 

# plot
output_ext_media <- map(c("ittcii", 
                          "caceii"), ~
                       estimate_no_sim(models, .x)) 


# combine
outputs = c(output_ext_media)

models_n = rep(c("ITT", 
                 "CACE"), 1)

dep_var_label = c(rep("Media Trust Score", 2))


# plot
plot_media <- plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects")  


#save
plot_media

save_function("h4_media_trust_restricted_no_sims.png")


# Political Cynicism ------------------------------------------------------

# var: survey question measuring political cynicism
# higher values means higher media trust

# d_s <- d2 %>%
#         mutate(data=map(data, ~.x %>% mutate(w3_pol_cyn_1_num = as.numeric(w3_pol_cyn_1))))

# check order
depvar <- "w3_polcyn_zcore"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_pol_cyn <- map(c("ittcii", 
                        "caceii"), ~
                          estimate_no_sim(models, .x)) 


# combine
outputs = c(output_pol_cyn)

models_n = rep(c("ITT", 
                 "CACE"), 1)

dep_var_label = c(rep("Political Cynicism", 2))


# plot
plot_polcyn <- plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects")  


# save
plot_polcyn
save_function("h5_political_cynicism_restricted_no_sims.png")


#Social Media  Cynicism ------------------------------------------------------

#### DV: Social Media Cynicism Score
depvar ="w3_sm_cyn_zcore"


# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 

# plot
output_sm <- map(c("ittcii", 
                   "caceii"), ~
                   estimate_no_sim(models, .x)) 


# Estimate on raw data

# conver the measures to numeric
d_s <- d_s %>%
  mutate(data=map(data, ~ .x %>% mutate(across(w3_sm_cynicism_1:w3_sm_cynicism_3, 
                                               as.numeric))))

d2 <- d2 %>%
  mutate(data=map(data, ~ .x %>% mutate(across(w3_sm_cynicism_1:w3_sm_cynicism_3, 
                                               as.numeric))))

#### DV: Social Media Cyn 1 -------------

depvar <- "w3_sm_cynicism_1"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_sm1 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 

#### DV: Social Media Cyn 2 ----------
depvar <- "w3_sm_cynicism_2"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_sm2 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 

#### DV: Social Media Cyn 3 -----------
depvar <- "w3_sm_cynicism_3"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_sm3 <- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 



# combine
outputs = c(output_sm, output_sm1,output_sm2, output_sm3)
models_n = rep(c("ITT", 
                 "CACE"), 4)
dep_var_label = c(rep("Social Media \n Cynicism Index", 2),
                  rep("Social Media \n Cynicism 1", 2), 
                  rep("Social Media \n Cynicism 2", 2), 
                  rep("Social Media \n Cynicism 3", 2))


# plot
plot_soccyn <- plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects")  +
  facet_wrap(~dv, nrow=2)

# save
plot_soccyn
save_function("h6_social_media_cyn_restricted_no_sims.png")

# only index

# combine
outputs = c(output_sm)
models_n = rep(c("ITT", 
                 "CACE"), 1)
dep_var_label = c(rep("Social Media \n Cynicism Index", 2))
                  
plot_soccyn <- plot_model(outputs, models_n, dep_var_label, 
                          title="", 
                          caption= "Standardized Treatment Effects")  +
  facet_wrap(~dv, nrow=2)

# save
plot_soccyn
save_function("h6_only_index_social_media_cyn_restricted_no_sims.png")




# Ability in detect misinfo -----------------------------------------------

# Perceived ability: average of two subjective questions
# Overconfidence: difference between the respondent’s self-reported ability ($Perc\_Misinfo\_Ident_i$) and their actual performance 

d2 <- d2 %>%
  mutate(data=map(data, ~.x %>%   
                    rowwise() %>%
                    mutate(w3_ability_mean=mean(c(w3_fc_ability_specific_1, w3_fc_ability_general_1))) %>%
                    ungroup()))

d_s <- d_s %>%
  mutate(data=map(data, ~.x %>%   
                    rowwise() %>%
                    mutate(w3_ability_mean=mean(c(w3_fc_ability_specific_1, w3_fc_ability_general_1))) %>%
                    ungroup()))

# over confidence

# convert to a quantile ranks
ecdf_func <- ecdf(d$w3_false_news_accuracy)

d2 <- d2 %>%
  mutate(data=map(data, ~.x %>%   
                    mutate(w3_ability_rank=ecdf_func(w3_false_news_accuracy), 
                           w3_overconfidence=w3_ability_mean-100*w3_ability_rank))) 

d_s <- d_s %>%
  mutate(data=map(data, ~.x %>%   
                    mutate(w3_ability_rank=ecdf_func(w3_false_news_accuracy), 
                           w3_overconfidence=w3_ability_mean-100*w3_ability_rank))) 

# dv
depvar= "w3_ability_mean"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_mean<- map(c("ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 



# check order
depvar <- "w3_overconfidence"

# get covariates
add.vars = lasso_select_covariates(d_s$data[[1]], cov, depvar)
add.vars_no_post_treatment_bias = lasso_select_covariates(d_s$data[[1]], cov_no_post_treatment_bias, depvar)

## Models
models <- d2 %>%
  mutate(ittcii=map(data, ~ ols_covariates(depvar, data=.x, add.vars_no_post_treatment_bias)), 
         caceii=map(data, ~ func_cace(depvar, data=.x, add.vars_no_post_treatment_bias, covariates = TRUE))) 


# plot
output_overconfidence<- map(c(
                    "ittcii", 
                    "caceii"), ~
                    estimate_no_sim(models, .x)) 



# combine
outputs = c(output_mean, output_overconfidence)

models_n = rep(c("ITT", 
                 "CACE"), 2)

dep_var_label = c(rep("Self-Report Ability", 2),
                  rep("Overconfidence ", 2))


# plot
plot_ability <- plot_model(outputs, models_n, dep_var_label, 
           title="", 
           caption= "Standardized Treatment Effects")  +
  facet_wrap(~dv, nrow=2)

# save
plot_ability
save_function("h7_ability_misinfo_restricted_no_sims.png")



# Save all point-estimates for power analysis -----------------------------
add_outcome <- function(data, outcome_){
  data %>%
    mutate(outcome=outcome_)
}

full_data <- add_outcome(plot_td$data, "Truth Discerniment") %>%
  bind_rows(add_outcome(plot_cb$data, "Conspiracy Beliefs")) %>%
  bind_rows(add_outcome(plot_ml$data, "Misinformation Literacy")) %>%
  bind_rows(add_outcome(plot_pol$data, "Affective Polarization")) %>%
  bind_rows(add_outcome(plot_media$data, "Trust in the Media")) %>%
  bind_rows(add_outcome(plot_soccyn$data, "Social Media Cynicism")) %>%
  bind_rows(add_outcome(plot_polcyn$data, "Political Cynicism")) %>%
  bind_rows(add_outcome(plot_ability$data, "Self-Reporter Ability to Detect Misinfo")) %>%
  mutate(model=fct_rev(model))

write_csv(full_data, here("output", "pilot", "all_results_no_sims.csv"))  

# Generate other graphs ---------------------------------------------------

full_data <- full_data %>%
  group_by(outcome) %>%
  nest() 

for(i in 1:nrow(full_data)){
ggplot(full_data$data[[i]],
       aes(y=dv, 
           x=effect, 
           color=model,
           fill=model,
           label=round(effect, 2))) +
  geom_errorbar(aes(xmin=lb95, 
                    xmax=ub95),  
                size=2, width=0, alpha=.8, 
                position=position_dodge(width = .8)) +
  geom_point(size=14, shape=21, fill="white",
             position=position_dodge(width = .8), 
             stroke=1, alpha=1) +
  geom_text(
    fontface = "italic", 
    fill="white", 
    color = "black",
    size=4, 
    position = position_dodge(.8)) +
  geom_vline(aes(xintercept=0), linetype="dashed", color="black", alpha=.8) +
  ylab("") +
  xlab("") +
  labs(title=paste0("Outcome: ", full_data$outcome[i]), 
       subtitle="", 
       caption= "Standardized Treatment Effects") +
  theme(legend.position = "bottom")  +
  scale_color_manual(values=c("black", "gray80"), name="Model:") 
  
save_function(paste0(str_replace(str_to_lower(full_data$outcome[i]), " ", "_"), "_viz_no_sims.png"))

}


