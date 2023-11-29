##############################################################################
# File-Name: 03_clean_pre_treatment_survey.r
# Date: 2023
# author: Tiago Ventura
# Purpose: Main script to clean the data coming from the pre_treatment survey 
# Data in: "data/pilot/pre_treatment/pre_treatment_raw.csv"
# Data out: "data/pilot/pre_treatment/pre_treatment_processed.csv" - cleaned data
#           "data/pilot/pre_treatment/pre_treatment_processed.rds" rds format
# status: ongoing
# Machine: MacOS High Sierra
##############################################################################


# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, qualtRics, tidylog)

source(here("code", "survey_code", "utils.R"))

# open data set -----------------------------------------------------------

d_clean = read_csv(here("data", "pilot", "pre_treatment", "pre_treatment_raw_no_iid.csv"))

# Clean colnames ----------------------------------------------------------
#d_clean = d

# Remove allspaces, trim, and capital letters

d <- d_clean %>% 
  clean_names()

# check 
dim(d)
colnames(d)


# Remove test emails and incompletes------------------------------------------------------

d <- d %>%
     select(email=recipient_email, everything()) %>%
      # remove tests with treatment and control email
      filter(!str_detect(email, "treatment|control")) %>%
      filter(!is.na(email)) %>%
      # remove participants who started the pre-treatment, but have not completed
      filter(finished==TRUE) 

# sanity check
length(unique(d$email))==dim(d)[[1]]

# recode digital literacy --------------------------------------------------------

# clean digital literacy items
d = d %>%
        # remove text from strongly agree / disagree
        mutate(across(digital_literacy_1:digital_literacy_4, 
                      ~ str_replace_all(.x, "Strongly Agree", "4"))) %>%
        mutate(across(digital_literacy_1:digital_literacy_4, 
                      ~str_replace_all(.x, "Strongly Disagree", "-4"))) %>%
        # convert to numeric
        mutate(across(digital_literacy_1:digital_literacy_4, 
                     as.numeric))  %>%
        # invert two variable so that all are on the same direction
        # higher positive values means higher digital literacy
        mutate(across(c(digital_literacy_1,digital_literacy_4), 
                      ~ case_when(.x<0 ~ abs(.x), 
                                .x>0 ~  -1*.x, 
                                  TRUE ~.x)))
  
  
# build z-score index
dl_vars <- c("digital_literacy_1",
             "digital_literacy_2",
             "digital_literacy_3",
             "digital_literacy_4") 

d <- d %>%
      # create zscore for each column
      mutate(across(dl_vars, center_scale)) %>%
      # sum up the zcores
      zscores_index(dl_vars, "dl_zcore") %>%
      # pca
      tidy_pca(dl_vars, scores_name = "dl_pcaindex", alpha_name="dl_alpha") 

# small alpha for digital literacy
d$dl_alpha

# news trust --------------------------------------------------------------

# values
trust_values <- names(table(d$news_trust_1))

# recode
trust_order <- c(trust_values[[2]], trust_values[[3]], trust_values[[4]], trust_values[[1]])

# recode all news trust measures
d <- d %>%
  # clean and recode
  mutate_at(vars(news_trust_1:news_trust_6),
            ~ fct_relevel(.x, trust_order)) %>%
  #rename collumns
  rename("news_trust_newspapers"="news_trust_1", # rename
         "news_trust_social_media"="news_trust_2", 
         "news_trust_news_websites"="news_trust_3", 
         "news_trust_radio"="news_trust_4", 
         "news_trust_local_news"="news_trust_5", 
         "news_trust_national_news"="news_trust_6")  %>%
  mutate(across(news_trust_newspapers:news_trust_national_news, 
                as.numeric))

# check
trust_vars  <- c("news_trust_newspapers",# rename
                 "news_trust_social_media", 
                 "news_trust_news_websites", 
                 "news_trust_radio", 
                 "news_trust_local_news", 
                 "news_trust_national_news")

# calculate trust z score index
d <- d %>%
  # create zscore for each column
  mutate(across(trust_vars, center_scale)) %>%
  # sum up the zcores
  zscores_index(trust_vars, "trust_zcore") %>% 
  # pca
  tidy_pca(trust_vars, scores_name = "trust_pcaindex", alpha_name="trust_alpha")   


# ok alpha here
unique(d$trust_alpha)


# Recode political variables ----------------------------------------------

## Ideological Placement

# get names
ideo_names <- names(table(d$ideo_place_self))

# reorder values from conversative to liberal
ideo_values <- c(ideo_names[[6]], ideo_names[[4]],ideo_names[[2]], 
                 ideo_names[[1]], 
                 ideo_names[[3]], ideo_names[[5]], ideo_names[[7]])

# reorder
d <- d %>%
      mutate(ideo_place_self=fct_relevel(ideo_place_self, ideo_values))

# check
levels(d$ideo_place_self)


## Partisanship
# create a party_id_imputation combining self-reported party id and party_id lean

d <- d %>%
      mutate(party_id_imputation=case_when(party_id=="Something else" ~ NA, 
                                party_id=="Independent" ~ party_id_lean, 
                                TRUE ~ party_id), 
             party_id_imputation=case_when(str_detect(party_id_imputation, "Republican")~"Republican", 
                                           str_detect(party_id_imputation, "Democrat")~"Democrat",
                                           party_id_lean=="Neither" ~ NA_character_,
                                           TRUE ~ party_id_imputation), 
             party_id_imputation=as_factor(party_id_imputation))
# check
table(d$party_id_imputation, d$party_id)
table(d$party_id_imputation, d$party_id_lean)
table(d$party_id, d$party_id_lean)

## Feeling thermometer

d <- d %>%
  # rename
  rename("ft_democrats"="feeling_therm_2",
         "ft_republicans"="feeling_therm_1") %>%
  # create three measures of polarization
  # affective_polarization: absolute difference between feeling thermomether across parties
  # ingroup affective pol: feeling thermometer for ingroup
  # outgroup affective pol: feeling thermometer for outgroup
  mutate(affective_polarization=abs(ft_democrats-ft_republicans), 
        ingroup_affective_pol=case_when(party_id_imputation=="Democrat" ~ ft_democrats, 
                                        party_id_imputation=="Republican"~ft_republicans), 
        outgroup_affective_pol=case_when(party_id_imputation=="Democrat" ~ ft_republicans, 
                                        party_id_imputation=="Republican"~ft_democrats))

# check
cbind(d$party_id_imputation, d$ingroup_affective_pol, d$outgroup_affective_pol, d$affective_polarization)  

mean(d$ingroup_affective_pol, na.rm = TRUE)
mean(d$outgroup_affective_pol, na.rm = TRUE)
mean(d$affective_polarization)



# social media cynicism ---------------------------------------------------
glimpse(d)
# clean social media cynicism items. 
# higher values mean higher cynicism in the credibility of the information one 
# finds on social media

d <- d %>%
  # remove text from strongly agree / disagree
  mutate(across(sm_cynicism_1:sm_cynicism_3, 
                ~ str_replace_all(.x, "Strongly Agree", "4"))) %>%
  mutate(across(sm_cynicism_1:sm_cynicism_3, 
                ~str_replace_all(.x, "Strongly Disagree", "-4"))) %>%
  # convert to numeric
  mutate(across(sm_cynicism_1:sm_cynicism_3, 
                as.numeric))  


# build z-score index
sm_vars <- c("sm_cynicism_1",
             "sm_cynicism_2",
             "sm_cynicism_3") 

d <- d %>%
  # create zscore for each column
  mutate(across(all_of(sm_vars), center_scale)) %>% 
  # replace na iwth median
  mutate(across(all_of(sm_vars), ~ replace_na(.x, median(.x, na.rm=TRUE)))) %>% 
  # sum up the zcores
  zscores_index(sm_vars, "sm_cyn_zcore") %>%
  # pca
  tidy_pca(sm_vars, scores_name = "sm_cyn_pcaindex", alpha_name="sm_cyn_alpha") 


# high alpha
d$sm_cyn_alpha


# political cynicism -----------------------------------------------------------------------
table(d$pol_cyn_1)

# higher values means higher political cynicism
d <- d %>%
      mutate(pol_cyn_1=fct_relevel(pol_cyn_1, "Hardly any", "Not very many", "Quite a few"))



# Media Trust -------------------------------------------------------------

# recode: 
## ext_media_1 and ext_media_2 will be binary where 1 indicates trust in the media
## ext_media_3 will also be binary 

d %>% rowwise(email)

d <- d %>% 
  mutate(ext_media_1_recode=case_when(str_detect(ext_media_1, "Stops") ~ 1, 
                                      TRUE ~ 0), 
         ext_media_2_recode=case_when(str_detect(ext_media_2, "Deal fairly") ~ 1, 
                                      TRUE ~ 0),
         ext_media_3_recode=case_when(ext_media_3=="Never"|ext_media_3=="Once in a while"~1, 
                                      TRUE ~ 0)) %>%
  rowwise(email) %>%
  mutate(ext_media_score=sum(c_across(ext_media_1_recode:ext_media_3_recode), na.rm = TRUE)) %>%
  ungroup()

# no reason we should build z-scores for binary data, so we are simple using the rowsum here. 

glimpse(d)


# misinfo_belief ----------------------------------------------------------

table(d$misinfo_belief_1)

# true items: 6, 7, 8
# false items: 1, 2, 3, 4, 5

d <- d %>%
  # rename
  mutate(misinfo_false_1=misinfo_belief_1, 
         misinfo_false_2=misinfo_belief_2,
         misinfo_false_3=misinfo_belief_3, 
         misinfo_false_4=misinfo_belief_4, 
         misinfo_false_5=misinfo_belief_5, 
         misinfo_true_1=misinfo_belief_6, 
         misinfo_true_2=misinfo_belief_7, 
         misinfo_true_3=misinfo_belief_8) %>%
  mutate(across(misinfo_false_1:misinfo_false_5,
          ~ case_when(.x=="Not at all accurate" | .x=="Not very accurate" ~ 1, 
                      TRUE ~ 0))) %>%
  mutate(across(misinfo_true_1:misinfo_true_3,
         ~ case_when(.x=="Somewhat accurate" | .x=="Very accurate" ~ 0, 
                     TRUE ~ 1))) %>%
  rowwise() %>%
  mutate(false_news_accuracy=sum(misinfo_false_1,misinfo_false_2, misinfo_false_3, misinfo_false_4, misinfo_false_5, na.rm = TRUE), 
         true_news_accuracy=sum(misinfo_true_1, misinfo_true_2, misinfo_true_3,  na.rm = TRUE), 
         belief_accuracy=sum(false_news_accuracy, true_news_accuracy,  na.rm = TRUE)) %>%
  ungroup()



# fake news problem -------------------------------------------------------
names_fk = names(table(d$fake_news_problem))
order_fk = c(names_fk[[4]], names_fk[[2]], names_fk[[1]], names_fk[[3]])

#reorder
d <- d %>% 
  mutate(fake_news_problem = fct_relevel(fake_news_problem, order_fk)) 

tabyl(d, fake_news_problem)

# Fact-Checking Familiriaty -------------------------------------------------------------------------

# convert to binary
d <- d %>%
      mutate(fc_familiarity=case_when(fc_familiarity=="Yes" ~ 1,
                                      fc_familiarity=="No" ~ 0))
tabyl(d, fc_familiarity)

# Fact-Checking Trust -----------------------------------------------------
names_fctrust = names(table(d$fc_trust))
order_fc_trust = c(names_fctrust[[5]], names_fctrust[[4]], names_fctrust[[3]], 
                   names_fctrust[[2]], names_fctrust[[1]])

# reorder
d <- d %>%
      mutate(fc_trust=fct_relevel(fc_trust, order_fc_trust)) 

tabyl(d, fc_trust)

# misinfo literacy --------------------------------------------------------

d <- d %>%
  # convert all to numeric
  mutate(across(misinfo_literacy_1:misinfo_literacy_6,
                ~ case_when(.x=="Not at all likely" ~ "1", 
                            .x =="Very likely" ~ "7", 
                            TRUE ~ .x))) %>%
  mutate(across(misinfo_literacy_1:misinfo_literacy_6,
                as.numeric)) 

# build z-score index
ml_vars <- c("misinfo_literacy_1",
             "misinfo_literacy_2",
             "misinfo_literacy_3", 
             "misinfo_literacy_4",
             "misinfo_literacy_5",
             "misinfo_literacy_6") 

d <- d %>%
  # create zscore for each column
  mutate(across(all_of(ml_vars), center_scale)) %>% 
  # replace na iwth median
  mutate(across(all_of(ml_vars), ~ replace_na(.x, median(.x, na.rm=TRUE)))) %>% 
  # sum up the zcores
  zscores_index(ml_vars, "misinfo_literacy_zcore") %>%
  # pca
  tidy_pca(ml_vars, scores_name = "misinfo_literacy_pcaindex", alpha_name="misinfo_literacy_alpha") 

# unique
unique(d$misinfo_literacy_alpha)

# social media news trust -------------------------------------------------
table(d$sm_news_trust_1)

# rename
d <- d %>%
  rename("sm_news_trust_facebook"= "sm_news_trust_1",
         "sm_news_trust_twitter"= "sm_news_trust_2", 
         "sm_news_trust_instagram" ="sm_news_trust_3", 
         "sm_news_trust_youtube" = "sm_news_trust_4")


# build z-score index
smt_vars <- c("sm_news_trust_facebook",
             "sm_news_trust_twitter",
             "sm_news_trust_instagram", 
             "sm_news_trust_youtube") 

d <- d %>%
  # create zscore for each column
  mutate(across(all_of(smt_vars), center_scale)) %>% 
  # replace na iwth median
  mutate(across(all_of(smt_vars), ~ replace_na(.x, median(.x, na.rm=TRUE)))) %>% 
  # sum up the zcores
  zscores_index(smt_vars, "social_media_trust_zcore") %>%
  # pca
  tidy_pca(smt_vars, scores_name = "social_media_trust_pcaindex", alpha_name="social_media_trust_alpha") 

unique(d$social_media_trust_alpha)

# exp ---------------------------------------------------------------------

d <- d %>%
      mutate(exp=fct_relevel(exp, "control"))

# save --------------------------------------------------------------------

# csv
write_csv(d, here("data", "pilot", "pre_treatment", "pre_treatment_processed.csv.csv"))

# Rdata ~ preserves ordering in the cleaning
saveRDS(d, file = here("data", "pilot", "pre_treatment", "pre_treatment_processed.rds"))
