##############################################################################
# File-Name: 05_clean_post_treatment_survey.r
# Date: 2023
# author: Tiago Ventura
# Purpose: Main script to clean the data coming from the post_treatment survey 
# Data in: "data/pilot/post_treatment/post_treatment_raw.csv"
# Data out: "data/pilot/post_treatment/post_treatment_processed.csv" - cleaned data
#           "data/pilot/post_treatment/post_treatment_processed.rds" rds format
# status: ongoing
# Machine: MacOS High Sierra
##############################################################################


# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, qualtRics, tidylog)

source(here("code", "survey_code", "utils.R"))

# open data set -----------------------------------------------------------

d_clean = read_csv(here("twitter-lists-experiment_gitcsmap", "data", "pilot", "post_treatment", "post_treatment_raw_no_iid.csv"))

# Clean colnames ----------------------------------------------------------

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
  # remove participants who started the post-treatment, but have not completed
  filter(finished==TRUE) 

# sanity check
length(unique(d$email))==dim(d)[[1]]

# cheating misinfo -------------------------------------------------------------------------

# convert to binary
d <- d %>%
  mutate(cheating_misinfo=case_when(cheating_misinfo=="Yes" ~ 1,
                                    cheating_misinfo=="No" ~ 0))
tabyl(d, cheating_misinfo)

# misinfo belief ----------------------------------------------------------

table(d$headlines_task_1)

# true items: 5:10
# false items: 1, 2, 3, 4, 5
# Don't Knows are already recoded as NA


d <- d %>%
  # rename
  mutate(misinfo_false_1=headlines_task_1, 
         misinfo_false_2=headlines_task_2,
         misinfo_false_3=headlines_task_3, 
         misinfo_false_4=headlines_task_4, 
         misinfo_false_5=headlines_task_5, 
         misinfo_true_1=headlines_task_6, 
         misinfo_true_2=headlines_task_7, 
         misinfo_true_3=headlines_task_8, 
         misinfo_true_4=headlines_task_9, 
         misinfo_true_5=headlines_task_10) %>%
  mutate(across(misinfo_false_1:misinfo_false_5,
                ~ case_when(.x=="Not at all accurate" | .x=="Not very accurate" ~ 1, 
                            TRUE ~ 0))) %>%
  mutate(across(misinfo_true_1:misinfo_true_5,
                ~ case_when(.x=="Somewhat accurate" | .x=="Very accurate" ~ 1, 
                            TRUE ~ 0))) %>%
  rowwise() %>%
  mutate(false_news_accuracy=sum(misinfo_false_1,misinfo_false_2, misinfo_false_3, misinfo_false_4, misinfo_false_5, na.rm = TRUE), 
         true_news_accuracy=sum(misinfo_true_1, misinfo_true_2, misinfo_true_3,  misinfo_true_4,  misinfo_true_4, na.rm = TRUE), 
         belief_accuracy=sum(false_news_accuracy, true_news_accuracy, na.rm = TRUE)) %>%
  ungroup() 


# fake news problem -------------------------------------------------------
names_fk = names(table(d$fake_news_problem))
order_fk = c(names_fk[[4]], names_fk[[2]], names_fk[[1]], names_fk[[3]])

#reorder
d <- d %>% 
  mutate(fake_news_problem = fct_relevel(fake_news_problem, order_fk)) 

tabyl(d, fake_news_problem)

# news trust -------------------------------------------------------------------------
# values
trust_values <- names(table(d$news_trust_1))

trust_values

# recode
trust_order <- c(trust_values[[2]], trust_values[[3]], trust_values[[4]], trust_values[[1]])

# recode all news trust measures

d <- d %>%
  # clean and recode
  mutate_at(vars(news_trust_1:news_trust_6),
            ~ fct_relevel(.x, trust_order)) %>%
  #rename collumns
  mutate(news_trust_newspapers=news_trust_1, # rename
         news_trust_social_media=news_trust_2, 
         news_trust_news_websites=news_trust_3, 
         news_trust_radio=news_trust_4, 
         news_trust_local_news=news_trust_5, 
         news_trust_national_news=news_trust_6)  %>%
  mutate(across(news_trust_newspapers:news_trust_national_news, 
                as.numeric, .names="{col}_num"))
# check
trust_vars  <- c("news_trust_newspapers_num",# rename
                 "news_trust_social_media_num", 
                 "news_trust_news_websites_num", 
                 "news_trust_radio_num", 
                 "news_trust_local_news_num", 
                 "news_trust_national_news_num")

# calculate trust z score index
d <- d %>%
  ungroup() %>%
  # create zscore for each column
  mutate(across(trust_vars, center_scale)) %>%
  # sum up the zcores
  zscores_index(trust_vars, "trust_zcore") %>% 
  # pca
  tidy_pca(trust_vars, scores_name = "trust_pcaindex", alpha_name="trust_alpha")   

# trust media -------------------------------------------------------------
# values
trust_values <- names(table(d$trust_media_3))

# recode
trust_order <- c(trust_values[[2]], trust_values[[3]], trust_values[[4]], trust_values[[5]], trust_values[[1]])

# recode all media trust measures

d <- d %>%
  # clean and recode
  mutate_at(vars(trust_media_1:trust_media_5),
            ~ fct_relevel(.x, trust_order)) %>%
  #rename collumns
  mutate(trust_fox=trust_media_1, # rename
         trust_cnn=trust_media_2, 
         trust_snopes=trust_media_3, 
         trust_politifact=trust_media_4, 
         trust_verify=trust_media_5)  %>%
  mutate(across(trust_fox:trust_verify, 
                as.numeric))


# social media usage ------------------------------------------------------
names_sm <- names(table(d$sm_usage_1))
order_sm <- c(names_sm[[5]], names_sm[1], names_sm[2], names_sm[3], names_sm[4], names_sm[6])


d <- d %>%
  # clean and recode
  mutate_at(vars(sm_usage_1:sm_usage_2),
            ~ fct_relevel(.x, order_sm)) %>%
  # rename
  rename(sm_usage_twitter=sm_usage_2, 
         sm_usage_facebook=sm_usage_1) 
         

tabyl(d, sm_usage_twitter)  


# party_id ----------------------------------------------------------------

# feeling thermomether ----------------------------------------------------

# notice I can regenerate these measures using the more complete party id 
# variable from the pre-treatment survey. 

d <- d %>%
  # rename
  rename("ft_democrats"="feeling_therm_2",
         "ft_republicans"="feeling_therm_1", 
         "ft_news"="feeling_therm_4",
         "ft_trump"="feeling_therm_5",
         "ft_biden"="feeling_therm_6") %>%
  # create three measures of polarization
  # affective_polarization: absolute difference between feeling thermomether across parties
  # affective_candidate_polarization: absolute difference between Trump and Biden
  # ingroup affective pol: feeling thermometer for ingroup
  # outgroup affective pol: feeling thermometer for outgroup
  mutate(affective_polarization=abs(ft_democrats-ft_republicans),
         affective_party_polarization=abs(ft_trump - ft_biden),
         ingroup_affective_pol=case_when(party_id=="Democrat" ~ ft_democrats, 
                                         party_id=="Republican"~ft_republicans), 
         outgroup_affective_pol=case_when(party_id=="Democrat" ~ ft_republicans, 
                                          party_id=="Republican"~ft_democrats))

# check
cbind(d$party_id, d$ingroup_affective_pol, d$outgroup_affective_pol, d$affective_polarization)  

mean(d$ingroup_affective_pol, na.rm = TRUE)
mean(d$outgroup_affective_pol, na.rm = TRUE)
mean(d$affective_polarization)

# conspiracy grid ---------------------------------------------------------
con_names <- names(table(d$conspiracies_4))
con_order <- c(con_names[5], con_names[3], con_names[1], con_names[2], con_names[4])


d <- d %>%
      mutate(across(c(conspiracies_1, conspiracies_3, conspiracies_4), ~ 
                      fct_relevel(.x, con_order)), 
             across(c(conspiracies_1, conspiracies_3, conspiracies_4), 
                    as.numeric,
                    .names = "{col}_num"))

is.na(d$conspiracies_1)
is.na(d$conspiracies_1)

# create zscore

cons_vars  <- c("conspiracies_1_num",# rename
                 "conspiracies_3_num", 
                 "conspiracies_4_num")

# calculate trust z score index
d <- d %>%
  # create zscore for each column
  mutate(across(cons_vars, center_scale)) %>%
  # sum up the zcores
  zscores_index(cons_vars, "cons_zcore") %>% 
  # pca
  tidy_pca(cons_vars, scores_name = "cons_pcaindex", alpha_name="cons_alpha")   


# misinfo literacy -------------------------------------------------------

table(d$misinfo_literacy_1)

# higher values means higher misinfo literacy
d <- d %>%
  # convert all to numeric
  mutate(across(misinfo_literacy_1:misinfo_literacy_6,
                ~ case_when(.x=="Not at all likely" ~ "1", 
                            .x =="Very likely" ~ "7", 
                            TRUE ~ .x))) %>%
  mutate(across(misinfo_literacy_1:misinfo_literacy_6,
                as.numeric, 
                .names = "{col}_num")) 


# build z-score index
ml_vars <- c("misinfo_literacy_1_num",
             "misinfo_literacy_2_num",
             "misinfo_literacy_3_num", 
             "misinfo_literacy_4_num",
             "misinfo_literacy_5_num",
             "misinfo_literacy_6_num") 

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


# # Testing manually if the z_score functions are  working as intendend
# 
# d$m1n = scale(as.numeric(d$misinfo_literacy_1))
# d$m2n = scale(as.numeric(d$misinfo_literacy_2))
# d$m3n = scale(as.numeric(d$misinfo_literacy_3))
# d$m4n = scale(as.numeric(d$misinfo_literacy_4))
# d$m5n = scale(as.numeric(d$misinfo_literacy_5))
# d$m6n = scale(as.numeric(d$misinfo_literacy_6))
# 
# d$mscore = d$m1n + d$m2n + d$m3n + d$m4n + d$m5n + d$m6n
# 
# d <- d %>%
#   rowwise() %>%
#   mutate(msscoreacross=sum(c_across(m1n:m6n)))
# 
# cbind(d$exp, d$mscore==d$misinfo_literacy_zcore, d$mscore, d$msscoreacross, d$misinfo_literacy_zcore)

# all same values

# social media cynicism ---------------------------------------------------
sm_names = names(table(d$sm_cynicism_1))
sm_order = list(sm_names[7],sm_names[2], sm_names[5], sm_names[3], sm_names[4], sm_names[1], sm_names[6]) 
sm_order

# clean social media cynicism items. 
# higher values mean higher cynicism in the credibility of the information one 
# finds on social media
d <- d %>%
  # remove text from strongly agree / disagree
  mutate(across(c(sm_cynicism_1, sm_cynicism_2, sm_cynicism_3), 
                ~ fct_relevel(.x, sm_order)) ,
  # convert to numeric
        across(c(sm_cynicism_1, sm_cynicism_2, sm_cynicism_3), 
                as.numeric, 
               .names="{col}_num"))  


# build z-score index
sm_vars <- c("sm_cynicism_1_num",
             "sm_cynicism_2_num",
             "sm_cynicism_3_num") 

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

# political cynicism ------------------------------------------------------
table(d$pol_cyn_1)
is.na(d$pol_cyn_1)

# higher values means higher political cynicism
d <- d %>%
  mutate(pol_cyn_1=fct_relevel(pol_cyn_1, "Hardly any", "Not very many", "Quite a few"), 
         pol_cyn_1_num=as.numeric(pol_cyn_1),
         pol_cyn_1_num=ifelse(is.na(pol_cyn_1_num), median(pol_cyn_1_num, na.rm = TRUE), pol_cyn_1_num))

# add one conspiracy item to pol_cyn
d <- d %>%
  mutate(conspiracies_2=fct_relevel(conspiracies_2, con_order),
          conspiracies_2_num=as.numeric(conspiracies_2))

# create zscore
polcyn_vars  <- c("conspiracies_2_num",# rename
                    "pol_cyn_1_num")

table(d$pol_cyn_1, d$conspiracies_2)

# calculate trust z score index
d <- d %>%
  # create zscore for each column
  mutate(across(polcyn_vars, center_scale)) %>%
  # sum up the zcores
  zscores_index(polcyn_vars, "polcyn_zcore") %>% 
  # pca
  tidy_pca(polcyn_vars, scores_name = "polcyn_pcaindex", alpha_name="polcyn_alpha")   


# ext media ---------------------------------------------------------------

# higher value means high trust in the

## for the main study, we should remove the randomization on the first ext_media question

## ext media 1 -> not at all and not too much trust in the news media recode as zero
## ext media 2 -> deal fairly with issues, recode as 1, trust in the media
## ext media 3 -> never or once in a while fabricate news, means high trust in the media

d <- d %>% 
  mutate(ext_media_1_recode=case_when(ext_media_1=="Not at all" ~ 0, 
                                      ext_media_1=="Not too much"~ 0.25,
                                      ext_media_1=="Some" ~ 0.75, 
                                      ext_media_1=="A lot"~ 1), 
         ext_media_2_recode=case_when(str_detect(ext_media_2, "Deal fairly") ~ 1, 
                                      TRUE ~ 0),
         ext_media_3_recode=case_when(ext_media_3=="Never" ~ 1, 
                                      ext_media_3=="Once in a while"~0.75,
                                      ext_media_3=="About half of the time" ~ .5 ,
                                      ext_media_3=="Most of the time"~0.25,
                                      ext_media_3=="All the time" ~ 0)) %>%
  rowwise(email) %>%
  mutate(ext_media_score=sum(c_across(ext_media_1_recode:ext_media_3_recode))) %>%
  ungroup()


# att_check ---------------------------------------------------------------
table(d$att_check)

d <- d %>% 
      mutate(att_check_pass=case_when(att_check=="President Obama" ~ 1, 
                                      TRUE ~ 0))
      
tabyl(d, att_check, att_check_pass)


# Fact-Checking Trust and share -----------------------------------------------------
names_fctrust = names(table(d$fc_trust))
order_fc_trust = c(names_fctrust[[5]], names_fctrust[[4]], names_fctrust[[3]], 
                   names_fctrust[[2]], names_fctrust[[1]])
# reorder
d <- d %>%
  mutate(across(c(fc_trust, fc_share), ~ fct_relevel(.x, order_fc_trust))) 

tabyl(d, fc_trust)
tabyl(d, fc_share, exp)



# social media trust ------------------------------------------------------

table(d$sm_news_trust_1)

# rename
d <- d %>%
  rename("sm_news_trust_facebook"= "sm_news_trust_1",
         "sm_news_trust_twitter"= "sm_news_trust_2", 
         "sm_news_trust_tiktok" ="sm_news_trust_3", 
         "sm_news_trust_youtube" = "sm_news_trust_4")


# build z-score index
smt_vars <- c("sm_news_trust_facebook",
              "sm_news_trust_twitter",
              "sm_news_trust_tiktok", 
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

# political efficacy ------------------------------------------------------
table(d$efficacy_3)

d <- d %>%
  # reorder
  mutate(across(efficacy_1:efficacy_3, 
                ~ fct_relevel(.x, c("Strongly disagree", 
                                    "Somewhat disagree", 
                                    "Neither agree nor disagree", 
                                    "Somewhat agree",
                                    "Strongly agree"))), 
         # convert to numeric
         across(efficacy_1:efficacy_3, 
                as.numeric, 
                .names="{col}_num"))  

# build z-score index
eff_vars <- c("efficacy_1_num",
              "efficacy_2_num",
              "efficacy_3_num") 

d <- d %>%
  # create zscore for each column
  mutate(across(all_of(eff_vars), center_scale)) %>% 
  # replace na iwth median
  mutate(across(all_of(eff_vars), ~ replace_na(.x, median(.x, na.rm=TRUE)))) %>% 
  # sum up the zcores
  zscores_index(smt_vars, "efficacy_zcore") %>%
  # pca
  tidy_pca(smt_vars, scores_name = "efficacy_pcaindex", alpha_name="efficacy_alpha") 

table(d$efficacy_1, d$efficacy_1_num)


# pol knowledge -----------------------------------------------------------

# convert to binary for correct responses
d <- d %>%
      mutate(pol_knowledge_1_bin=case_when(pol_knowledge_1=="Six years" ~ 1, 
                                        TRUE ~ 0), 
             pol_knowledge_2_bin=case_when(pol_knowledge_2=="Twice" ~ 1, 
                                            TRUE ~ 0)) %>%
  rowwise() %>%
  mutate(pol_know=sum(pol_knowledge_1_bin, pol_knowledge_2_bin)) %>%
  ungroup()

tabyl(d, pol_knowledge_1_bin, pol_knowledge_1)
tabyl(d, pol_knowledge_2_bin, pol_knowledge_2)


# info lookup -------------------------------------------------------------

d <- d %>%
      mutate(info_lookup=ifelse(info_lookup=="Yes", 1, 0))

table(d$info_lookup)


# Treatment Questions about the timeline ----------------------------------

## fc experience
names_fc = names(table(d$fc_experience))
order_fc = c(names_fc[1], names_fc[2], names_fc[5], names_fc[3], names_fc[4])

# reorder
d <- d %>%
      mutate(fc_experience=fct_relevel(fc_experience, order_fc))

table(d$fc_experience)

## fc bias
d <- d %>%
      mutate(across(fc_bias_1:fc_bias_4, 
                    ~ fct_relevel(.x, c("Strongly disagree", 
                                        "Somewhat disagree", 
                                        "Neither agree nor disagree", 
                                        "Somewhat agree",
                                        "Strongly agree"))))

## fc keep
table(d$fc_keep)

d <- d %>%
      mutate(fc_keep=case_when(fc_keep=="Not at all likely" ~ "1", 
              fc_keep=="Neither likely nor unlikely" ~ "4",                  
              fc_keep =="Extremely likely" ~ "7", 
              TRUE ~ fc_keep), 
             fc_keep=as.numeric(fc_keep))


# save  -------------------------------------------------------

# csv
write_csv(d, here("twitter-lists-experiment_gitcsmap", "data", "pilot", "post_treatment", "post_treatment_processed.csv.csv"))

# Rdata ~ preserves ordering in the cleaning
saveRDS(d, file = here("twitter-lists-experiment_gitcsmap", "data", "pilot", "post_treatment", "post_treatment_processed.rds"))

