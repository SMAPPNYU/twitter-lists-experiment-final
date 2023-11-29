##############################################################################
# File-Name: 01_clean_recruitment_survey.r
# Date: 2022
# author: Tiago Ventura
# Purpose: Main script to clean the data coming from the recruitment survey 
# Data in: "data/pilot/recruitment_raw.csv"
# Data out: "data/pilot/recruitment_processed.csv" - cleaned data
#           "data/pilot/recruitment_processed.csv.rds" rds format
# status: ongoing
# Machine: MacOS High Sierra
##############################################################################

# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, qualtRics, tidylog)


# open dataset ------------------------------------------------------------

d <- read_csv(here("data", "pilot","recruitment", "recruitment_raw_no_iid.csv"), na=c("", "NA", "-99"))

# Clean colnames ----------------------------------------------------------

# Remove allspaces, trim, and capital letters
d <- d %>% 
     clean_names()

# check 
dim(d)
colnames(d)

# 191 (184 + 7) participants went up to the app authorization and did not complete the survey. We will invite the participants among those who 
# complete the authorization and passed the quality filters
table(d$progress) 

# Filter preview and no consent -------------------------------------------
d <- d %>%
      slice(-1:-2) %>%
      mutate_at(vars(contains("date")),as.Date) %>% # converting to date
      mutate(duration_in_min=as.numeric(duration_in_seconds)/60) 

# remove participants who did not give consent.
d <- d  %>%  
  filter(str_detect(consent, "The purpose and nature of this research have been sufficiently explained")) # removing those who said no to the consent question
    


# filter pre-tests --------------------------------------------------------
d <- d %>%
      filter(!str_detect(email, "@nyu"))

# Statistics of the survey --------------------------------------------------------------

# median time
median(d$duration_in_min) # perfect

# quntiles
quantile(d$duration_in_min, probs=seq(0,1, by=.1))

# completes
table(d$finished)

# get emaild 
# d %>%
#   select(email) %>%
#   na.exclude() %>%
#   distinct() %>%
#   write_csv("data/pilot_email_informing_pause.csv")

# Convert all strings all to lower case -----------------------------------------------------
d <- d %>%
  mutate_if(is.character, str_to_lower)  # converting all to lower case

# Fix email variables -----------------------------------------------------
d <- d %>% 
  rename("email2"="q20") %>%
  mutate(email=str_trim(email), 
         email2=str_trim(email2))

# Recode variables-------------------------------------------------------------

## block demographics
colnames(d)


### age
# all categories
order_age <- names(table(d$age))


# Remove under 18
d <- d %>%
      filter(age!=order_age[[8]]) %>%
      mutate(age=ifelse(age==order_age[[7]], NA, age), 
             age=fct_relevel(age, order_age))

# check
table(d$age)


### gender
table(d$gender)

d <- d %>%
      mutate(gender=case_when(gender=="non-binary / third gender" ~ "other", 
                              gender=="prefer not to say" ~ NA_character_,
                              TRUE ~ gender), 
             gender=fct_relevel(gender, c("female", "male", "other")))


table(d$gender)

### race
d <- d %>%
      mutate(race=fct_relevel(race, "white"))
  
table(d$race)

### Education
order_educ <- names(table(d$education))
order_educ <- c(order_educ[5], order_educ[4],order_educ[7], order_educ[1], order_educ[2], order_educ[6], order_educ[3])

d<- d %>%
    mutate(education=fct_relevel(education, order_educ))

table(d$education)


# Block Politics ----------------------------------------------------------

#### how closely do you follow politics?
politics_order <- names(table(d$politics))
politics_order <- c(politics_order[2], politics_order[3],politics_order[1], politics_order[4] )

# reorder
d <- d %>%
      mutate(politics=fct_relevel(politics, politics_order))

table(d$politics)

##### which social media plat have an active social media account?
d %>% select(social_media_1)

d <- d %>%
      rename("active_facebook"="social_media_1", 
             "active_twitter"="social_media_2", 
             "active_tiktok"="social_media_3", 
             "active_instagram"="social_media_4", 
             "active_telegram"="social_media_5", 
             "active_whatsapp"="social_media_6", 
             "active_other"="social_media_7") %>%
      mutate_at(vars(contains("active")), ~ case_when(is.na(.x) ~ 0, 
                                              TRUE ~ 1))

# see binary
table(d$active_facebook)

# other
d <- d %>%
     rename("active_other_text"="social_media_7_text")


#### How frequently do you use them?
table(d$social_media_freq_1)
list_order <- c("at least 10 times a day",
                    "several times a day",
                    "about once a day",
                    "3 to 6 days a week",
                    "1 to 2 days a week",
                    "every few weeks",
                    "don’t know")
# reorder
d <- d %>%
  rename("usage_facebook"="social_media_freq_1", 
         "usage_twitter"="social_media_freq_2", 
         "usage_tiktok"="social_media_freq_3", 
         "usage_instagram"="social_media_freq_4", 
         "usage_telegram"="social_media_freq_5", 
         "usage_whatsapp"="social_media_freq_6", 
         "usage_other"="social_media_freq_7", 
         "usage_other_text"="social_media_freq_7_text") %>%
  mutate_at(vars(contains("usage_")), ~ fct_relevel(.x, list_order))

table(d$usage_facebook)

# Twitter usage -----------------------------------------------------------

##  Freqency Twitter usage
order_tw <- names(table(d$tw_freq))
order_tw <- c(order_tw[7], order_tw[6], order_tw[8], order_tw[2], order_tw[4], order_tw[1], order_tw[3], order_tw[5], order_tw[9])
order_tw

# order
d <- d %>%
      mutate(tw_freq=fct_relevel(tw_freq, order_tw), 
             tw_mobile_share=case_when(str_detect(tw_dev, "mobile phone shared")~ 1, 
                                       TRUE ~ 0))
## How do you use twitter
table(d$tw_freq)

d <- d %>% 
  rename("tw_usage_family_friends"="tw_usage_1", 
         "tw_usage_news"="tw_usage_2", 
         "tw_usage_sports"="tw_usage_3", 
         "tw_usage_interact_politicians"="tw_usage_4", 
         "tw_usage_politics"="tw_usage_5", 
         "tw_usage_share_my_opinion"="tw_usage_6") %>%
  mutate_at(vars(contains("tw_usage")), ~ fct_relevel(.x, list_order))


# follower
d <- d %>%
      mutate_at(vars(tw_followers, tw_following), 
                as.numeric)


# Join study --------------------------------------------------------------
d <- d %>%
      mutate(join_study_bin=ifelse(join_study=="yes, count me in!", 1, 0))


table(d$join_study_bin)

# Check quality of survey responses -----------------------------------------------------------

# length of the survey
table(round(d$duration_in_min, digits = 0))
d$duration_in_min_bin <- round(d$duration_in_min, digits = 0)

# median
median(d$duration_in_min)

# repeated email?
d %>%
  count(hash) %>%
  arrange(desc(n)) 

# function to identify and clean repeated emails, phone numbers, and ip_addresses
id_repeated <- function(data, variable, n_var_name, position_var_name){
  data %>% 
    group_by({{variable}}) %>%
    mutate("{{n_var_name}}":=n(), 
           "{{position_var_name}}":=1:n()) %>%
    ungroup()
}


# select repeates for cellphone, email and ipaddress
d <- d %>% 
  id_repeated(hash, n_email, position_email) %>%
  id_repeated(ip_address, n_ip_address, repeated_ip_addres) 

# qualtrics controls ------------------------------------------------------

# bot track: hidden java scrip
d <- d %>%
      mutate(bot_track=ifelse(is.na(bot_track), FALSE, TRUE))


# bot track: from qualtrics
d <- d %>%
      mutate(bot_qualtrics =case_when(q_recaptcha_score<0.5 ~ TRUE,
                                      TRUE ~ FALSE))

# q_relevant_id_duplicate: means the respondents is taking the survey multiple times
# q_relevant_id_duplicate_score: >75 means likely duplicate
# q_relevant_id_fraud_score: >30, likely a bot

d <- d %>%
  mutate(duplicate_qualtrics= case_when(q_relevant_id_duplicate==TRUE ~ TRUE,
                                        TRUE ~ FALSE), 
         duplicate_score_qualtrics=case_when(q_relevant_id_duplicate_score>75 ~ TRUE,
                                             TRUE ~ FALSE), 
         duplicate_id_fraud_score=case_when(q_relevant_id_fraud_score>30 ~ TRUE, 
                                         TRUE ~ FALSE))

# check if nas are sent to FALSE
sum(table(d$duplicate_score_qualtrics))==dim(d)[1]


# Procedure to screenout participants -------------------------------------

# step 1) when ip_address is repeated, select a random response across the repeated. 

uniques_ip_addres <- d %>% 
                      group_by(ip_address) %>%
                      slice_sample(n=1) %>%
                      ungroup() %>%
                      select(response_id) %>%
                      mutate(select_ip_address=TRUE) # means the participants we will invite

d <- left_join(d, uniques_ip_addres) %>%
      mutate(select_ip_address=ifelse(is.na(select_ip_address), FALSE, select_ip_address))

table(d$select_ip_address)

# step 2) when email is repeated with different ipaddress, also select a random respondent across the repeated emails

uniques_email <- d %>% 
                  filter(select_ip_address==TRUE) %>%
                  group_by(hash) %>%
                  slice_sample(n=1) %>%
                  ungroup() %>%
                  select(response_id) %>%
                  mutate(select_email=TRUE) # means the participants that we will invite

d <- left_join(d, uniques_email) %>%
        mutate(select_email=ifelse(is.na(select_email),
                                   FALSE, select_email))

# then apply all the qualtrics quality control filters.
d <- d %>%
  mutate(pass_quality_filters_qualtrics=case_when((bot_qualtrics==TRUE |
                                                  bot_track==TRUE |
                                                  duplicate_qualtrics==TRUE |
                                                  duplicate_score_qualtrics==TRUE) ~ FALSE, 
                                                  TRUE ~ TRUE), 
         pass_quality_filters_rep=case_when(select_ip_address==TRUE &  select_email==TRUE ~ TRUE, 
                                            TRUE ~ FALSE) )

## True in ip_address and false in select_email are participants who have the same email for different ip_addresses
table(d$select_ip_address, d$select_email)

# pass both filters
table(d$pass_quality_filters_qualtrics, d$pass_quality_filters_rep)

# create an variable with this outcome.
d <- d %>%
      mutate(pass_all_filters=case_when(pass_quality_filters_qualtrics==TRUE & pass_quality_filters_rep==TRUE ~TRUE, 
                                        TRUE ~ FALSE))

# convert email truncated to hash
d$email = d$hash

# Output ------------------------------------------------------------------

# saving as csv
write_csv(d, here("data", "pilot","recruitment", "recruitment_clean_data.csv"))

# saving as  Rdata ~ preserves ordering in the cleaning

saveRDS(d, file = here("data","pilot", "recruitment", "recruitment_clean_data.rds"))

