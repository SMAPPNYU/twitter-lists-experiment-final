##############################################################################
# File-Name: 06_crosswalk_all_waves.r
# Date: 2023
# author: Tiago Ventura
# Purpose: script to combine data coming from each wave
# Data in: data/pilot/recruitment/recruitment_processed.rds rds format
#           data/pilot/pre_treatment/pre_treatment_processed.rds" rds format
#           data/pilot/compliance/compliance_processed.rds" rds format
#           data/pilot/post_treatment/compliance_processed.rds" rds format
# Data out: "data/pilot/all_data_processed.csv" - cleaned data
#           "data/pilot/all_data_processed_processed.rds" rds format
# status: ongoing
# Machine: MacOS High Sierra
##############################################################################


# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, qualtRics, tidylog)



# open recruitment --------------------------------------------------------

# vars I will not use in the analysis
vars_to_remove <- c("start_date", "end_date", "status", "ip_address", "progress", "recorded_date", 
                    "recipient_last_name" ,  "recipient_first_name" , 
                    "distribution_channel" , 
                    "user_language", 
                    "q_recaptcha_score", 
                    "external_reference", 
                    "location_latitude",
                    "location_longitude", 
                    "q_relevant_id_duplicate", 
                    "q_relevant_id_duplicate_score", 
                    "q_relevant_id_fraud_score" , 
                     "q_relevant_id_last_start_date", 
                    "bot_track", 
                    "n_ip_address", 
                    "repeated_ip_addres",
                    "bot_qualtrics",                 
                    "duplicate_qualtrics" ,
                    "duplicate_score_qualtrics",
                    "duplicate_id_fraud_score"  ,    
                    "select_ip_address" ,
                    "select_email" , 
                    "fb_submit"  , 
                    "browser"  ,
                    "device_type" ,                  
                    "platform"  ,
                    "referer" , 
                    "q_ballot_box_stuffing", 
                    "n_email", 
                    "position_email", 
                    "pass_quality_filters_qualtrics",
                    "pass_quality_filters_rep", 
                    "pass_all_filters")

# open data
recruitment = readRDS(file = here("data","pilot", "recruitment", "recruitment_clean_data.rds")) %>%
                select(!vars_to_remove) %>%
                select(email, response_id, everything()) %>%
                select(!contains("_do_"))


glimpse(recruitment)

# add the wave id in the column names
# w1 stands for recruitment
recruitment<- recruitment %>%
  # rename all but email
  rename_at(vars(3:ncol(.)), ~str_c("w1_", .x))


# open all participants included in the randomization ------------------------------------------

randomization <- read_csv(here("data","pilot", "secure_data", "randomization_for_pinning_no_iid.csv")) %>%
                  filter(!str_detect(email, "control|treatment"))

recruitment_randomization <- inner_join(recruitment, randomization %>% select(email=RecipientEmail, exp, response_id))

# unique?
length(unique(recruitment_randomization$email))==dim(recruitment_randomization)[1]

# open pre_treatment ------------------------------------------------------
pre_treatment = readRDS(file = here( "data","pilot", "pre_treatment", "pre_treatment_processed.rds")) %>%
  select(!any_of(vars_to_remove)) %>%
  select(email, exp, everything()) %>%
  select(!contains("_do_"))



# add the wave id in the column names
# w2 stands for the pre-treatment survey
pre_treatment<- pre_treatment %>%
  # rename all but email
  rename_at(vars(3:ncol(.)), ~str_c("w2_", .x))


# join with recruitment
glimpse(pre_treatment)
glimpse(recruitment)

# here I merge left with pre_treatment. It keep only the participants who entered in the experiment
recruitment_randomization_prt <- left_join(pre_treatment, recruitment_randomization)


# open compliance ---------------------------------------------------------
c <-readRDS(file = here( "data","pilot", "compliance", "compliance_clean_data.rds")) %>%
      select(!any_of(vars_to_remove)) %>%
      select(email, exp, everything()) %>%
      select(!contains("_do_"))

# add the wave id in the column names
# w2 stands for the pre-treatment survey
c<- c %>%
  # rename all but email
  rename_at(vars(3:ncol(.)), ~str_c("w3_", .x))

recruitment_randomization_prt_c <- left_join(recruitment_randomization_prt, c)


# open final --------------------------------------------------------------

f <-readRDS(file = here("data","pilot", "post_treatment", "post_treatment_processed.rds")) %>%
  select(!any_of(vars_to_remove)) %>%
  select(email, exp, everything()) %>%
  select(!contains("_do_"))


f %>%
  group_by(exp) %>%
  summarize(m=mean(belief_accuracy),
            f=mean(false_news_accuracy), 
            t=mean(true_news_accuracy))


# add the wave id in the column names
# w2 stands for the pre-treatment survey
f<- f %>%
  # rename all but email
  rename_at(vars(3:ncol(.)), ~str_c("w3_", .x))



all <- left_join(recruitment_randomization_prt_c, f)


# save --------------------------------------------------------------------

# saving as csv
write_csv(all, here("data", "pilot", "all_data_processed_processed.csv"))

# saving as  Rdata ~ preserves ordering in the cleaning

saveRDS(all, file = here( "data","pilot", "all_data_processed_processed.rds"))


