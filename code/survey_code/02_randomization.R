##############################################################################
# File-Name: randomization.r
# Date: 2022
# author: Tiago Ventura
# Purpose: Main script to merge tokens + surver and randomize participants 
# Data in:  "data/pilot/recruitment_processed.rds" - survey
#            "data/pilot/pilot_I_tokens.csv" - tokens
# status: ongoing
# Machine: MacOS High Sierra
##############################################################################

# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, qualtRics, tidylog, 
               randomizr)

# open data ---------------------------------------------------------------
ds <- read_rds("data/pilot/recruitment/recruitment_clean_data.rds") %>% mutate(survey=1)


# this contains users tokens on Twitter which we are not allowed to share. 
tokens <- read_csv("data/pilot/_misc/pilot_II_tokens.csv")

dim(tokens)

# Keep only unique profiles -----------------------------------------------
dm <- tokens %>%
      rename("response_id"="userid") %>%
      mutate(response_id=str_to_lower(response_id)) %>%
      left_join(ds)

# How many successful matches -------------------------------------------------------------------------

dm %>% tabyl(survey) # eight from the app were not matched in the survey. Likely our internal testing. 

tokens %>%
  rename("response_id"="userid") %>%
  mutate(response_id=str_to_lower(response_id), 
         tokens=TRUE) %>%
  anti_join(ds) %>%
  pull(profileId) %>%
  as.character()


# How many completed the survey but did not finished the app auth ---------
tokens %>%
  rename("response_id"="userid") %>%
  mutate(response_id=str_to_lower(response_id), 
         tokens=TRUE) %>%
  right_join(ds) %>%
  tabyl(tokens, finished)

# 320 completed the survey but did not finished the auth (likely the glitch from not having an extra question after the auth)

# 217 did not completed the survey and did not finish the auth

## 88 completed the authorization but did not finished the survey. it is fine because we have their tokens and emails. 

## 198 completed both



# Add other screening questions -------------------------------------------

# twitter usage
dm <- dm %>%
        mutate(screen_out_tw_usage=case_when(tw_freq=="i don't have an account on twitter" |
                                         tw_freq=="i do not check my twitter every day" |
                                         tw_freq=="less than 10 minutes" ~ FALSE, 
                                       TRUE ~ TRUE)) 

# Time to complete among those who finished
quantile(ds$duration_in_min, probs=seq(0,1, .1)) # remove 10% fasters <2 min

#  how many
sum(dm$duration_in_min < 1, na.rm = TRUE) # remove 9 speeders


# create
dm  <- dm %>%
       mutate(screen_out_speeding = case_when(duration_in_min < 1 ~ FALSE, 
                                              TRUE ~ TRUE))


# Select all participants who will be invited -----------------------------
dm <- dm %>%
      mutate(invite_study=case_when(screen_out_speeding==TRUE &
                                      screen_out_tw_usage==TRUE & 
                                      pass_all_filters==TRUE ~ TRUE, 
                                    TRUE~ FALSE))

table(dm$invite_study)

# Block randomization -----------------------------------------------------
set.seed(1310)
table(dm$age)
table(dm$education)
table(dm$gender)
table(dm$race)

# block on education, age and gender
educ = names(table(dm$education))
age_ = names(table(dm$age))
gender_ = names(table(dm$gender))
race = names(table(dm$race))


# create the blocks
d_block <- dm %>%
  filter(invite_study==TRUE) %>%
  select(age, education, race,  gender) %>%
  mutate(educ_rec=case_when(education==educ[[1]] | education==educ[[2]]  ~ 1, 
                            education==educ[[3]] | education==educ[[4]] | education==educ[[5]]~ 2, 
                            education==educ[[6]] | education==educ[[7]] ~ 3),
         age_recode=case_when(age==age_[[1]] | age==age_[[2]] ~ 1, 
                              age==age_[[3]] | age==age_[[4]] ~ 2, 
                              age==age_[[5]] | age==age_[[6]] ~ 3), 
         gender_recode=case_when(gender==gender_[[1]] | gender==gender_[[3]]~1, 
                                 TRUE ~ 2), 
         race_recode=case_when(race==race[[1]] ~ 1, 
                               TRUE ~ 2))

table(d_block$gender_recode, d_block$gender)

# put ids in each block and remove repetitions
blocks <- d_block %>%
  select(gender_recode, educ_rec, age_recode, race_recode) %>% 
  drop_na() %>%
  distinct() %>%
  rowid_to_column(var="blocks")

# merge the blocks back
blocks_to_merge <- left_join(d_block, blocks) %>%
  select(age, education, gender, race, blocks) %>%
  drop_na() %>%
  distinct()

table(blocks_to_merge$blocks)

# merge with the main data
dm <- left_join(dm, blocks_to_merge) %>%
  mutate(blocks_recode=case_when(is.na(blocks) ~ 21, 
                                 TRUE ~ as.numeric(blocks))) 
sum(is.na(dm$q_gender))

dm$blocks_recode

# get randomization
colnames(dm)

selection <- dm %>%
  filter(invite_study==TRUE) %>%
  select(access_secret, access_token, response_id, profileId, email, blocks_recode, age, gender, education, race) %>%
  # randomization here
  mutate(exp=block_ra(blocks_recode, prob = 0.5, conditions = c("control", "treatment")))

# check quick balance
selection %>%
  group_by(exp) %>%
  summarise(m=mean(as.numeric(age), na.rm=TRUE), 
            g=mean(as.numeric(as.factor(gender)), na.rm=TRUE), 
            e=mean(as.numeric(education), na.rm=TRUE), 
            r=mean(as.numeric(as.factor(race)), na.rm=TRUE))

table(selection$blocks_recode, selection$exp)

table(selection$age, selection$exp)

# add ids so that we access the surveys as well. This is necessary because I will upload
# this csv directly to qualtrics. 
selection <- selection  %>%
  bind_rows(tibble(email=c("control1@gmail.com", "control2@gmail.com", "treatment1@gmail.com", "treatment2@gmail.com"),
                   exp=c("control", "control", "treatment", "treatment")))

# block randomization
selection_to_qualtrics <- selection %>% 
  select(Email=email,exp, id_crosswalk=response_id)

# selection to save
colnames(selection)
selection_to_pin = selection %>% select(-age, -gender, -blocks_recode, -education)


# save --------------------------------------------------------------------


##### never run this ever again. 
##### Save randomization in a secure folder 
#write_csv(selection_to_pin, here("data","pilot", "secure_data", "randomization_for_pinning.csv"))
#write_csv(selection_to_qualtrics, here("data","pilot", "secure_data", "randomization_for_qualtrics.csv"))



# Predicting assignment ---------------------------------------------------
d <- selection

d <- d %>%
      mutate(exp_num=ifelse(exp=="treatment", 1, 0)) %>%
      sample_n(1200, replace=TRUE)

colnames(d)
m1 <- lm_robust(exp_num ~ blocks_recode, data=d) %>% tidy() %>% mutate(model=1)
m2 <- lm_robust(exp_num ~ blocks_recode + age, data=d)   %>% tidy() %>% mutate(model=2)
m3 <- lm_robust(exp_num ~ blocks_recode + age + education, data=d )  %>% tidy()%>% mutate(model=3)
m4 <- lm_robust(exp_num ~ blocks_recode +  age + education + race, data=d )  %>% tidy()%>% mutate(model=4)
m5 <- lm_robust(exp_num ~ blocks_recode +  age + education + race + gender, data=d )  %>% tidy()%>% mutate(model=5)


m <- bind_rows(m1, m2, m3, m4, m5)
colnames(m)

ggplot(m,
       aes(y=term, 
           x=estimate,
           label=round(estimate, 2))) +
  geom_errorbar(aes(xmin=conf.low, 
                    xmax=conf.high),  
                size=.8, width=.1, alpha=.8, 
                position=position_dodge(width = .8), 
                color="black") +
  geom_point(size=14, shape=21, fill="white",
             position=position_dodge(width = .6), 
             stroke=1, 
             color="black", alpha=1) +
  geom_text(
    fontface = "italic", 
    fill="white", 
    color = "black",
    size=4, 
    position = position_dodge(.6)) +
  geom_vline(aes(xintercept=0), linetype="dashed", color="black", alpha=.8) +
  facet_grid(~model, scales = "free_y")

