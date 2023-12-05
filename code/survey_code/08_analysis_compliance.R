##############################################################################
# File-Name: 08_analysis_compliance.r
# Date: 2023
# author: Tiago Ventura
# Purpose: Main script to generate descriptive analysis for compliance data
# Data in: "data/pilot/compliance/compliance_processed.rds" rds format
# status: ongoing
# Machine: MacOS High Sierra
##############################################################################
install.packages("pacman")
# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, qualtRics, 
               #tidylog,
               DeclareDesign, rsample, ggridges)

source(here("code", "survey_code", "utils_modeling.R"))

# open data ---------------------------------------------------------------

d = readRDS(file = here("data","pilot", "all_data_processed_processed.rds"))

# Create compliance measure -----------------------------------------------
d <- d %>%
  mutate(compliance_one_side=case_when(exp=="treatment" & w3_correct_fct_submissions>9 ~ 1,
                                       TRUE ~ 0))
table(d$compliance_one_side)

# Open compliance ---------------------------------------------------------
d_comp <- d %>%
  # remove those who attrit
  filter(!is.na(w3_response_id)) %>%
  select(exp, w3_compliance_all, w3_compliance_per_day, w3_correct_fct_submissions) %>%
  mutate_all(~ifelse(is.na(.x), 0, .x))

d$w3_compliance_all
d$w3_compliance_per_day
# Plot --------------------------------------------------------------------
treat <- wesanderson::wes_palette("BottleRocket2")[[3]]
control <- wesanderson::wes_palette("BottleRocket2")[[2]]


d_comp %>%
  select(exp, w3_compliance_all, w3_correct_fct_submissions) %>%
  mutate(exp=str_to_sentence(exp))  %>%
  pivot_longer(cols=-exp, 
              names_to = "names", 
              values_to = "values") %>%
  mutate(names=ifelse(names=="w3_compliance_all", "Any Compliance Tasks", 
                      "Link/Screenshot from \n Fact-Checking \n Organization")) %>%
  ggplot(aes(x= values, y=exp,fill=exp, color=exp)) +
  stat_density_ridges(quantile_lines = TRUE, 
                      quantiles = 4, 
                      alpha=.4) +
  scale_fill_manual(values=c(treat, control),
                    name="",
                    guide = guide_legend(reverse=TRUE, title.position="top")) +
  scale_color_manual(values=c(treat, control),
                     name="",
                     guide = guide_legend(reverse=TRUE, title.position="top")) +
  ylab("Density of Participants") +
  xlab("Number of Compliance Tasks Completed") +
  labs(title="", 
       subtitle="", 
       caption="Lines represent the first, the median, and third terciles for the distributions") +
  facet_grid(~names) +
  theme(axis.text.y = element_blank()) +
  xlim(0, 40)

save_function("treatment_takeup.png")

# anothe option
density <- d_comp %>%
  select(exp, w3_compliance_per_day) %>%
  mutate(exp=str_to_sentence(exp))  %>%
  pivot_longer(cols=-exp, 
               names_to = "names", 
               values_to = "values") %>%
  mutate(names=ifelse(names=="w3_compliance_per_day", "Any Compliance Tasks", 
                      "Link/Screenshot from \n Fact-Checking \n Organization")) %>%
  ggplot(aes(x= values, y=exp,fill=exp, color=exp)) +
  stat_density_ridges(quantile_lines = TRUE, 
                      quantiles = 4, 
                      alpha=.4) +
  scale_fill_manual(values=c(treat, control),
                    name="",
                    guide = guide_legend(reverse=TRUE, title.position="top")) +
  scale_color_manual(values=c(treat, control),
                     name="",
                     guide = guide_legend(reverse=TRUE, title.position="top")) +
  ylab("Density of Participants") +
  xlab("Number of Compliance Tasks Completed") +
  labs(title="", 
       subtitle="", 
       caption="Lines represent the first, the median, and third terciles for the distributions") +
  facet_grid(~names) +
  theme(axis.text.y = element_blank(), 
        legend.position = "bottom")

# hsitogram
d_comp %>%
  select(exp,w3_correct_fct_submissions) %>%
  mutate(exp=str_to_sentence(exp))  %>%
  pivot_longer(cols=-exp, 
               names_to = "names", 
               values_to = "values") %>%
  mutate(names=ifelse(names=="w3_compliance_per_day", "Any Compliance Tasks", 
                      "Link/Screenshot from \n Fact-Checking \n Organization")) %>%
  filter(exp=="Treatment") %>%
  ggplot(aes(x=values)) +
  geom_histogram(alpha=.4, 
                 fill=treat, 
                 color="black") +
  scale_fill_manual(values=c(treat, control),
                    name="",
                    guide = guide_legend(reverse=TRUE, title.position="top")) +
  scale_color_manual(values=c(treat, control),
                     name="",
                     guide = guide_legend(reverse=TRUE, title.position="top")) +
  ylab("Number of Participants") +
  xlab("Number of Tasks") +
  labs(title="", 
       subtitle="") +
  facet_grid(~names) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  theme(axis.text.y = element_blank(), 
        legend.position = "bottom")


# numbers total
d_comp %>%
  select(exp, w3_compliance_all, w3_correct_fct_submissions) %>%
  mutate(exp=str_to_sentence(exp)) %>%
  pivot_longer(cols=-exp, 
               names_to = "names", 
               values_to = "values") %>%
  group_by(names) %>%
  summarise(md=median(values, na.rm = TRUE), 
            m=mean(values, na.rm = TRUE), 
            sd=sd(values, na.rm = TRUE))

# How many below 10 tasks
d_comp %>%
  select(exp, w3_compliance_all, w3_correct_fct_submissions) %>%
  mutate(exp=str_to_sentence(exp)) %>%
  pivot_longer(cols=-exp, 
               names_to = "names", 
               values_to = "values") %>%
  mutate(below10=ifelse(values<10, TRUE, FALSE)) %>%
  tabyl(names, below10)  %>%
  adorn_totals("col")

1- 16/104

# komolgorov
ks.test(d_comp$w3_compliance_all[d$exp=="treatment"], d_comp$w3_compliance_all[d$exp=="control"])


# numbers treatment for fact_checking
d_comp %>%
  select(exp, w3_compliance_all, w3_correct_fct_submissions) %>%
  mutate(exp=str_to_sentence(exp)) %>%
  pivot_longer(cols=-exp, 
               names_to = "names", 
               values_to = "values") %>%
  group_by(exp, names) %>%
  summarise(md=median(values, na.rm = TRUE), 
            m=mean(values, na.rm = TRUE), 
            sd=sd(values, na.rm = TRUE))


d_comp %>%
  select(exp, w3_correct_fct_submissions) %>%
  mutate(exp=str_to_sentence(exp)) %>%
  pivot_longer(cols=-exp, 
               names_to = "names", 
               values_to = "values") %>%
  mutate(below10=ifelse(values<10, TRUE, FALSE)) %>%
  tabyl(exp, names, below10)  %>%
  adorn_totals("col")

