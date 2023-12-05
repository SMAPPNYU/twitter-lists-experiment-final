# random code
# random code
quantile(unlist(map(l, "estimate"))) 

l = map(d_s$data, ~ func_ols_base(.x, depvar) )
d_s$data[[1]]$
  
  true_true_itt <- func_ols_base(depvar, data=d); true_true_itt
true_true_ittc <- func_ols_covariates(depvar,d); true_true_ittc
true_true_cace <- func_cace(depvar, data=d); true_true_cace


# plot all together
output= list(false_false_itt, false_false_ittc, false_false_cace,
             true_true_itt, true_true_ittc, true_true_cace)

models= list("ITT", "Cov-ITT", "CACE", "ITT", "Cov-ITT", "CACE")
dep_var_label= unlist(list(rep("False Rumors Accuracy", 3), rep("True News Accuracy", 3)))

# plot
plot_model(output, models, dep_var_label,
           title="B",
           caption = "Standardized Treatment Effects \n on Belief Accuracy on False and True Headlines")


d
library(modelsummary)
cov = c("w1_age", "w1_gender", "w1_race", "w1_education", "w1_politics", 
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
        "w2_ideo_place_self")

lm(w3_belief_accuracy ~ exp, data=d)
lm(w3_belief_accuracy ~ exp + w1_age+w1_race+w1_education+w1_politics+w1_usage_twitter+
     +      w1_tw_following+w2_dl_zcore+w2_trust_zcore+w2_belief_accuracy, data=d) %>%
  summary()

add.vars
datasummary_balance(~exp,
                    data =  d %>% 
                      group_by(exp) %>%
                      sample_n(2000, replace = TRUE) %>% 
                      select(cov, exp) %>% mutate_at(vars(cov), as.numeric),
                    title = "",
                    notes = "", 
                    dinm_statistic = "p.value",
                    fmt=2)


ds <- d %>% bootstrapping_resampling(2000, exp, 800)

f <- d %>% 
  sample_n(100, replace=TRUE) %>%
  mutate(model=map(data, ~ lm_robust(as.numeric(as.factor(exp)) ~w1_age+w1_race+w1_education+w1_politics+w1_usage_twitter+
                                       +      w1_tw_usage_sports+w1_tw_usage_interact_politicians+
                                       +      w1_tw_following+w2_dl_zcore+w2_trust_zcore+w2_ideo_place_self+
                                       +      w2_affective_polarization+w2_ingroup_affective_pol+
                                       +      w2_sm_cyn_zcore+w2_ext_media_score+w2_belief_accuracy+
                                       +      w2_fc_familiarity+w2_fc_trust+w2_misinfo_literacy_zcore+
                                       +      w2_social_media_trust_zcore, data=.x %>% mutate_all(as.numeric)) %>% tidy())) %>%
  select(-data) 

f %>% 
  unnest(model)  %>%
  group_by(term) %>%
  summarize(m=mean(p.value, na.rm=TRUE))  %>% 
  arrange(m) %>%
  filter(m<0.10)

d %>% 
  sample_n(100, replace=TRUE) %>%
  lm_robust(as.numeric(as.factor(exp)) ~w1_age, data=.) %>% tidy()

#ols_covariates(depvar, ds)
lm_robust(w3_belief_accuracy ~ exp, data = ds, se_type = "stata")

lm_robust(as.formula(paste(depvar, " ~ exp +",
                           paste(add.vars, collapse = "+"))), 
          data=ds)

lm_robust(as.formula(paste("exp", " ~ ",
                           paste(add.vars, collapse = "+"))),
          data=d %>% mutate(exp=as.numeric(as.factor(exp))))


lm(as.numeric(as.factor(exp)) ~w2_dl_zcore, data=ds) %>% summary()




d$w1_tw_usage_interact_politicians
class(ds$exp)

func_ols_base(depvar, ds)
ols_covariates(depvar, ds, add.vars)
dep
add.vars = lasso_select_covariates(ds, depvar)
add <- paste0(add.vars, collapse = "+")
bootstraps(ds,times = 2000, apparent = TRUE) %>%
  mutate(models=map(splits, ~ ols_covariates(depvar, data=analysis(.x), add.vars=add.vars))) %>%
  int_pctl(models)


resample2 %>%
  dplyr::select(-splits) %>%
  unnest() %>%
  dplyr::select(id,estimate)

res = map(l, ~ func_ols_base(depvar, data=.x))


p_ints <- int_pctl(resample2, models)
