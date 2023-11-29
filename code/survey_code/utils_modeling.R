# utils functions for modeling# Functions for modeling --------------------------------------------------

# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, scales,  knitr, srvyr, extrafont, 
               rebus, broom, tidyr, lubridate, here, ggdist, ggtext, ggalt,  janitor, 
               broom, tidyr,  stargazer, janitor)

# bootstrap resampling

bootstrapping_resampling <- function(data, N_simulation, group, sample_size){
  
  map_dfr(1:N_simulation, ~ 
            data %>% 
            group_by({{group}}) %>%
            sample_n(sample_size%/%2, replace=TRUE) %>%
            ungroup() %>%
            mutate(id_sample=.x)) %>%
    group_by(id_sample) %>%
    nest()
}

# calculating quantities from bootstrapping
bootstrapping_estimate <- function(data, columns){
  data %>%
    select(-data) %>%
    unnest({{columns}}) %>%
    group_by(term) %>%
    summarise(effect=mean(estimate, na.rm=TRUE), 
              lb95=quantile(estimate, probs=0.05), 
              ub95=quantile(estimate, probs=0.95))
  
  
}



# Functions ---------------------------------------------------------------

# means difference
func_t.test = function(depvar, data, model=FALSE){
  
  # separate treatment and control
  control <- data %>% filter(exp=="control") %>% pull({{depvar}})
  treatment <- data %>% filter(exp=="treatment") %>% pull({{depvar}})
  
  # t.test
  out = t.test(treatment, control)
  names(out$estimate) <- c("mean of treatment", "mean of control")
  return(out)
  
}

# ITT: ols
# inputs: dependent variable, data, and logical for model, 
# where false returns the full model, and true returns a dataframe with only the standardized treatment effects. 

# Lasso to select covariates
lasso_select_covariates <- function(data, cov, depvar){
  
  set.seed(1313)    
  
  # all pre-treatment covariates   
  covariates <- cov
    
    
    
    
                  # select all pre-treatment covariates  
                  d_covariates <- data  %>%
                    mutate_at(vars(covariates), ~ifelse(is.na(.x), median(as.numeric(.x), na.rm = TRUE), as.numeric(.x))) %>%
                    select(all_of(covariates))
                  
                  # select the outcome
                  outcomes <- data %>%  
                    select({{depvar}}) %>%
                    mutate_all(~ifelse(is.na(.x), median(as.numeric(.x), na.rm = TRUE), as.numeric(.x))) %>%
                    pull()
                  
                  # estimate lasso
                  library(glmnet)
                  model <- cv.glmnet(x=data.matrix(d_covariates),
                                     y = outcomes,
                                     alpha=1)
                  
                  coef.out <- coef(model, exact = TRUE)
                  indices <- which(coef.out != 0)
                  names_of_columns <- c(rownames(coef.out)[indices[-1]])
                  
                  return(names_of_columns)
}

# Covariate adjusted ITT: ols + controls
# inputs: dependent variable, data, and logical for model, where false returns the full model, and true returns a dataframe with only the standardized treatment effects. 

ols_covariates = function(depvar, data, add.vars, model=FALSE){
  
  # convert to numeric and impute missings
  data <- data  %>%
    mutate_at(vars(add.vars), ~ifelse(is.na(.x), median(as.numeric(.x), na.rm = TRUE), as.numeric(.x)))
  
  
  if(model==TRUE){
    model.expr = as.formula(paste(depvar, " ~ exp +",
                                  paste(add.vars, collapse = "+")))
    
    out = lm_robust(model.expr, data = data, se_type="HC0")
    return(out)
  }    
  
  model.expr = as.formula(paste(depvar, " ~ exp +",
                                paste(add.vars, collapse = "+")))
  
  out = lm_robust(model.expr, data = data, se_type = "HC0")
  
  # sd control
  sd_control <- data %>% 
    filter(exp=="control") %>%
    pull(depvar) %>%
    sd(., na.rm=TRUE)
  
  # tidy
  out <- out %>% 
    tidy() %>%
    filter(term=="exptreatment") %>%
    mutate_at(vars(c(estimate, std.error)), 
              ~.x/sd_control)
  
  return(out)
}


# CACE
# inputs: dependent variable, data, and logical for model, where false returns the full model, and true returns a dataframe with only the standardized treatment effects. 
func_cace = function(depvar, data, add.vars, covariates=FALSE,  model=FALSE){
  data <- data  %>%
    mutate_at(vars(add.vars), ~ifelse(is.na(.x), median(as.numeric(.x), na.rm = TRUE), as.numeric(.x)))
  
  if(model==TRUE & covariates==FALSE){ 
    
    # without covariates
    model.expr = as.formula(paste(depvar, " ~ compliance_one_side | exp "))
    
    # model
    out = iv_robust(model.expr, data = data) 
    return(out)
  }
  
  else if(model==TRUE & covariates==TRUE) {
    # with covariates
    model.expr = as.formula(paste(depvar, " ~ compliance_one_side + " ,paste(add.vars, collapse = "+") , 
                                  "| exp  + ", 
                                  paste(add.vars, collapse = "+")))
    out = iv_robust(model.expr, data = data) 
    return(out)
    
  } else if(model==FALSE & covariates==FALSE){
    
    model.expr = as.formula(paste(depvar, " ~ compliance_one_side | exp "))
    
    
    # model
    out = iv_robust(model.expr, data = data) 
    
    # sd control
    sd_control <- data %>% 
      filter(exp=="treatment") %>%
      pull(depvar) %>%
      sd(., na.rm=TRUE)
    
    # tidy
    out <- out %>% 
      tidy() %>%
      mutate_at(vars(c(estimate, std.error)), 
                ~.x/sd_control)
    return(out)
    
    
  } else if(model==FALSE & covariates==TRUE) {
    
    
    model.expr = as.formula(paste(depvar, " ~ compliance_one_side + " ,paste(add.vars, collapse = "+") , 
                                  "| exp  + ", 
                                  paste(add.vars, collapse = "+")))
    
    # model
    out = iv_robust(model.expr, data = data) 
    
    # sd control
    sd_control <- data %>% 
      filter(exp=="treatment") %>%
      pull(depvar) %>%
      sd(., na.rm=TRUE)
    
    # tidy
    out <- out %>% 
      tidy() %>%
      mutate_at(vars(c(estimate, std.error)), 
                ~.x/sd_control) %>%
      filter(term=="compliance_one_side")
    return(out)
  }
}



# ggplot theme for the paper ------------------------------------------------------------
my_font <- "Palatino"
my_bkgd <- "white"
#my_bkgd <- "#f5f5f2"
pal <- RColorBrewer::brewer.pal(9, "Spectral")
colorwes <-  wesanderson::wes_palette("BottleRocket2")[[3]]

my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd),
                  plot.background = element_rect(fill = my_bkgd),
                  panel.background = element_rect(fill = "gray94"),
                  panel.border = element_rect(color="transparent"),
                  panel.grid = element_blank(),
                  strip.background = element_rect(fill="gray94", color="transparent"), 
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(size = 4, fill = "white", colour = NA), 
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 12, family = my_font),
                  legend.title = element_text(size=16, family=my_font, face="bold"),
                  plot.title = element_markdown(size = 22, face = "bold", family=my_font),
                  plot.subtitle = element_markdown(size=16, family=my_font),
                  axis.title= element_text(size=22),
                  axis.text = element_text(size=12, family=my_font),
                  axis.title.x = element_text(hjust=1),
                  strip.text = element_text(family = my_font, 
                                            color = "#22211d",
                                            size = 16), 
                  plot.margin = margin(1, 1, 1, 1, "cm"), 
                  plot.caption = element_text(size=12, 
                                              family=my_font,
                                              hjust = 0.5, 
                                              face="italic"))

theme_set(theme_bw() + my_theme)

## ggpoint
plot_model <- function(output, 
                       models,
                       dep_var_label,
                       colorvalues="black",
                       data,
                       xlab="", ylab="", 
                       title="", subtitle="", caption="", style=1){
  
  # first tidy
  tidydata <- pmap(list(output, 
                        models, 
                        dep_var_label), 
                   function(data, model, dv) 
                     data %>%
                     mutate(model=model) %>%
                     #filter(term=="exptreatment"|term=="compliance_one_side") %>%
                     # mutate(up90=estimate + 1.64*std.error,
                     #        up95=estimate + 1.96*std.error, 
                     #        lb90=estimate - 1.64*std.error, 
                     #        lb95=estimate - 1.96*std.error) %>%
                     mutate(dv=dv)) %>%
    bind_rows()  %>%
    mutate(model=fct_rev(fct_inorder(model)), 
           dv=fct_inorder(dv))
  
  
  
  
  
  # graph
  if(style==1){
    
    # graph
    colorvalues="black"
    
    # graph with a unique DV
    ggplot(tidydata,
           aes(y=model, 
               x=effect, 
               color=model,
               label=round(effect, 2))) +
      # geom_errorbar(aes(xmin=lb90, 
      #                   xmax=up90), 
      #               size=1, width=0, alpha=.8, 
      #               position=position_dodge(width = 1), 
      #               color=colorvalues) +
      geom_errorbar(aes(xmin=lb95, 
                        xmax=ub95),  
                    size=.8, width=.1, alpha=.8, 
                    position=position_dodge(width = .8), 
                    color=colorvalues) +
      geom_point(size=14, shape=21, fill="white",
                 position=position_dodge(width = .6), 
                 stroke=1, 
                 color=colorvalues, alpha=1) +
      geom_text(
        fontface = "italic", 
        fill="white", 
        color = "black",
        size=4, 
        position = position_dodge(.6)) +
      geom_vline(aes(xintercept=0), linetype="dashed", color="black", alpha=.8) +
      ylab(ylab) +
      xlab(xlab) +
      labs(title=title, 
           subtitle=subtitle, 
           caption=caption) +
      facet_grid(~dv) +
      theme(legend.position = "none") 
  }
  else{
    # graph with a unique DV
    ggplot(tidydata,
           aes(y=dv, 
               x=effect, 
               color=model,
               fill=model,
               label=round(effect, 2))) +
      geom_errorbar(aes(xmin=lb95, 
                        xmax=ub95),  
                    size=2, width=0, alpha=.8, 
                    position=position_dodge(width = .2)) +
      geom_point(size=14, shape=21, fill="white",
                 position=position_dodge(width = .2), 
                 stroke=1, alpha=1) +
      geom_text(
        fontface = "italic", 
        fill="white", 
        color = "black",
        size=4, 
        position = position_dodge(.2)) +
      geom_vline(aes(xintercept=0), linetype="dashed", color="black", alpha=.8) +
      ylab(ylab) +
      xlab(xlab) +
      labs(title=title, 
           subtitle=subtitle, 
           caption=caption) +
      theme(legend.position = "bottom")  +
      scale_color_manual(values=colorvalues, name="Model:")
  }
}



# Function to save graph outputslocally and on overleaf ----------------------------------
save_function <- function(name, 
                          output_ov="~/Dropbox/Apps/Overleaf/Fact_Checking_Timeline_Experiment/output"){
  
  
  ggsave(filename = here("output","pilot", name), 
         width = 12, height = 8, units = "in", 
         pointsize = 12, bg = "white")
  
  ggsave(filename = paste0(output_ov, "/", name), 
         width = 12, height = 8, units = "in", 
         pointsize = 12, bg = "white")
}
