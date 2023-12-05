# utils functions

# Basics ------------------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, here, janitor, 
               rebus, RColorBrewer, wesanderson, psych)

# function to calculate pca in a tidyverse style
tidy_pca <- function(data, var_input, scores_name, alpha_name){
  input = data[var_input]  
  #PCA_results <- principal(input, nfactors = 1)
  #as.numeric(PCA_results$scores)
  alpha = psych::alpha(input, 'check.keys=TRUE')$total$std.alpha 
  prcomp<- prcomp(input)
  scores <- prcomp$x[,1]
  bind_cols(data,as_tibble_col(scores, column_name = scores_name), as_tibble_col(alpha, column_name = alpha_name))
}

# function to calculate z score
center_scale <- function(x) {
  out = scale(x, scale = TRUE)
  as.numeric(out[,1])
}

# function to build index from zscores in a tidyverse style
zscores_index <- function(data, vars, output){
  score = data %>%
    rowwise() %>%
    mutate(score=sum(c_across(vars))) %>%
    ungroup() %>%
    pull(score)
  
bind_cols(data, as_tibble_col(score, column_name = output))
}

