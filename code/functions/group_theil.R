group_theil <- function(.data, outcome, grouping, weights) {
    outcome <- deparse(substitute(outcome)) ##
    grouping <- deparse(substitute(grouping)) ##
    weights <- deparse(substitute(weights))
    
    df <- data.frame(x = as.numeric(.data[[outcome]]), group = as.factor(.data[[grouping]]), w = as.numeric(.data[[weights]]))
    
  xMean_group <- df %>% 
      group_by(group) %>% 
      summarise(means=stats::weighted.mean(x, w)) %>% 
      pull(means)
  
  mean_x <- mean(xMean_group)
  
  n_cat <-  df %>% 
    distinct(group) %>% 
    nrow()
  
  sum((xMean_group/mean_x)*asinh(xMean_group/mean_x))/n_cat
  }
 