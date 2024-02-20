group_mean <- function(.data, outcome, grouping, weights) {
  outcome <- deparse(substitute(outcome)) ##
  grouping <- deparse(substitute(grouping)) ##
  weights <- deparse(substitute(weights))
  
  df <- data.frame(x = as.numeric(.data[[outcome]]), group = as.factor(.data[[grouping]]), w = as.numeric(.data[[weights]]))
  
  df %>% 
    group_by(group) %>% 
    summarise(means=stats::weighted.mean(x, w))
}
