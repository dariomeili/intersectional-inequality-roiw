group_count <- function(.data, outcome, grouping, weights) {
  outcome <- deparse(substitute(outcome)) ##
  grouping <- deparse(substitute(grouping)) ##
  weights <- deparse(substitute(weights))
  
  df <- data.frame(group = as.factor(.data[[grouping]]))
  
  df %>% 
    distinct(group) %>% 
    nrow()
}
