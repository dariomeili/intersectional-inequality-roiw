group_n <- function(.data, grouping) {
  grouping <- deparse(substitute(grouping)) ##

  df <- data.frame(group = as.factor(.data[[grouping]]))
  
  df %>% 
    count(group)
}
