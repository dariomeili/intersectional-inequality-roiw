ineq_ratio <- function (.group_means) {
  res <- .group_means %>% 
    summarise(min_group = min(means, na.rm = T), 
              max_group = max(means, na.rm = T)) %>% 
    mutate(value = 1-(min_group/max_group)) %>% 
    pull(value)
  
  if (res == 1) return(NaN) else return(res)
}
