# share of rural population in a group; .x= data list-column, .y = group name column
rural_pct = function(.x, .y) {
  if (!is.na(.y)) {
    result <- .x %>%
      filter(gen.eth == paste(.y)) %>% 
      count(residence) %>% 
      mutate(pct = n/sum(n)) %>% 
      filter(residence %in% c("rural")) %>% 
      pull(pct)
    if (length(result)!=0) {
      return(result) } else {
        return(0)
      }
  } else {
    return(NaN)
  }
}

# share of zero's (education)
pct_zero <- function(.data) {
  min_educ <- min(.data[["education"]])
  if (min_educ > 0) {
    return(0)
  } else {
    .data %>%
      count(education) %>% 
      mutate(pct = n/sum(n)) %>%
      filter(education == 0) %>% 
      pull(pct)
  }
}

# weighted standard deviation
wtd.sd <-  function(x, weights) sqrt(Hmisc::wtd.var(x, weights=weights))
