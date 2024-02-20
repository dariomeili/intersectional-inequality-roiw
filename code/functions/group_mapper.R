high_name <- function(results) {
  if (!is.na(results$group_means)) {
    results$group_means %>% 
      which.max %>% 
      names() 
    } else {
      NA_character_
    }
}

high_value <- function(results) {
  if (!is.na(results$group_means)) {
    results$group_means %>% 
      max() 
    } else {
        NaN
    }
}

low_name <- function(results) {
  if (!is.na(results$group_means)) {
    results$group_means %>% 
      which.min %>% 
      names() 
    } else {
        NA_character_
    }
}

low_value <- function(results) {
  if (!is.na(results$group_means)) {
    results$group_means %>% 
      min() 
    } else {
        NaN
      }
}

low_share <- function(results) {
  if (!is.na(results$group_means)) {
    results$group_shares[[which.min(results$group_means)]]
  } else {
    NaN
  }
}

high_share <- function(results) {
  if (!is.na(results$group_means)) {
    results$group_shares[[which.max(results$group_means)]]
  } else {
    NaN
  }
}