name_extractor <- function(.group_means, group=c("low", "high")) {
  means <- .group_means[["means"]]
  names(means) <- .group_means[["group"]]
  if(group=="low") {
    names(means)[which.min(means)]
    } else {
      names(means)[which.max(means)]
  }
}

mean_extractor <- function(.group_means, group=c("low", "high")) {
    means <- .group_means[["means"]]
    names(means) <- .group_means[["group"]]
    if(group=="low") {
      min(means, na.rm = T)
      } else {
        max(means, na.rm = T)}
}

share_extractor <- function(.group_n, .group_means, type=c("low", "high")) {
  means <- .group_means[["means"]]
  names(means) <- .group_means[["group"]]
  if (type=="low") {
    group_name <- names(means)[which.min(means)]
    if (is.null(group_name)) NaN else {
    .group_n %>% 
      mutate(pct = n/sum(n)) %>%  
      filter(group == group_name) %>% 
      pull(pct)
    }
  } else {
    group_name <- names(means)[which.max(means)]
    if (is.null(group_name)) NaN else {
      .group_n %>% 
        mutate(pct = n/sum(n)) %>%  
        filter(group == group_name) %>% 
        pull(pct)
    }
  }
}

groupn_extractor <- function(.group_n, .group_means, type=c("low", "high")) {
  means <- .group_means[["means"]]
  names(means) <- .group_means[["group"]]
  if (type=="low") {
    group_name <- names(means)[which.min(means)]
    if (is.null(group_name)) NaN else {
      .group_n %>% 
        filter(group == group_name) %>% 
        pull(n)
    }
  } else {
    group_name <- names(means)[which.max(means)]
    if (is.null(group_name)) NaN else {
      .group_n %>% 
        filter(group == group_name) %>% 
        pull(n)
    }
  }
}

