# function to run calculate GGinis
ggini <- function (.data, outcome, grouping, weights) {
  # this is all just prep
  outcome <- deparse(substitute(outcome)) ##
  grouping <- deparse(substitute(grouping)) ##
  weights <- deparse(substitute(weights))
  
  #make sure function only runs if not the hole category is NA
  if (all(is.na(.data[[grouping]]))) { 
    return(list("group_shares" = NA,
                "group_means" = NA,
                "overall_mean" = NA,
                "no_groups" = NA,
                "group_gini" = NA,
                "theil" = NaN)) 
  } else {
  
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }
  if (!all(weights >= 0, na.rm = TRUE)) 
    stop("At least one weight is negative", call. = FALSE)
  if (all(weights == 0, na.rm = TRUE)) 
    stop("All weights are zero", call. = FALSE)
  # assigning var names and making sure format is right
  
  df <- data.frame(x = as.numeric(.data[[outcome]]), z = as.factor(.data[[grouping]]), w = as.numeric(.data[[weights]]))
  z <- factor(grouping)
  df <- df[stats::complete.cases(df), ,drop = FALSE]
  df <- droplevels(df)
  df[, "w"] <- df[, "w"]/sum(df[, "w"])
  n <- as.numeric(nrow(df))
  n_weighted <- sum(df[, "w"])
  dfSplit <- split(df[, c("x", "w")], df[, "z"])
  n_group <- table(df[, "z"])
  n_group_weighted <- sapply(dfSplit, function(df) sum(df[, "w"]), simplify = TRUE)

  xMean <- stats::weighted.mean(df[, "x"], df[, "w"])
  xMean_group <- sapply(dfSplit, function(df) stats::weighted.mean(df[,"x"], df[, "w"]), simplify = TRUE)
  share_group <- n_group_weighted/n_weighted
  
  n_cat <- length(n_group) 
    
  value_matrix <-  matrix(nrow = n_cat, ncol = n_cat)
  for (g in 1:n_cat) {
    for (k in 1:n_cat) {
      value_matrix[g,k] = share_group[g]*share_group[k]*abs(xMean_group[g]-xMean_group[k])
    }
  }
  k_sums <- rowSums(value_matrix, na.rm = T)
  g_sums <- sum(k_sums, na.rm = T)
  
  ggini_value <- g_sums/(2*xMean)

  # calculate Theil
  mean_x <- mean(xMean_group)
  theil <- sum((xMean_group/mean_x)*asinh(xMean_group/mean_x))/n_cat
  return(list("group_shares" = share_group,
              "group_means" = xMean_group,
              "overall_mean" = xMean,
              "no_groups" = n_cat,
              "group_gini" = ggini_value, 
              "theil" = theil)) 
  }
  
}



