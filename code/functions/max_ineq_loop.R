# function to compute maximum counterfactual inequality
max_ineq_loop <- function(.data, outcome, grouping, weights) {
  # this is all just prep
  outcome <- deparse(substitute(outcome)) ##
  grouping <- deparse(substitute(grouping)) ##
  weights <- deparse(substitute(weights))
  
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }
  if (!all(weights >= 0, na.rm = TRUE)) 
    stop("At least one weight is negative", call. = FALSE)
  if (all(weights == 0, na.rm = TRUE)) 
    stop("All weights are zero", call. = FALSE)
  # assigning var names and making sure format is right
  
  df <- data.frame(x = as.numeric(.data[[outcome]]), z = as.factor(.data[[grouping]]), w = as.numeric(.data[[weights]]))
  df <- df[stats::complete.cases(df), ,drop = FALSE]
  n <- as.numeric(nrow(df))
  df[, "w"] <- df[, "w"]/sum(df[, "w"])
  
  n_weighted <- sum(df[, "w"])
  dfSplit <- split(df[, c("x", "w")], df[, "z"])
  n_group <- table(df[, "z"])
  n_group_weighted <- sapply(dfSplit, function(df) sum(df[,"w"]), simplify = TRUE)
  xMean <- stats::weighted.mean(df[, "x"], df[, "w"])
  xMean_group <- sapply(dfSplit, function(df) stats::weighted.mean(df[,"x"], df[, "w"]), simplify = TRUE)
  share_group <- n_group_weighted/n_weighted
  share_group_income <- share_group * xMean_group/xMean
  
  # permute outcome and group distribution and re-calculate parameters
  share.max <- share_group[order(xMean_group)]
  
  df.max <- df[order(df[,"x"]),]
  z_group <-  df[, "z"]
  
  df.max[, "z"] <- z_group[order(match(z_group, names(share.max)))]
  
  n_weighted.max <- sum(df.max[, "w"])
  dfSplit.max <- split(df.max[, c("x", "w")], df.max[, "z"])
  n_group.max <- table(df.max[, "z"])
  n_group_weighted.max <- sapply(dfSplit.max, function(df.max) sum(df.max[,"w"]), simplify = TRUE)
  
  xMean.max <- stats::weighted.mean(df.max[, "x"], df.max[, "w"])
  xMean_group.max <- sapply(dfSplit.max, function(df.max) stats::weighted.mean(df.max[,"x"], df.max[, "w"]), simplify = TRUE)
  share_group.max <- n_group_weighted.max/n_weighted.max
  share_group_income.max <- share_group.max * xMean_group.max/xMean.max
  
  # calculate maximum ggini
  n_cat <- length(n_group.max)
  value_matrix <-  matrix(nrow = n_cat, ncol = n_cat) 
  for (g in 1:n_cat) {
    for (k in 1:n_cat) {
      value_matrix[g,k] = share_group.max[g]*share_group.max[k]*abs(xMean_group.max[g]-xMean_group.max[k])
    }
  }
  k_sums <- rowSums(value_matrix, na.rm = T)
  g_sums <- sum(k_sums, na.rm = T)
  
  ggini_value <- g_sums/(2*xMean.max)
  
  return(list("group_shares" = share_group.max,
              "group_means" = xMean_group.max,
              "overall_mean" = xMean.max,
              "no_groups" = n_cat,
              "group_gini" = ggini_value)) 
}  
