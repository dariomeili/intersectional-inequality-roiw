# function to compute maximum counterfactual inequality
max_ratio <- function(.data, outcome, grouping, weights) {
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
                "group_gini" = NA)) 
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
  df <- df[stats::complete.cases(df), ,drop = FALSE]
  df <- droplevels(df)
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
  
  n_cat <- length(n_group.max)
  # calculate maximum ratio
  weights.max <- share_group.max/sum(share_group.max)
  order <- order(xMean_group.max)
  xMean_group.max <- xMean_group.max[order]
 
  max_r <- ineq_ratio(xMean_group.max)
  
  return(max_r) 
  }
}  
