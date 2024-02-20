# Gini Permutation function
gini_perm <- function (.data, iterations, outcome_var, stratum, variable, wgt) {
  
# define variables 

outcome_var <- deparse(substitute(outcome_var)) ##
stratum <- deparse(substitute(stratum)) ##
variable <- deparse(substitute(variable)) ##
wgt <- deparse(substitute(wgt))

df <- data.frame(y = as.numeric(.data[[outcome_var]]), 
             c = as.factor(.data[[stratum]]), 
             z = as.factor(.data[[variable]]),
             w = as.numeric(.data[[wgt]])) 

df <- df[stats::complete.cases(df), ,drop = FALSE]
df <- droplevels(df)
# initialize rpermutation df
resamp <- tibble::as_tibble(matrix(NA, nrow = nrow(df), ncol= iterations))

# permute "variable" group
resamp %<>% 
  mutate(across(.cols = everything(),
                ~ sample(df[["z"]]))) 

# combine stratum group and variable
resamp_cross <- resamp %>%  
  mutate(across(.cols = everything(),
                ~ fct_cross(df[["c"]], .x))
  )

# function to run calculate GGini
ggini2 <- function (outcome, grouping, weights) {
  # this is all just prep
  
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }
  if (!all(weights >= 0, na.rm = TRUE)) 
    stop("At least one weight is negative", call. = FALSE)
  if (all(weights == 0, na.rm = TRUE)) 
    stop("All weights are zero", call. = FALSE)
  # assigning var names and making sure format is right
  
  df <- data.frame(x = as.numeric(outcome), z = as.factor(grouping), w = as.numeric(weights))
  z <- factor(grouping)
  df <- df[stats::complete.cases(df), ,drop = FALSE]
  df <- droplevels(df)
  df[, "w"] <- df[, "w"]/sum(df[, "w"])
  n <- as.numeric(nrow(df))
  n_weighted <- sum(df[, "w"])
  dfSplit <- split(df[, c("x", "w")], df[, "z"])
  n_group <- table(df[, "z"])
  n_group_weighted <- sapply(dfSplit, function(df) sum(df[, 
                                                          "w"]), simplify = TRUE)
  
  xMean <- stats::weighted.mean(df[, "x"], df[, "w"])
  xMean_group <- sapply(dfSplit, function(df) stats::weighted.mean(df[,"x"], df[, "w"], na.rm = T), simplify = TRUE)
  share_group <- n_group_weighted/n_weighted
  share_group_income <- share_group * xMean_group/xMean
  
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
  
  
  return(list("group_shares" = share_group,
              "group_means" = xMean_group,
              "overall_mean" = xMean,
              "no_groups" = n_cat,
              "group_gini" = ggini_value)) 
}

sim <- sapply(resamp_cross, 
              function(x) {
                result <- ggini2(df[["y"]], x, df[["w"]])
                return(result[["group_gini"]])
                }
              )
sim <- unname(sim)

sim_mean = mean(sim, na.rm = T)
sim_sd = sd(sim, na.rm = T)

return(list("sim_mean" = sim_mean,
            "sim_sd" = sim_sd,
            "sim_values" = sim))

}
