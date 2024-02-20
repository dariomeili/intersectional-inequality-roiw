# collection of functions for horizontal inequality
p_load(dineq, tidyverse, magrittr)
# read multiple dta files and write file name to column
read_plus <- function(flnm) {
 read_stata(flnm)
}

read_dhs <- function(flnm) {
  read_stata(flnm, )
}
# function to estimate inequality on nested data frame
intersect <- function(.data, ..., .id) {
  group_ <- enquos(...)
  .data %>% 
    group_by(!!!group_) %>% 
    mutate(!!quo_name(.id) := cur_group_id()) 
}

library(dineq)
decomp <- function(.data, outcome, grouping, weights) {
  outcome <- deparse(substitute(outcome)) ##
  grouping <- deparse(substitute(grouping)) ##
  weights <- deparse(substitute(weights)) ##
  gini_decomp(x=.data[[outcome]], z=.data[[grouping]], weights = .data[[weights]])
}

# function for regression based decomposition (insert new group gini function later)
dinolog <- function (formula, weights = NULL, data) 
{
  data <- data.frame(data)
  if (is.null(weights)) {
    data[, "weights"] <- rep(1, nrow(data))
    weights <- "weights"
  }
  else {
    data[, "weights"] <- data[, weights]
  }
  variables <- c(all.vars(formula), "weights")
  y_name <- variables[1]
  note = paste(sum(data[, y_name] <= 0, na.rm = TRUE), "negative or zero x's deleted (unweighted)")
  if (!all(data[, "weights"] >= 0, na.rm = TRUE)) 
    stop("At least one weight is negative", call. = FALSE)
  if (all(data[, "weights"] == 0, na.rm = TRUE)) 
    stop("All weights are zero", call. = FALSE)
  df <- (data[variables])
  df <- df[stats::complete.cases(df), , drop = FALSE]
  gini <- gini.wtd(df[, y_name], df[, "weights"])
  mld <- mld.wtd(df[, y_name], df[, "weights"])
  theil <- theil.wtd(df[, y_name], df[, "weights"])
  variance_income <- Hmisc::wtd.var(df[, y_name], weights = df[, 
                                                                  "weights"])
  df[, "weights_tot"] <- df[, "weights"]
  df[, "weights"] <- df[, "weights"]/sum(df[, "weights"])
  regression <- stats::lm(formula, weights = weights, data = df)
  summary_regression <- summary(regression)
  prediction <- as.data.frame(stats::predict.lm(regression, 
                                                df, type = "terms"))
  prediction[, "residual"] <- stats::resid(regression)
  correlations <- sapply(prediction, function(x) boot::corr(d = cbind(x, 
                                                                      df[, y_name]), w = df[, "weights"]))
  variance_x <- sapply(prediction, function(x) Hmisc::wtd.var(x, 
                                                              weights = df[, "weights_tot"]))
  covvariance <- correlations * sqrt(variance_x * variance_income)
  decomposition_inequality <- covvariance/variance_income
  return(list(inequality_measures = c(gini = gini, mld = mld, 
                                      theil = theil, variance_income = variance_income), 
              decomposition_inequality = decomposition_inequality, 
              regression_results = summary_regression, note = note))
}
# functions to reorganize factors (similar to recode_factor but with "new" = "old)
fct_reorg <- function(fac, ...) {
  fct_recode(fct_relevel(fac, ...), ...)
}

# compound annual growth rate

cagr <- function(.data, value_start, value_end, time_start, time_end) {
  value_start = deparse(substitute(value_start)) ##
  value_end = deparse(substitute(value_end)) ##
  time_start = deparse(substitute(time_start)) ##
  time_end = deparse(substitute(time_end)) ##
  
  v1 = .data[[value_start]]
  v2 = .data[[value_end]]
  t1 = .data[[time_start]]
  t2 = .data[[time_end]]
  
  (v2/v1)^(1/(t2-t1))-1
}

# weighted standard deviation
wtd.sd <-  function(x, weights) sqrt(Hmisc::wtd.var(x, weights=weights))

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


