## function to compute maximum counterfactual inequality
theil_perm <- function(.data, iterations, outcome, grouping, weights) {
  # this is all just prep
  outcome <- deparse(substitute(outcome)) ##
  grouping <- deparse(substitute(grouping)) ##
  weights <- deparse(substitute(weights))
  
  #make sure function only runs if not the hole category is NA
  if (all(is.na(.data[[grouping]]))) { 
    return(NaN) 
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
    
    
    #initialize results vector
    sim <- c(rep(NA, iterations))
    
    # simulation
    for(i in 1:iterations) {
      #permutate groups 
      df.perm <- df
      df.perm[,"z"] <- sample(df[["z"]])
      xMean_group <- df.perm %>% 
        group_by(z) %>% 
        summarise(means=stats::weighted.mean(x, w)) %>% 
        pull(means)
      
      mean_x <- mean(xMean_group)
      
      n_cat <-  df.perm %>% 
        distinct(z) %>% 
        nrow()
      
      sim[i] <-  sum((xMean_group/mean_x)*asinh(xMean_group/mean_x))/n_cat
    }
    mean(sim, na.rm=T)
  }
}  
