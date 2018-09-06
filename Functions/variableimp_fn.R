## Variable importance function using Akaike Weights
## Requires Xway models

var_imp <- function(Xway) {
  
  #if (!"vars" %in% names(Xway)) {
  #Xway$vars <- apply(Xway[which(startsWith(colnames(Xway), "var"))],1,list)
  #}
  
  # Best global AIC
  best_AIC <- min(Xway$AIC)
  
  # Akaike weights
  w_vars <- Xway %>% 
    mutate(delta = AIC - best_AIC,
           w_numer = exp(-delta/2),
           w_denom = sum(w_numer),
           w = w_numer/w_denom) %>% 
    dplyr::select(vars, w)
  
  # Inferring number of columns in X
  n_col_x = Xway %>% filter(iter %in% 1:100) %>% dplyr::select(vars) %>% unlist %>% max
  
  # Empty variable importance df
  var_imp <- data.frame(var = 1:n_col_x, sum_w = 0, coef_wsum = 0)
  
  for (i in 1:nrow(w_vars)) {
    # Variables
    vars <- w_vars$vars[i] %>% unlist %>% unname
    
    # Add weight to sum of weights
    w <- w_vars$w[i]
    var_imp[vars, "sum_w"] <- var_imp[vars, "sum_w"] + w
    
    # Add running tally of weighted coefficient
    var_imp[vars, "coef_wsum"] <- var_imp[vars, "coef_wsum"] + w*(Xway[i, ]$coef %>% unlist)
  }
  
  # Compute weighted model average of coeffficient
  var_imp <- var_imp %>% 
    mutate(coef_wavg = (coef_wsum / sum_w))
  
  return(var_imp)
}

#test <- var_imp(Xway = twoway_2016)
