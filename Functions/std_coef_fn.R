## Standardized coefficients for varible importance

std_coef_fn <- function(Xway) {
  
  # Inferring number of columns in X
  n_col_x = Xway %>% filter(iter %in% 1:100) %>% dplyr::select(vars) %>% unlist %>% max
  
  # Average coefficients
  var_coef = data.frame(var = 1:n_col_x, coef = 0, t = 0, avg_coef = 0, avg_abs_coef = 0, variance_coef = 0)
  
  for (i in 1:nrow(Xway)) {
    
    vars <- Xway$vars[i] %>% unlist %>% unname
    
    for (j in 1:length(vars)) {
      var = vars[j]
      
      # Coef
      coef = Xway$coef[i][[1]][j]
      
      row_id = which(var_coef$var == var)
      
      exist_coef = var_coef$coef[row_id] %>% unlist
      
      if (exist_coef[1] == 0) {
        exist_coef = exist_coef[-1]
      }
      
      var_coef$coef[row_id] <- c(exist_coef, coef) %>% list
      
      # T-stat
      t = Xway$t[i][[1]][j]
      
      exist_t = var_coef$t[row_id] %>% unlist
      
      if (exist_t[1] == 0) {
        exist_t = exist_t[-1]
      }
      
      var_coef$t[row_id] <- c(exist_t, t) %>% list
      
      
    }
    
  }
  
  for (k in 1:nrow(var_coef)) {
    var_coef$avg_coef[k] = mean(var_coef$coef[k] %>% unlist)
    var_coef$avg_abs_coef[k] = mean(abs(var_coef$coef[k] %>% unlist))
    var_coef$variance_coef[k] = var(var_coef$coef[k] %>% unlist)
  }
  

  return(var_coef)
  
}

#test <- std_coef_fn(Xway = twoway_2016)
