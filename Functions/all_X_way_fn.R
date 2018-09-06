## Model all 5 way variable combinations

# test
y_vec = y16
x_df = x16
n_vars = 2
interact_varname = "Educ"

all_X_way <- function(y_vec, x_df, n_vars, interact_varname = NA) {
  
  index <- combn(x = c(1:ncol(x_df)), m = n_vars)
  
  Xway_models <- data.frame(iter = c(1:ncol(index))) %>%  
    mutate(intercept = 0, r2 = 0, adj_r2 = 0, AIC = 0, BIC = 0, logL = 0)
  
  ## No interaction
  if (is.na(interact_varname)) {
  
  for (i in 1:ncol(index)) {
    
    # Combine the data
    mat_data <- cbind(x_df, y_vec)
    
    #a <- index[,i][1] 
    #b <- index[,i][2]
    #c <- index[,i][3]
    #d <- index[,i][4]
    #e <- index[,i][5]
    
    # Fit regression
    fit <- lm(y_vec ~ ., mat_data[,c(index[,i],ncol(mat_data))])
    
    # Model statistics
    sumr = summary(fit)
    aic = AIC(fit)
    bic = BIC(fit)
    logL = logLik(fit)
    r2 = sumr$r.squared
    adj_r2 = sumr$adj.r.squared
    
    Xway_models$r2[i] = r2
    Xway_models$adj_r2[i] = adj_r2
    Xway_models$AIC[i] = aic
    Xway_models$BIC[i] = bic
    Xway_models$logL[i] = logL
    
    # Variables, coefficients and t statistics
    Xway_models$intercept[i] = fit$coefficients[1]
    
    Xway_models$vars[i] = list(index[,i])
    
    Xway_models$coef[i] = list(fit$coefficients[-1] %>% unname)
    
    Xway_models$t[i] = list(sumr$coefficients[-1,"t value"] %>% unname)
    
    
    if (i %% 10000 == 0) {
      print(paste("rep",i))
    }
    
    
  }
  }
  
  
  ## Interaction
  if (!is.na(interact_varname)) {
    
    if(!as.character(interact_varname) %in% names(x_df)) {
      print("Error: Interaction variable name is not in data. Make sure names matches and is entered as a string.")
      break
    }
    
    int_id <- which(names(x_df) == interact_varname)
    
    for (i in 1:ncol(index)) {
      
      # Combine the data
      mat_data <- cbind(x_df, y_vec)
      
      union_index <- union(index[,i], int_id)
      
  #    form = as.formula(paste(paste0("y_vec ~ (", paste(names(x), collapse = " + ")), 
  #                            paste0(")*", interact_varname, collapse = "")))
      
      form = as.formula(paste(paste0("y_vec ~ (.", paste0(")*", interact_varname, collapse = ""))))
                            
      
      # Fit regression
      fit <- lm(form, mat_data[,c(union_index,ncol(mat_data))])
      
      # Model statistics
      sumr = summary(fit)
      aic = AIC(fit)
      bic = BIC(fit)
      logL = logLik(fit)
      r2 = sumr$r.squared
      adj_r2 = sumr$adj.r.squared
      
      Xway_models$r2[i] = r2
      Xway_models$adj_r2[i] = adj_r2
      Xway_models$AIC[i] = aic
      Xway_models$BIC[i] = bic
      Xway_models$logL[i] = logL
      
      # Variables, coefficients and t statistics
      Xway_models$intercept[i] = fit$coefficients[1]
      
      Xway_models$vars[i] = list(index[,i])
      
      Xway_models$coef[i] = list(sumr$coefficients[-1,"Estimate"] %>% unname)
      
      Xway_models$t[i] = list(sumr$coefficients[-1,"t value"] %>% unname)
      
      
      if (i %% 10000 == 0) {
        print(paste("rep",i))
      }
      
      
    }
  }
  
 
  return(Xway_models)
   
}

####################################################################################################


# Reduced variable set (including factors)
#y2 <- data_mod16$Perc_LNP
#x2 <- data_mod16 %>% 
#dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
#  scale() %>% # already has been scaled, so this doesn't change anything
#  as.data.frame() 

#x_test <- x2[,1:10]

#test <- all_X_way(y2, x_test, n_vars = 2)
