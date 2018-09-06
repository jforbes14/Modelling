## Function for top 100 AIC

step100AIC <- function(all5way, y, x) {
  
  if (nrow(all5way) != 100) {
    all5way <- all5way %>% 
      arrange(AIC) %>% top_n(100)
  }
  
  top100_all <- data.frame(matrix(NA, nrow = (ncol(x)-4)*100, ncol = 22))
  colnames(top100_all) <- colnames(all5way)
  
  top100_all <- top100_all %>% 
    mutate(num_vars = rep(5:ncol(x), each = 100)) 
  
  top100_all[1:100, ] <- mutate(all5way, num_vars = 5, iter = 1:100)
  
  
  top100_all$vars = apply(top100_all[which(startsWith(colnames(top100_all), "var"))],1,list)
  top100_all$coef = apply(top100_all[which(startsWith(colnames(top100_all), "coef"))],1,list)
  top100_all$t = apply(top100_all[which(startsWith(colnames(top100_all), "t"))],1,list)
  
  top100_all <- top100_all %>% 
    dplyr::select(-c(var1, var2, var3, var4, var5, 
              t1, t2, t3, t4, t5, 
              coef1, coef2, coef3, coef4, coef5,
              iter, intercept))
  
  ## START
  
  for (k in 5:(ncol(x)-1)) { # k-way models
    #  prev_models <- top100_all[ (100*(k-5) + 1):(100*(k - 4)), ]
    prev_models <- top100_all %>% filter(num_vars == k)
    
    prev_no <- min(100, nrow(prev_models))
    
    for (j in 1:prev_no) { # each of the previous top 100 models
      prev_iter <- prev_models[j,]
      vars = unlist(prev_iter$vars, use.names = FALSE)
      #ticker = 0
      
      for (i in 1:ncol(x)) { # adding each unused variable to the model
        
        if (!i %in% vars) {
          #     ticker = ticker + 1 # ticker to count models branching off previous model
          
          new_vars = c(vars,i)
          
          dat = cbind(y, x[,new_vars])
          
          mod <- lm(y ~ ., dat)
          
          new_coef = mod$coefficients[-1]
          new_t = (summary(mod)$coefficients %>% as.data.frame)$"t value"[-1]
          r2 = summary(mod)$r.squared
          adj_r2 = summary(mod)$adj.r.squared
          AIC = AIC(mod)
          BIC = BIC(mod)
          logL = logLik(mod)
          
          #      index = 100*(k-4) + (52-k)*(j-1) + ticker # row number
          index = sum(!is.na(top100_all$AIC)) + 1 #(ncol(x)-k)*(j-1) + ticker # row number
          
          top100_all$vars[index] <- list(new_vars)
          top100_all$coef[index] <- list(new_coef)
          top100_all$t[index] <- list(new_t)
          top100_all$r2[index] <- r2
          top100_all$AIC[index] <- AIC
          top100_all$adj_r2[index] <- adj_r2
          top100_all$BIC[index] <- BIC
          top100_all$logL[index] <- logL
          
          top100_all$num_vars[index] = k + 1
          
        }
      }
    }
    
    # Keep only the lowest 100 AIC from new models
    new_models <- top100_all[(100*(k - 4) + 1):nrow(top100_all),] %>% 
      arrange(AIC)
    
    n = length(unique(new_models$AIC))
    
    if (n >= 100) {
      
      AIC100 <- data.frame(z = 1:100, AIC = unique(new_models$AIC)[1:100])
      new_models2 <- new_models %>% 
        inner_join(AIC100, by = "AIC")
      
      unique_new_models <- new_models2[!duplicated(new_models2$z),] %>% 
        dplyr::select(-z)
      
      top100_all[(100*(k - 4) + 1):(100*(k - 4) + 100), ] <- unique_new_models[1:100, ]
      top100_all[(100*(k - 4) + 100 + 1):((ncol(x)-4)*100), ] = NA
      
    } else {
      
      AIC100 <- data.frame(z = 1:n, AIC = unique(new_models$AIC)[1:n])
      new_models2 <- new_models %>% 
        inner_join(AIC100, by = "AIC")
      
      unique_new_models <- new_models2[!duplicated(new_models2$z),] %>% 
        dplyr::select(-z)
      
      top100_all[(100*(k - 4) + 1):(100*(k - 4) + n), ] <- unique_new_models[1:n, ]
      top100_all[(100*(k - 4) + n + 1):((ncol(x)-4)*100), ] = NA
      
    }
    print(k)
  }

  
  ## Omit NA rows
  
  top100_all <- na.omit(top100_all)
  
  return(top100_all)
  
}

x2 = data_mod16 %>% 
  dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
  scale() %>% # already has been scaled, so this doesn't change anything
  as.data.frame() 
y2 = data_mod16$Perc_LNP

test <- step100AIC(all5way = top100_5way, 
                   y = y2, 
                   x = x2)
