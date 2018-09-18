## Model all 5 way variable combinations
## Spatial Durbin Model

# Reduced variable set (including factors)
y2 <- data_mod16$Perc_LNP
x2 <- data_mod16 %>% 
  dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
  scale() %>% # already has been scaled, so this doesn't change anything
  as.data.frame() 

x_test <- x2[,1:10]

# All 3 way SDM
sdm3way16 <- all_X_way_SDM(y_vec = data_mod %>% dplyr::filter(year == "2016") %>% dplyr::select(Perc_LNP),
                      x_df = data_mod %>% dplyr::filter(year == "2016") %>% 
                        dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                        scale() %>% 
                        as.data.frame(), 
                      n_vars = 3,
                      listw = listw_16)

sdm3way13 <- all_X_way_SDM(y_vec = data_mod %>% dplyr::filter(year == "2013") %>% dplyr::select(Perc_LNP),
                         x_df = data_mod %>% dplyr::filter(year == "2013") %>% 
                           dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                           scale() %>% 
                           as.data.frame(), 
                         n_vars = 3,
                         listw = listw_13)

sdm3way10 <- all_X_way_SDM(y_vec = data_mod %>% dplyr::filter(year == "2010") %>% dplyr::select(Perc_LNP),
                         x_df = data_mod %>% dplyr::filter(year == "2010") %>% 
                           dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                           scale() %>% 
                           as.data.frame(), 
                         n_vars = 3,
                         listw = listw_10)

sdm3way07 <- all_X_way_SDM(y_vec = data_mod %>% dplyr::filter(year == "2007") %>% dplyr::select(Perc_LNP),
                         x_df = data_mod %>% dplyr::filter(year == "2007") %>% 
                           dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                           scale() %>% 
                           as.data.frame(), 
                         n_vars = 3,
                         listw = listw_07)

sdm3way04 <- all_X_way_SDM(y_vec = data_mod %>% dplyr::filter(year == "2004") %>% dplyr::select(Perc_LNP),
                         x_df = data_mod %>% dplyr::filter(year == "2004") %>% 
                           dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                           scale() %>% 
                           as.data.frame(), 
                         n_vars = 3,
                         listw = listw_04)

sdm3way01 <- all_X_way_SDM(y_vec = data_mod %>% dplyr::filter(year == "2001", !is.na(Perc_LNP)) %>% dplyr::select(Perc_LNP),
                         x_df = data_mod %>% dplyr::filter(year == "2001", !is.na(Perc_LNP)) %>% 
                           dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                           scale() %>% 
                           as.data.frame(), 
                         n_vars = 3,
                         listw = listw_01)

save(sdm3way01, file = "/Users/Jeremy/Documents/R/Modelling-Elections/Clean-Data/sdm3way01.rda")
save(sdm3way04, file = "/Users/Jeremy/Documents/R/Modelling-Elections/Clean-Data/sdm3way04.rda")
save(sdm3way07, file = "/Users/Jeremy/Documents/R/Modelling-Elections/Clean-Data/sdm3way07.rda")
save(sdm3way10, file = "/Users/Jeremy/Documents/R/Modelling-Elections/Clean-Data/sdm3way10.rda")
save(sdm3way13, file = "/Users/Jeremy/Documents/R/Modelling-Elections/Clean-Data/sdm3way13.rda")
save(sdm3way16, file = "/Users/Jeremy/Documents/R/Modelling-Elections/Clean-Data/sdm3way16.rda")

# Variable importance
## 2016
var_imp_16 <- var_imp(fiveway_2016) %>% 
  mutate(year = "2016")

## 2013
var_imp_13 <- var_imp(fiveway_2013) %>% 
  mutate(year = "2013")

## 2010
var_imp_10 <- var_imp(fiveway_2010) %>% 
  mutate(year = "2010")

## 2007
var_imp_07 <- var_imp(fiveway_2007) %>% 
  mutate(year = "2007")

## 2004
var_imp_04 <- var_imp(fiveway_2004) %>% 
  mutate(year = "2004")

## 2001
var_imp_01 <- var_imp(fiveway_2001) %>% 
  mutate(year = "2001")

## Combine and add variable names
name_vars <- data.frame(var = 1:ncol(x16), varname = names(x16))

var_imp_SDM <- rbind(var_imp(sdm3way16) %>% mutate(year = "2016"), 
                     var_imp(sdm3way13) %>% mutate(year = "2013"), 
                     var_imp(sdm3way10) %>% mutate(year = "2010"), 
                     var_imp(sdm3way07) %>% mutate(year = "2007"), 
                     var_imp(sdm3way04) %>% mutate(year = "2004"), 
                     var_imp(sdm3way01) %>% mutate(year = "2001")) %>% 
  left_join(name_vars, by = "var")

# Compare with all 2 way LM
threeway_2016 <- all_X_way(y_vec = data_mod %>% dplyr::filter(year == "2016") %>% dplyr::select(Perc_LNP) %>% unlist %>% unname,
                      x_df = data_mod %>% dplyr::filter(year == "2016") %>% 
                        dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                        scale() %>% 
                        as.data.frame(), 
                      n_vars = 3)
threeway_2013 <- all_X_way(y_vec = data_mod %>% dplyr::filter(year == "2013") %>% dplyr::select(Perc_LNP) %>% unlist %>% unname,
                           x_df = data_mod %>% dplyr::filter(year == "2013") %>% 
                             dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                             scale() %>% 
                             as.data.frame(), 
                           n_vars = 3)
threeway_2010 <- all_X_way(y_vec = data_mod %>% dplyr::filter(year == "2010") %>% dplyr::select(Perc_LNP) %>% unlist %>% unname,
                           x_df = data_mod %>% dplyr::filter(year == "2010") %>% 
                             dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                             scale() %>% 
                             as.data.frame(), 
                           n_vars = 3)
threeway_2007 <- all_X_way(y_vec = data_mod %>% dplyr::filter(year == "2007") %>% dplyr::select(Perc_LNP) %>% unlist %>% unname,
                           x_df = data_mod %>% dplyr::filter(year == "2007") %>% 
                             dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                             scale() %>% 
                             as.data.frame(), 
                           n_vars = 3)
threeway_2004 <- all_X_way(y_vec = data_mod %>% dplyr::filter(year == "2004") %>% dplyr::select(Perc_LNP) %>% unlist %>% unname,
                           x_df = data_mod %>% dplyr::filter(year == "2004") %>% 
                             dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                             scale() %>% 
                             as.data.frame(), 
                           n_vars = 3)
threeway_2001 <- all_X_way(y_vec = data_mod %>% dplyr::filter(year == "2001", !is.na(Perc_LNP)) %>% dplyr::select(Perc_LNP) %>% unlist %>% unname,
                           x_df = data_mod %>% dplyr::filter(year == "2001", !is.na(Perc_LNP)) %>% 
                             dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
                             scale() %>% 
                             as.data.frame(), 
                           n_vars = 3)

var_imp_LM <- rbind(var_imp(threeway_2016) %>% mutate(year = "2016"), 
                     var_imp(threeway_2013) %>% mutate(year = "2013"), 
                     var_imp(threeway_2010) %>% mutate(year = "2010"), 
                     var_imp(threeway_2007) %>% mutate(year = "2007"), 
                     var_imp(threeway_2004) %>% mutate(year = "2004"), 
                     var_imp(threeway_2001) %>% mutate(year = "2001")) %>% 
  left_join(name_vars, by = "var") 

var_imp_LM <- var_imp_LM %>% 
  arrange(year, -sum_w)
var_imp_SDM <- var_imp_SDM %>% 
  arrange(year, -sum_w)

# Print top 5 superset
var_imp_SDM %>% 
  arrange(year, -sum_w) %>% 
  group_by(year) %>%
  dplyr::select(varname, sum_w, year) %>% 
  top_n(wt = sum_w, n = 5) %>% 
  group_by(varname) %>% 
  count(n = n())

var_imp_LM %>% 
  arrange(year, -sum_w) %>% 
  group_by(year) %>%
  dplyr::select(varname, sum_w, year) %>% 
  top_n(wt = sum_w, n = 5) %>% 
  group_by(varname) %>% 
  count(n = n())


##########################################################################################

## Function

##########################################################################################

all_X_way_SDM <- function(y_vec, x_df, n_vars, listw, interact_varname = NA) {
  
  index <- combn(x = c(1:ncol(x_df)), m = n_vars)
  
  Xway_models <- data.frame(iter = c(1:ncol(index))) %>%  
    mutate(intercept = 0, rho = 0, rho.se = 0, AIC = 0, logL = 0)
  
    
    for (i in 1:ncol(index)) {
      
      # Combine the data
      y_vec <- y_vec %>% unlist %>% unname
      mat_data <- cbind(x_df, y_vec)
      
      # Fit regression
      fit <- lagsarlm(y_vec ~ ., 
                      data = mat_data[,c(index[,i],ncol(mat_data))],
                      listw = listw)
      
      # Model statistics
      aic = AIC(fit)
      logL = logLik(fit)[1]
      
      Xway_models$AIC[i] = aic
      Xway_models$logL[i] = logL
      Xway_models$rho[i] = fit$rho
      Xway_models$rho.se[i] = fit$rho.se
      
      # Variables, coefficients and t statistics
      Xway_models$intercept[i] = fit$coefficients[1]
      
      Xway_models$vars[i] = list(index[,i])
      
      Xway_models$coef[i] = list(fit$coefficients[-1] %>% unname)
      
      Xway_models$t[i] = list(fit$rest.se[-1] %>% unname)
      
      
      if (i %% 100 == 0) {
        print(paste("rep",i))
      }
  
    }
  
  
  
  return(Xway_models)
  
}


####################################################################################################


# Reduced variable set (including factors)
y2 <- data_mod16$Perc_LNP
x2 <- data_mod16 %>% 
dplyr::select(-c(Perc_LNP, Election_Division, year, Swing)) %>% 
  scale() %>% # already has been scaled, so this doesn't change anything
  as.data.frame() 

x_test <- x2[,1:10]

test <- all_X_way(y2, x_test, n_vars = 2)
