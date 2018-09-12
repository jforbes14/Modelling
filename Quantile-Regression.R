# Quantile regression for 2016 election
library(tidyverse)
library(broom)
library(quantreg)

coef_qreg <- function(data_mod, Year) {
  
  Year = as.character(Year)
  
q10 <- rq(Perc_LNP ~ ., 
   data = data_mod %>% 
     filter(year == Year) %>% 
     select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.10)

q20 <- rq(Perc_LNP ~ ., 
          data = data_mod %>% 
            filter(year == Year) %>% 
            select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.20)

q30 <- rq(Perc_LNP ~ ., 
          data = data_mod %>% 
            filter(year == Year) %>% 
            select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.30)

q40 <- rq(Perc_LNP ~ ., 
          data = data_mod %>% 
            filter(year == Year) %>% 
            select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.40)

q50 <- rq(Perc_LNP ~ ., 
          data = data_mod %>% 
            filter(year == Year) %>% 
            select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.50)

q60 <- rq(Perc_LNP ~ ., 
          data = data_mod %>% 
            filter(year == Year) %>% 
            select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.60)

q70 <- rq(Perc_LNP ~ ., 
          data = data_mod %>% 
            filter(year == Year) %>% 
            select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.70)

q80 <- rq(Perc_LNP ~ ., 
          data = data_mod %>% 
            filter(year == Year) %>% 
            select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.80)

q90 <- rq(Perc_LNP ~ ., 
          data = data_mod %>% 
            filter(year == Year) %>% 
            select(Perc_LNP,  (superset$varname %>% as.character)), tau = 0.90)

qreg <- bind_rows(data.frame(summary(q10, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q10, se = "boot")$tau),
                  data.frame(summary(q20, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q20, se = "boot")$tau),
                  data.frame(summary(q30, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q30, se = "boot")$tau),
                  data.frame(summary(q40, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q40, se = "boot")$tau),
                  data.frame(summary(q50, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q50, se = "boot")$tau),
                  data.frame(summary(q60, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q60, se = "boot")$tau),
                  data.frame(summary(q70, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q70, se = "boot")$tau),
                  data.frame(summary(q80, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q80, se = "boot")$tau),
                  data.frame(summary(q90, se = "boot")$coefficients %>% data.frame() %>% rownames_to_column(var = "term"), q = summary(q90, se = "boot")$tau)) %>% 
  mutate(year = Year)

return(qreg)
  }

## 2016
qreg16 <- coef_qreg(data_mod = data_mod, Year = "2016")

qreg16 %>% 
  left_join(all_coefs %>% select(term, estimate, year), by = c("year", "term")) %>% 
  filter(term != "(Intercept)")  %>% 
  mutate(upper95 = Value + 1.96*Std..Error, lower95 = Value - 1.96*Std..Error) %>% 
  ggplot(aes(x = q, y = Value)) + 
  geom_line() + geom_ribbon(aes(ymin = lower95, ymax = upper95), col = "grey50", alpha = 0.3) + 
  geom_hline(aes(yintercept = estimate), linetype = 2, col = "blue") +
  geom_hline(aes(yintercept = 0), alpha = 0.5) + 
  facet_wrap(~term)
  
## 2013
qreg13 <- coef_qreg(data_mod = data_mod, Year = "2013")
  
qreg13 %>% 
  left_join(all_coefs %>% select(term, estimate, year), by = c("year", "term")) %>% 
  filter(term != "(Intercept)")  %>%
mutate(upper95 = Value + 1.96*Std..Error, lower95 = Value - 1.96*Std..Error) %>% 
  ggplot(aes(x = q, y = Value)) + 
  geom_line() + geom_ribbon(aes(ymin = lower95, ymax = upper95), col = "grey50", alpha = 0.3) + 
  geom_hline(aes(yintercept = estimate), linetype = 2, col = "blue") +
  geom_hline(aes(yintercept = 0), alpha = 0.5) + 
  facet_wrap(~term)

## 2010
qreg10 <- coef_qreg(data_mod = data_mod, Year = "2010")

qreg10 %>% 
  left_join(all_coefs %>% select(term, estimate, year), by = c("year", "term")) %>% 
  filter(term != "(Intercept)")  %>%
  mutate(upper95 = Value + 1.96*Std..Error, lower95 = Value - 1.96*Std..Error) %>% 
  ggplot(aes(x = q, y = Value)) + 
  geom_line() + geom_ribbon(aes(ymin = lower95, ymax = upper95), col = "grey50", alpha = 0.3) + 
  geom_hline(aes(yintercept = estimate), linetype = 2, col = "blue") +
  geom_hline(aes(yintercept = 0), alpha = 0.5) + 
  facet_wrap(~term)


## 2007
qreg07 <- coef_qreg(data_mod = data_mod, Year = "2007")

qreg07 %>% 
  left_join(all_coefs %>% select(term, estimate, year), by = c("year", "term")) %>% 
  filter(term != "(Intercept)")  %>%
  mutate(upper95 = Value + 1.96*Std..Error, lower95 = Value - 1.96*Std..Error) %>% 
  ggplot(aes(x = q, y = Value)) + 
  geom_line() + geom_ribbon(aes(ymin = lower95, ymax = upper95), col = "grey50", alpha = 0.3) + 
  geom_hline(aes(yintercept = estimate), linetype = 2, col = "blue") +
  geom_hline(aes(yintercept = 0), alpha = 0.5) + 
  facet_wrap(~term)

## 2004
qreg04 <- coef_qreg(data_mod = data_mod, Year = "2004")

qreg04 %>% 
  left_join(all_coefs %>% select(term, estimate, year), by = c("year", "term")) %>% 
  filter(term != "(Intercept)")  %>%
  mutate(upper95 = Value + 1.96*Std..Error, lower95 = Value - 1.96*Std..Error) %>% 
  ggplot(aes(x = q, y = Value)) + 
  geom_line() + geom_ribbon(aes(ymin = lower95, ymax = upper95), col = "grey50", alpha = 0.3) + 
  geom_hline(aes(yintercept = estimate), linetype = 2, col = "blue") +
  geom_hline(aes(yintercept = 0), alpha = 0.5) + 
  facet_wrap(~term)

## 2001
qreg01 <- coef_qreg(data_mod = data_mod, Year = "2001")

qreg01 %>% 
  left_join(all_coefs %>% select(term, estimate, year), by = c("year", "term")) %>% 
  filter(term != "(Intercept)")  %>%
  mutate(upper95 = Value + 1.96*Std..Error, lower95 = Value - 1.96*Std..Error) %>% 
  ggplot(aes(x = q, y = Value)) + 
  geom_line() + geom_ribbon(aes(ymin = lower95, ymax = upper95), col = "grey50", alpha = 0.3) + 
  geom_hline(aes(yintercept = estimate), linetype = 2, col = "blue") +
  geom_hline(aes(yintercept = 0), alpha = 0.5) + 
  facet_wrap(~term)
