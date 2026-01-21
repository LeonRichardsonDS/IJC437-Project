library(scales)

### Code for RQ3

# Create df to run regressions
regression_data <- areas %>%
  filter(! Area %in% c('Yorks and Humber', 'North Yorks County', 'South Yorks Met',
                       'West Yorks Met'), Year %in% c(2009, 2022)) %>%
  select(Area, Year, enterprises = Active, employees = Employees, 
         employment_growth = Employees_pct_change, 
         enterprise_growth = Active_pct_change) %>%
  pivot_wider(names_from = Year,
              values_from = c(enterprises, employees, 
                              employment_growth, enterprise_growth),
              names_sep = '_') %>%
  select(-enterprises_2022, -employees_2022, -enterprise_growth_2009,
         -employment_growth_2009) %>%
  mutate(PC1 = pca_vectors$PC1, 
         PC1_inverted = PC1 * -1)


# Table 3.5: Test correlations between independent variables
cor.test(regression_data$enterprises_2009, regression_data$employees_2009)
cor.test(regression_data$enterprises_2009, regression_data$PC1)
cor.test(regression_data$employees_2009, regression_data$PC1)


### Run linear regressions

## Model A: Number of Enterprises, Enterprise Growth 
model_a <- lm(formula = enterprise_growth_2022~enterprises_2009, data = regression_data)
summary(model_a) # view results
coefs_model_a <- coef(model_a) # get coefficients

# Check residuals
hist(resid(model_a), # visualise distribution 
     main = 'Model A Residuals (IV: Number of Enterprises)',
     xlab = 'Residual Value') 
shapiro.test(resid(model_a)) # test residuals for normality
summary(resid(model_a)) # sumamry statistics for residuals

plot(model_a, which = 1, # Residuals vs Fitted values plot
     main = 'Model A Residuals (IV: Number of Enterprises)') 


## Model B: Number of Employees, Enterprise Growth
model_b <- lm(formula = enterprise_growth_2022~employees_2009, data = regression_data)
summary(model_b)
coefs_model_b <- coef(model_b)

hist(resid(model_b), # visualise distribution of residuals
     main = 'Model B Residuals (IV: Number of Employees)',
     xlab = 'Residual Value') 
shapiro.test(resid(model_b)) # test residuals for normality
summary(resid(model_b))

plot(model_b, which = 1, # Residuals vs Fitted values plot
     main = 'Model B Residuals (IV: Number of Employees)') 


## Model C: Number of Enterprises, Employment Growth 
model_c <- lm(formula = employment_growth_2022~enterprises_2009, data = regression_data)
summary(model_c)
coefs_model_c <- coef(model_c)

hist(resid(model_c)) # visualise distribution of residuals
shapiro.test(resid(model_c)) # test residuals for normality


## Model D: Number of Employees, Employment Growth 
model_d <- lm(formula = employment_growth_2022~employees_2009, data = regression_data)
summary(model_d)
coefs_model_d <- coef(model_d)

hist(resid(model_d)) # visualise distribution of residuals
shapiro.test(resid(model_d)) # test residuals for normality


## Model E: Rurality (PC1), Enterprise Growth
model_e <- lm(formula = enterprise_growth_2022~PC1, data = regression_data)
summary(model_e)
coefs_model_e <- coef(model_e)

hist(resid(model_e), # visualise distribution of residuals
     main = 'Model E Residuals (IV: Residual (PC1))',
     xlab = 'Residual Value')
shapiro.test(resid(model_e)) # test residuals for normality

plot(model_e, which = 1, # Residuals vs Fitted values plot
     main = 'Model E Residuals (IV: Number of Employees)') 


## Model F: Rurality (PC1), Employment Growth
model_f <- lm(formula = employment_growth_2022~PC1, data = regression_data)
summary(model_f1)
coefs_model_f <- coef(model_f)

hist(resid(model_f)) # visualise distribution of residuals
shapiro.test(resid(model_f)) # test residuals for normality


# Scatter plot: model A, number of enterprises and enterprise performance
regression_data %>%
  ggplot() +
  geom_point(aes(x = enterprises_2009, y = enterprise_growth_2022)) +
  geom_abline(slope = coefs_model_a["enterprises_2009"], 
              intercept = coefs_model_a["(Intercept)"]) +
  geom_text_repel(aes(x = enterprises_2009, y = enterprise_growth_2022, label = Area), 
                  size = 3, vjust = -0.4) +
  geom_label(label = 'Significant\np-value = 0.013', x = 20000, y = 17.5) +
  labs(title = 'Model A: Number of Enterprises Predicting Enterprise Growth', 
       x = 'Number of Enterprises',
       y = 'Enterprise Growth')

# Scatter plot: model B, number of employees and enterprise performance
regression_data %>%
  ggplot() +
  geom_point(aes(x = employees_2009, y = enterprise_growth_2022)) +
  geom_abline(slope = coefs_model_b["employees_2009"], 
              intercept = coefs_model_b["(Intercept)"]) +
  geom_text_repel(aes(x = employees_2009, y = enterprise_growth_2022, label = Area), 
                  size = 3, vjust = -0.4) +
  geom_label(label = 'Significant\np-value = 0.004', x = 350000, y = 17.5) +
  labs(title = 'Model B: Number of Employees Predicting Enterprise Growth', 
       x = 'Number of Employees',
       y = 'Enterprise Growth')

# Scatter plot: model C, number of enterprises and employment performance
regression_data %>%
  ggplot() +
  geom_point(aes(x = enterprises_2009, y = employment_growth_2022)) +
  geom_abline(slope = coefs_model_c["enterprises_2009"], 
              intercept = coefs_model_c["(Intercept)"]) +
  geom_text_repel(aes(x = enterprises_2009, y = employment_growth_2022, label = Area), 
                  size = 3, vjust = -0.4) +
  geom_label(label = 'Not Significant\np-value = 0.169', x = 20000, y = 4) +
  labs(title = 'Model C: Number of Enterprises Predicting Employment Growth', 
       x = 'Number of Enterprises',
       y = 'Employment Growth')

# Scatter plot: model D, number of employees and employment performance
regression_data %>%
  ggplot() +
  geom_point(aes(x = employees_2009, y = employment_growth_2022)) +
  geom_abline(slope = coefs_model_d["employees_2009"], 
              intercept = coefs_model_d["(Intercept)"]) +
  geom_text_repel(aes(x = employees_2009, y = employment_growth_2022, label = Area), 
                  size = 3, vjust = -0.4) +
  geom_label(label = 'Not Significant\np-value = 0.283', x = 350000, y = 4) +
  labs(title = 'Model D: Number of Employees predicting Employment Growth', 
       x = 'Number of Employees',
       y = 'Employment Growth')

# Scatter plot: model E, Rurality (PC1) and enterprise performance
regression_data %>%
  ggplot() +
  geom_point(aes(x = PC1, y = enterprise_growth_2022)) +
  geom_abline(slope = coefs_model_e["PC1"], 
              intercept = coefs_model_e["(Intercept)"]) +
  geom_text_repel(aes(x = PC1, y = enterprise_growth_2022, label = Area), size = 3, 
                  vjust = -0.4) +
  geom_label(label = 'Significant\np-value = 0.004', x = -10, y = 15) +
  labs(title = 'Model E: Rurality (PC1) predicting Enterprise Growth', 
       x = 'Rurality (PC1)', y = 'Enterprise Growth')

# Scatter plot: model F, Rurality (PC1) and employment performance
regression_data %>%
  ggplot() +
  geom_point(aes(x = PC1, y = employment_growth_2022)) +
  geom_abline(slope = coefs_model_f["PC1"], 
              intercept = coefs_model_f["(Intercept)"]) +
  geom_text_repel(aes(x = PC1, y = employment_growth_2022, label = Area), size = 3, 
                  vjust = -0.4) +
  geom_label(label = 'Not Significant\np-value = 0.143', x = -10, y = 3) +
  labs(title = 'Model F: Rurality (PC1) predicting Employment Growth', 
       x = 'Rurality (PC1)', y = 'Employment Growth')



## Compare significant regressions on same plot
# Scale data for comparison
compare_regressions <- regression_data %>%
  mutate(enterprises_2009 = rescale(enterprises_2009),
         employees_2009 = rescale(employees_2009),
         PC1_inverted = rescale(PC1_inverted))

## Run models with scaled data
# Model A version B
model_ab <- lm(formula = enterprise_growth_2022~enterprises_2009, 
               data = compare_regressions)
summary(model_ab)
coefs_model_ab <- coef(model_ab)

# Model B version B
model_bb <- lm(formula = enterprise_growth_2022~employees_2009, 
               data = compare_regressions)
summary(model_bb)
coefs_model_bb <- coef(model_bb)

# Run model E, version B, with Rurality (PC1) inverted
model_eb <- lm(formula = enterprise_growth_2022~PC1_inverted, 
               data = compare_regressions)
summary(model_eb)
coefs_model_eb <- coef(model_eb)

# Plot all regression together
compare_regressions %>%
  ggplot() +
  # Add points in different colours for each independent variable
  geom_point(aes(x = PC1_inverted, y = enterprise_growth_2022)) + 
  geom_point(aes(x = enterprises_2009, y = enterprise_growth_2022), color = 'blue') +
  geom_point(aes(x = employees_2009, y = enterprise_growth_2022), color = 'red') +
  # Add regression lines in different colours for each independent variables
  geom_abline(slope = coefs_model_eb["PC1_inverted"], 
              intercept = coefs_model_eb["(Intercept)"]) +
  geom_abline(slope = coefs_model_ab["enterprises_2009"], 
              intercept = coefs_model_ab["(Intercept)"], color = 'blue') +
  geom_abline(slope = coefs_model_bb["employees_2009"], 
              intercept = coefs_model_bb["(Intercept)"], color = 'red') +
  geom_label(label = 'Leeds', x = 1, y = 40) +
  geom_label(label = 'Doncaster', x = 0.25, y = 53) +
  labs(title = 'Figure 3.9: Significant Regression Lines Compared for Enterprise Growth', 
       x = 'Scaled Independent Variables', y = '% Enterprise Growth', 
       caption = 'Black = Rurality (PC1), Blue = Enterprises, Red = Employment. Contains three points for each area,\none for each independent variable. ') + 
  theme(plot.caption = element_text(hjust = -0.0, face="italic")) # modify caption





