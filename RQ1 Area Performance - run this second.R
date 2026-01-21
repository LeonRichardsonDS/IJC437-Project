library(ggrepel)

### Code for RQ1
# Uses areas (database one)

# Figure 1: Yorkshire and Humber overall line graph: enterprises vs employment
areas %>%
  filter(Area == 'Yorks and Humber') %>%
  ggplot() +
  geom_line(aes(x = Year, y = Active_pct_change, color = 'Enterprises'),
            linewidth = 1) +
  geom_line(aes(x = Year, y = Employees_pct_change, color = 'Employees'), linewidth = 1) +
  scale_color_manual(values = c("Enterprises" = "Blue", "Employees" = "Red")) +
  labs(title = 'Figure 3.1: Yorkshire and Humber (Region) Performance 2009-2022', y = '% Change vs 2009', colour = '')

# Test correlation for Region enterprise and employment growth
yh <- areas %>% # subset data to region
  filter(Area == 'Yorks and Humber') 
cor.test(yh$Active_pct_change, yh$Employees_pct_change) # test correlation

# Get area performance summary statistics for table 3.3
summary(study_areas_summary$Active_pct_change)
summary(study_areas_summary$Employees_pct_change)


## Compare Enterprise and Employment growth (scatter plot with regression line)
# Enterprises and Employment: Linear Regression 
lr_active_emps <- lm(formula = Employees_pct_change~Active_pct_change,
                     data = areas) # Get regression line

summary(lr_active_emps)

# Get formula for line
coefs_lr_ae <- coef(lr_active_emps)

# Scatter plot comparing area growth in enterprises and employees
areas %>%
  filter(Year == 2022, ! County %in% c('Region', 'County Total')) %>%
  ggplot(aes(x = Active_pct_change, y = Employees_pct_change, 
             color = Geo_Grouping)) +
  geom_point(size = 3) +
  geom_abline(slope = coefs_lr_ae["Active_pct_change"], # add regression line
              intercept = coefs_lr_ae["(Intercept)"]) + 
  geom_text_repel(aes(label = Area), vjust = -0.7, size = 3, 
                  show.legend = FALSE) + # make areanames readable
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8, 
             linetype = "dashed") +
  coord_cartesian(xlim = c(-5, 55), ylim = c(-5,55)) + # make axes same size
  labs(title = 'Figure 3.2: Areas by Growth in Enteprises and Employment', 
       x = 'Enterprises % Change', y = 'Employment % Change', 
       color = 'Geo Grouping') # Add label for legend


# Line graph, enterprise growth for each area with region line
areas %>%
  filter(! Area %in% c('Yorks and Humber', 'North Yorks County', 'South Yorks Met',
                       'West Yorks Met')) %>%
  ggplot(aes(x = Year, y = Active_pct_change, color = Geo_Grouping, group = Area)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.8, linetype = 'dashed') + # add line at zero
  geom_line(data = yh, aes(x = Year, y = Active_pct_change), # plot region line
            colour = 'black', linewidth = 1) +
  labs(title = "Figure 3.3: Enterprises - % Change by Area 2009-2022",
       y = "% Change vs 2009", x = "Year", color = 'Geo Grouping',
       # add caption 
       caption = 'The solid black line represents the performance of the region, the dashed line is at zero.') + 
  theme(plot.caption = element_text(hjust = -0.0, face="italic")) # modify caption

# Line graph, enterprise growth for each area with region line
areas %>%
  filter(! Area %in% c('Yorks and Humber', 'North Yorks County', 'South Yorks Met',
                       'West Yorks Met')) %>%
  ggplot(aes(x = Year, y = Employees_pct_change, color = Geo_Grouping, group = Area)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  geom_line(data = yh, aes(x = Year, y = Employees_pct_change), colour = 'black', linewidth = 1) +
  labs(title = "Figure 3.4: Employment - % Change by Area 2009-2022",
       y = "% Change vs 2009", x = "Year", color = 'Geo Grouping',
       # add caption
       caption = 'The solid black line represents the performance of the region, the dashed line is at zero.') + 
  theme(plot.caption = element_text(hjust = -0.0, face="italic")) # modify caption



