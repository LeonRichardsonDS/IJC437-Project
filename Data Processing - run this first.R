library(tidyverse)
library(readxl)

### Code for processing data

## Process Employment Data
# Yorkshire employment dataset (pre-processed)
Yorkshire_Employment_R <- read_excel("Employment Data.xlsx")

# Create long form of Employment Data
emp_long <- Yorkshire_Employment_R %>%
  pivot_longer(
    cols = 3:ncol(Yorkshire_Employment_R), # assumes first 2 columns are Year, Area
    names_to = "Industry",
    values_to = "Employees")

# Convert employee values to numeric
emp_long <- emp_long %>%
  mutate(Employees = as.numeric(Employees))

# New dataset without industry data so it can be combined with business 
# demography data
emp <- emp_long %>%
  filter(Industry == 'Area Total')

### Process Business Demography Data
BD_Data_For_R <- read_excel("Business Demography Data.xlsx")

# Create long form of business demography data
bd_long <- BD_Data_For_R %>%
  pivot_longer(
    cols = 3:ncol(BD_Data_For_R), # assumes first 2 columns are Year, Area
    names_to = "Year",
    values_to = "Values")

# Convert values to numeric and round 
bd_long <- bd_long %>%
  mutate(Values = as.numeric(Values)) %>%
  mutate(Values = round(Values, digits = 0))

# Create shorter dataset with births, deaths, active as columns
bd <- bd_long %>%
  pivot_wider(names_from  = Category,
              values_from = Values,
              names_sep = "-")

# Add new columns (Net births/deaths, Births and deaths as % of active)
bd <- bd %>%
  mutate(Net = Births - Deaths) %>%
  mutate(Net_pct = (Births - Deaths) / Active * 100) %>%
  mutate(Births_pct = Births / Active * 100) %>%
  mutate(Deaths_pct = Deaths / Active * 100)

### Combine employment and demography to create areas df
# Order datasets so they can be joined
bd <- bd %>%
  arrange(Year, Area)
emp <- emp %>%
  arrange(Year, Area)


"
Database 1 is called areas 
"
# Join datasets
areas <- cbind(emp, bd) 

# Remove Columns - select wouldn't work for duplicates
areas <- subset(areas, select = -c(Industry))
areas <- subset(areas, select = -c(Year.1))
areas <- subset(areas, select = -c(Area.1))

# Get baseline (2009) enterprise and employees values for each area
baseline <- areas %>%
  filter(Year == 2009) %>%
  select(Area, baseline_active = Active, baseline_employees = Employees) 

# Add baseline values so percentage can be calculated
areas <- areas %>%
  left_join(baseline, by = "Area") %>%
  mutate(Active_pct_change = (Active - baseline_active) / baseline_active * 100, 
         Employees_pct_change = (Employees - baseline_employees) / baseline_employees * 100)

# Remove baseline columns
areas <- subset(areas, select = -c(baseline_active, baseline_employees))

# Add County column based County Classification
areas <- areas %>%
  mutate(County = case_when(
    Area %in% c('Yorks and Humber') ~ 'Region',
    Area %in% c('North Yorks County', 'South Yorks Met', 'West Yorks Met') ~ 'County Total',
    Area %in% c('Craven', 'Hambleton', 'Harrogate', 'Richmondshire', 'Ryedale', 'Scarborough','Selby') ~ 'North Yorks County',
    Area %in% c('Barnsley', 'Doncaster', 'Rotherham', 'Sheffield') ~ 'South Yorks Met County',
    Area %in% c('Bradford', 'Calderdale', 'Kirklees', 'Leeds', 'Wakefield') ~ 'West Yorks Met County',
    Area %in% c('ER Yorks', 'Hull', 'NE Lincs', 'N Lincs', 'York') ~ 'No County'))

# Add Geographical Grouping Column
areas <- areas %>%
  mutate(Geo_Grouping = case_when(
    Area %in% c('Craven', 'Hambleton', 'Harrogate', 'Richmondshire', 'Ryedale', 'Scarborough', 'Selby', 'York') ~ 'North Yorks County and York',
    Area %in% c('Barnsley', 'Doncaster', 'Rotherham', 'Sheffield') ~ 'South Yorks Met County',
    Area %in% c('Bradford', 'Calderdale', 'Kirklees', 'Leeds', 'Wakefield') ~ 'West Yorks Met County',
    Area %in% c('ER Yorks', 'Hull', 'NE Lincs', 'N Lincs') ~ 'East Yorks and Humber',
    Area %in% c('North Yorks County', 'South Yorks Met', 'West Yorks Met') ~ 'County Total',
    Area %in% c('Yorks and Humber') ~ 'Region'))


### Create area Summary df, with summary statistics for all areas
# Add employees per enterprise to main df so it can be summarised
areas <- areas %>%  
  mutate(emps_per_ent = Employees/Active)

# Build summary df
areas_summary <- areas %>%
  group_by(Area) %>%
  summarise(
    avg_births = mean(Births), avg_deaths = mean(Deaths),
    avg_births_pct = mean(Births_pct), avg_deaths_pct = mean(Deaths_pct),
    avg_net = mean(Net), avg_net_pct = mean(Net_pct),
    avg_emps_per_ent = mean(emps_per_ent)) %>%
  arrange(Area)

# Get enterprise percentage growth from areas
active_22 <- areas %>%
  filter(Year == 2022) %>%
  arrange(Area) %>%
  select(Active_pct_change, Employees_pct_change)

# Add enterprise percentage change to area summary df 
areas_summary <- cbind(areas_summary, active_22)

# Subset summary df to 21 areas used for study
study_areas_summary <- areas_summary %>% filter(! Area %in% c('Yorks and Humber', 
                                                              'North Yorks County', 'South Yorks Met', 'West Yorks Met'))

### Add percentage change in employees by area and industry to  emp_long
# Create baseline df
baseline_industry_values <- emp_long %>%
  filter(Year == 2009) %>% # just first year
  select(Industry, Area, baseline_value = Employees) # get baseline values

# Form final df
emp_long <- emp_long %>%
  left_join(baseline_industry_values, by = c("Industry", "Area")) %>% # join with baseline df, duplicating baselines values for every year
  mutate(pct_change = (Employees - baseline_value) / baseline_value * 100) # create % change column using baseline



"
Database two is called ind_09 (industries in 09)
"

# Create wider df for PCA in RQ2
ind_09 <- emp_long %>%
  filter(Year == 2009, Industry != 'Area Total', ! Area %in% c('Yorks and Humber', 
                                                               'North Yorks County', 'South Yorks Met', 'West Yorks Met')) %>%
  
  select(Industry, Area, Employees) %>%
  # shorten industry names for heatmap
  mutate(Industry = recode(Industry,  "Agriculture, forestry & fishing" = "Agriculture", 
                           "Mining, quarrying & utilities" = "Mining & utilities",
                           "Manufacturing" = "Manufacturing",
                           "Construction" = "Construction",
                           "Motor trades" = "Motor trades",
                           "Wholesale" = "Wholesale",
                           "Retail" = "Retail",
                           "Transport & storage" = "Transport",
                           "Accommodation & food services" = "Accomm & food",
                           "Information & communication" = "Info & comms",
                           "Financial & insurance" = "Finance",
                           "Property" = "Property",
                           "Professional, scientific & technical" = "Prof services",
                           "Business administration & support services" = "Business support",
                           "Public administration & defence" = "Public admin",
                           "Education" = "Education",
                           "Health" = "Health",
                           "Arts, entertainment, recreation & other services" = "Arts & recreation")) %>%
  pivot_wider(names_from = Industry, values_from = Employees) %>%
  as.data.frame() # so rownames can be changed


