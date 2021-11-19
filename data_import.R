# This script the complete cov_dataID data set from OWID, extracts the variable needed fro modeling of the 
# daily cases, mortalities and hospitalizations for the Europe's countries

# tools -----

  library(plyr)
  library(tidyverse)
  library(stringi)

# data container -----

  cov_data <- list()
  
# reading the raw OWID dataset ---
  
  cov_data$raw <- try(read_csv(file = 'https://covid.ourworldindata.org/data/owid-covid-data.csv'), silent = TRUE)
  
  if(any(class(cov_data$raw) == 'try-error')){
    
    warning('Data server access denied, retrieving the data locally', call. = FALSE)
    message('Data server access denied, retrieving the data locally')
    
    cov_data$raw <- read_csv(file = 'owid-covid-data.csv')
    
  } 
  
# clearing ----
  
  ## selecting what I need for Europe, other interesting variables like booster vaccinations
  ## are unfortunately too messy!
  
  cov_data$data <- cov_data$raw %>% 
    filter(continent == 'Europe') %>% 
    select(iso_code, 
           location, 
           date, 
           total_cases_per_million, 
           new_cases_per_million, 
           new_deaths_per_million, 
           people_fully_vaccinated_per_hundred)
  
  ## new cases are dependent on the cases registered on former days - up to 14 days
  ## adding such lag variables
  ## removing the incomplete observations per country
  
  cov_data$data <- cov_data$data %>% 
    group_by(location) %>% 
    add_lag_var(src_variable = 'new_cases_per_million', 
                lags = 1:14) %>% 
    group_by(location) %>% 
    filter(!is.na(total_cases_per_million), 
           !is.na(new_cases_per_million), 
           !is.na(new_deaths_per_million)) %>% 
    ungroup
  
  ## adding the year and day-of-the-year information
  
  cov_data$data <- cov_data$data %>% 
    mutate(date_chr = as.character(date), 
           year = stri_extract(date_chr, regex = '2020|2021'), 
           year = factor(year, c('2020', '2021')), 
           day = ifelse(year == '2020', 
                        date - as.Date('2020-01-01'), 
                        date - as.Date('2021-01-01')))
  
# some globals ----
  
  cov_data$countries <- unique(cov_data$data$location)
  
  cov_data$responses <- c(cases = 'new_cases_per_million', 
                          fatal = 'new_deaths_per_million')
  
  cov_data$indep_vars <- c('day', 
                           'total_cases_per_million', 
                           paste('new_cases_per_million_lag', 1:14, sep = '_'))
  
# END -----