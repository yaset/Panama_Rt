####################################
###################################
###################################
##### LOAD DATA ###################
###################################
###################################


####DATABASE#####
#### This files are copy of least update of CSSE-COVID19 from JH
#### Direction of repository https://github.com/CSSEGISandData/COVID-19

#### LOAD DATA ######
### Update with repository. You should clone repository of JH and make a fixed route
data_reported <- read_csv("data/time_series_covid19_confirmed_global.csv")
data_death <- read_csv("data/time_series_covid19_deaths_global.csv")
#####Functions countries
### Countries: To fix original database for each country

countries <- function(name_country){ #### Names of countries  
  database <- as.character(name_country)# Name
  
  database_2 <- database
  database_2 <- data_reported %>% #filter from original database
    filter(`Country/Region` == database)
  
  
  
  data_base2 <- database_2 %>%
    gather(`Country/Region`)  # Fixing data
  data_base2 <- data_base2 %>% # Select dates and rename original variable to "dates"
    slice(4:length(data_base2$value)) %>%
    rename(dates = `Country/Region`)
  
  data_base2$dates <- format(as.Date(data_base2$dates, format = "%m/%d/%Y"), "%Y-%m-%d") ## Date format YYYY-MM-DD
  
  data_base2 <- data_base2 %>% ### Rename value variable to cumulative incidence
    rename(I_a = value)
  
  data_base2 <- as.data.frame(data_base2) ### Declare database as dataframe object
  data_base2$I_a <- as.numeric(data_base2$I_a)
  
  for(i in 2:length(data_base2$I_a)){ ### Calculate Number of cases for each day
    data_base2$I[1] = data_base2[1,2]
    data_base2$I[i] = data_base2$I_a[i] - data_base2$I_a[i-1]
  }
  
  
  
  return(data_base2)
  
}

countries_death <- function(name_country){
  database <- as.character(name_country)
  
  database_2 <- database
  database_2 <- data_death %>%
    filter(`Country/Region` == database)
  
  data_base2 <- database_2 %>%
    gather(`Country/Region`)  # Fixing data
  data_base2 <- data_base2 %>% # Select dates and rename original variable to "dates"
    slice(4:length(data_base2$value)) %>%
    rename(dates = `Country/Region`)
  
  data_base2$dates <- format(as.Date(data_base2$dates, format = "%m/%d/%Y"), "%Y-%m-%d") ## Date format YYYY-MM-DD
  
  data_base2 <- data_base2 %>% ### Rename value variable to cumulative incidence
    rename(I_a = value)
  
  data_base2 <- as.data.frame(data_base2) ### Declare database as dataframe object
  data_base2$I_a <- as.numeric(data_base2$I_a)
  
  for(i in 2:length(data_base2$I_a)){ ### Calculate Number of cases for each day
    data_base2$I[1] = data_base2[1,2]
    data_base2$I[i] = data_base2$I_a[i] - data_base2$I_a[i-1]
  }
  
  
  
  return(data_base2)
  
  return(database_2)
  
}


