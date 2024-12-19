# ====== R Scipt for take home exercise solution === 

# task 1: data import 
covid_cases <- readRDS("./day1/data/covid_cases.rds")

# task 2 

# earliest and lastest date of data report
first_report_date <- min(covid_cases$date)
last_report_date <- max(covid_cases$date)

# create new column case_global
covid_cases$case_global <-  rowSums(covid_cases[,-1])

# create new column percent_chn
covid_cases$percent_chn <-  covid_cases$cases_chn*100/covid_cases$case_global

# task 3
# create function
compute_percent <- function(data,country){
  percentage <- covid_cases[[paste("cases_",country,sep = "")]]/data$case_global
  return(percentage*100)
} 

covid_cases$percent_vnm <- compute_percent(data = covid_cases, country = "vnm")
covid_cases$percent_usa <- compute_percent(data = covid_cases, country = "usa")
covid_cases$percent_sgp <- compute_percent(data = covid_cases, country = "sgp")

print(covid_cases[,c("date","percent_chn","percent_vnm","percent_usa","percent_sgp")])

# task 3.5

library(ggplot2)

plot_covid <- function(plot_col,country,col_bar,col_line,period){
  ggplot() +
    geom_col( # layer for bar chart
      aes( # define columns for x, y axis
        x = covid_cases$date, # this is equivalent to covid_cases[["date"]]
        y = covid_cases[[plot_col]] 
      ), 
      fill = col_bar # choose color for bar chart
    ) +
    geom_line( # layer for line chart
      aes( # define columns for x, y axis
        x = covid_cases$date, 
        y = covid_cases[[plot_col]] 
      ), 
      color = col_line # choose color for line chart
    ) +
    labs(
      y = "Cases",
      x = "Date",
      title = paste0("Reported Covid cases for ", country) # define title for the plot
    )+
    xlim(period)
}

## standard plot
plot_covid(plot_col = "cases_chn",
           country = "China",
           col_bar = "cornflowerblue",
           col_line = "red",
           period = c(first_report_date,last_report_date))

## flexible plot
plot_covid(plot_col = "cases_vnm",
           country = "Vietnam",
           col_bar = "red",
           col_line = "cornflowerblue",
           period = c(first_report_date+50,last_report_date-10))


# task 4

# install.packages("skimr")
library(skimr)

# China
skim(covid_cases$cases_chn)
## 92 observations, 0 missing value, only numeric varriable
## mean = 913 cases, standard deviation = 2207 cases
## 25th percentile = 65.5 cases, median = 128 cases, 75th  = 950 cases
## right skewed distribution


# Vietnam
skim(covid_cases$cases_vnm)
## 92 observations, 0 missing value, only numeric varriable
## mean = 2.91 cases, standard deviation = 4.53 cases
## 25th percentile = 0 cases, median = 1 cases, 75th  = 4 cases
## right skewed distribution


# USA
skim(covid_cases$cases_usa)
## 92 observations, 0 missing value, only numeric varriable
## mean = 8166 cases, standard deviation = 12331 cases
## 25th percentile = 0 cases, median = 12 cases, 75th  = 18014 cases
## right skewed distribution


# Singapore
skim(covid_cases$cases_sgp)
## 92 observations, 0 missing value, only numeric varriable
## mean = 87.1 cases, standard deviation = 217 cases
## 25th percentile = 0 cases, median = 7.5 cases, 75th  = 51.2 cases
## right skewed distribution



