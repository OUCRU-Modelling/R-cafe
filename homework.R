library(dplyr)
#TASK 1
#Data import
covid_cases<-readRDS("day1/data/covid_cases.rds")

#TASK 2
#The earliest and latest date of data report in the dataset and assign them to 2 variables first_report_date, last_report_date
class(covid_cases$date)=="Date"
class(covid_cases$date)
first_report_date<- min(covid_cases$date, na.rm=TRUE)
last_report_date<-max(covid_cases$date, na.rm=TRUE)

#Create new column case_global for covid_cases, which represents the total cases across every country per report day.
numeric_columns <- sapply(covid_cases, is.numeric)
covid_cases$case_global<-rowSums(covid_cases[, numeric_columns], na.rm=TRUE)
covid_cases$percent_chn< (covid_cases$case_chn/covid_cases$case_global)*100

#TASK 3: Create a function
compute_percent<- function(data, country_code){
country_col<-paste0("cases_", country_code)
percent_col<-paste0("percent", country_code)
data[[percent_col]]<-(data[[country_col]]/data$case_global)
return (data)
}
covid_cases<-compute_percent(covid_cases, "vnm")
covid_cases<-compute_percent(covid_cases, "usa")
covid_cases<-compute_percent(covid_cases, "sgp")
print(covid_cases[, c("date", "percentvnm", "percentusa", "percentsgp")])
colnames(covid_cases)


#Task 3.5: Create a function from given code

# install.packages("tidyverse")
library(ggplot2)
plot_col <- "cases_chn"
country <- "China"

ggplot() +
  geom_col( # layer for bar chart
    aes( # define columns for x, y axis
      x = covid_cases$date, # this is equivalent to covid_cases[["date"]]
      y = covid_cases[[plot_col]] 
      ), 
    fill = "cornflowerblue" # choose color for bar chart
  ) +
  geom_line( # layer for line chart
    aes( # define columns for x, y axis
      x = covid_cases$date, 
      y = covid_cases[[plot_col]] 
      ), 
    color = "red" # choose color for line chart
  ) +
  labs(
    y = "Cases",
    x = "Date",
    title = paste0("Reported Covid cases for ", country) # define title for the plot
  )

#TASK4: GENERATE DATA SUMMARY
install.packages("skimr")
library (skimr)
skim(covid_cases, cases_chn, cases_vnm, cases_usa, cases_sgp)
skim(covid_cases[c("cases_chn", "cases_vnm", "cases_usa", "cases_sgp")])


