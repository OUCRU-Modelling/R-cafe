# ====== R Scipt for take home exercise solution === 
#Task 1
#Read data
covid_cases <- readRDS("~/Documents/GitHub/R-cafe/day1/data/covid_cases.rds")
str(covid_cases)
# Task 2
#earliest and latest date of data report in the dataset
class(covid_cases$date) == "Date"
first_report_date<- min(covid_cases$date, na.rm=TRUE)
first_report_date
last_report_date<- max(covid_cases$date, na.rm=TRUE)
last_report_date
class(covid_cases)
# Task 3
# Create new column case_global for covid_cases, which represents the total cases across every country per report day.
covid_cases <-covid_cases %>% mutate(case_global=rowSums(covid_cases[,2:212], na.rm=TRUE))
rowSums(covid_cases[,-1])

#covid_cases <-covid_cases %>% mutate(case_global=rowSums(across(where(is.numeric)), na.rm=TRUE))
#Create a function compute_percent that
#takes the dataset and the country code (i.e. the 3 characters after cases_ in column name) as input.
#return the percentage of the global cases that the given country’s cases account for per report day.
compute_percent <-function (data, col_name) {
  country_code<-substring(col_name, 7)
  percent_col_name<- paste("percent_", country_code, sep="")
  data[[percent_col_name]]<-(data[,col_name]/data[,"case_global"])*100
  return(data[,percent_col_name])
}
covid_cases$percent_chn<-compute_percent(data=covid_cases,col_name = "cases_chn")
covid_cases$percent_usa<-compute_percent(data=covid_cases,col_name = "cases_usa")
covid_cases$percent_vnm<-compute_percent(data=covid_cases,col_name = "cases_vnm")
covid_cases$percent_sgp<-compute_percent(data=covid_cases,col_name = "cases_sgp")
# Print the final covid_cases selecting only the following columns date, percent_chn, percent_vnm, percent_usa, percent_sgp
percent_country<- data.frame(covid_cases$date, covid_cases$percent_chn, covid_cases$percent_sgp,
                             covid_cases$percent_usa, covid_cases$percent_vnm)
percent_country

#Create a function that returns the plot for number of cases reported for a country over time, where users can

#Select the case column being plotted

#Change the country name displayed on the plot’s title

#Change the color of the bar chart and line chart

#Set the min and max date (x-axis limit) for the plot
library(ggplot2)
create_graph<- function (data, col_name, country_name){
  plot_col <- col_name
  country <- country_name
  
  ggplot() +
    geom_col( # layer for bar chart
      aes( # define columns for x, y axis
        x = data[["date"]], # this is equivalent to covid_cases[["date"]]
        y = data[[plot_col]] 
      ), 
      fill = "cornflowerblue" # choose color for bar chart
    ) +
    geom_line( # layer for line chart
      aes( # define columns for x, y axis
        x = data[["date"]], 
        y = data[[plot_col]] 
      ), 
      color = "red" # choose color for line chart
    ) +
    labs(
      y = "Cases",
      x = "Date",
      title = paste0("Reported Covid cases for ", country) # define title for the plot
    )
}
create_graph(data=covid_cases, col_name = "cases_chn", country_name = "Chinna")

#Task 4
# Use the function skim() from package skimr to generate summary for the following countries: China, Vietnam, USA, Singapore
install.packages("skimr")
library(skimr)
skim(covid_cases, cases_chn, cases_vnm, cases_usa, cases_sgp)
china_summary<- skim(covid_cases$cases_chn)
# Chinna: Mean 913 (0-19461)
vietnam_summary<- skim(covid_cases$cases_vnm)
# Vietnam: Mean 3 (0-19)
usa_summary<-skim(covid_cases$cases_usa)
# USA: Mean 8166 (0-35386)
sing_summary<- skim(covid_cases$cases_sgp)
# Singapore: Mean 87 (0-1462)