
# ====== R Scipt for take home exercise solution === 
renv::init()

#Task 1: Data import
library(readr)
getwd()
covid_cases <- readRDS("data/covid_cases.rds")

##Task 2: Simple computations using dataset
covid_cases$date <- as.Date(covid_cases$date, format = "%d/%m/%Y") # transfer the data of date column to Date type
class(covid_cases$date) # check the data type of date column before compute

#the earliest date of data report in the dataset
first_report_date <- min(covid_cases$date, na.rm = TRUE)
first_report_date
# the latest date of data report in the dataset
last_report_date <- max(covid_cases$date, na.rm = TRUE)
last_report_date

#Create new column case_global for covid_cases, which represents the total cases across every country per report day
covid_cases[["case_global"]] <- rowSums(covid_cases[,!names(covid_cases) %in% "date"])
library(dplyr)
covid_cases <-  covid_cases %>% 
  mutate(case_global = rowSums(covid_cases[,-1], na.rm = TRUE)) #sum for all row, except column 1 (date)
covid_cases <-  covid_cases %>% 
  mutate(case_global = rowSums(select(.,-date)), na.rm = TRUE)

print(covid_cases$case_global)
print(covid_cases)

#Create new column percent_chn in covid_cases, which represents the percentage of global cases that China’s cases account for per report day.
covid_cases[["percent_chn"]] <- covid_cases$cases_chn/covid_cases$case_global *100
print(covid_cases)

###Task 3
#Create a function compute_percent that takes the dataset and the country code as input and return the percentage of the global cases that the given country’s cases account for per report day.

compute_percent <- function(data,country_code){
  country_collumn <- paste0("cases_", country_code)
  percent_column <- round(data[[country_collumn]]/data[["case_global"]] *100, 2) #use function round() to round the result to 2 numbers after decimal
  return(percent_column)
}
covid_cases$percent_vnm <- compute_percent(covid_cases, "vnm")
covid_cases$percent_usa <- compute_percent(covid_cases, "usa")
covid_cases$percent_sgp <- compute_percent(covid_cases, "sgp")
covid_cases$percent_chn <- compute_percent(covid_cases, "chn")
print(covid_cases)

compute_case_percent <- function(data,case_code){
  case_col <- paste("cases_", case_code, sep ="")
  if(!(case_col %in% colnames(data))) {
    stop(paste("Invalid country code. Column", case_col, "not found."))}
  if(!("case_global" %in% colnames(data))){
    warning("case_global not found, computing case_global from all 'cases_' columns.")
    data[["case_global"]] <- rowSums(data[,grep("^cases_", colnames(data), ignore.case=FALSE, value= TRUE)], na.rm=TRUE)
  }
  percent <- round(data[[case_col]]/data[["case_global"]]*100, 2)
  return(percent)
}
covid_cases$vnm_percent <- compute_case_percent(covid_cases, "vnm")
covid_cases$usa_percent <- compute_case_percent(covid_cases, "usa")
covid_cases$sgp_percent <- compute_case_percent(covid_cases, "sgp")
covid_cases$sgp_percent <- compute_case_percent(covid_cases, "chn")

#Print the final covid_cases selecting only the following columns date, percent_chn, percent_vnm, percent_usa, percent_sgp
percent_table <- data.frame(covid_cases$date, covid_cases$percent_vnm,covid_cases$percent_usa, covid_cases$percent_sgp)
print(percent_table)

print(covid_cases[,c("date","percent_vnm", "percent_usa", "percent_sgp", "percent_chn")])

###Task 3.5
#Create a function that returns the plot for number of cases reported for a country over time
install.packages("tidyverse")
library(ggplot2)

plot <- function(data, plot_col, country, col_color, line_color){
  ggplot(data, aes(x=.data[["date"]], y=.data[[plot_col]])) + 
    geom_col(fill = col_color) +
    geom_line(color = line_color) + 
    scale_x_date(limits = as.Date(c(first_report_date, last_report_date))) + 
    labs(y="Cases", x ="Date", title = paste0("Reported Covid cases for ", country))
  }
##plot for number of cases reported for a China over time
plot_china <- plot(covid_cases, "cases_chn", "China", "cornflowerblue","red")
plot_china

##plot for number of cases reported for a Vietnam over time
plot_vietnam <- plot(covid_cases, "cases_vnm", "Vietnam", "lightgreen", "orange")
plot_vietnam

##plot for number of cases reported for a Singapore over time
plot_sing <- plot(covid_cases, "cases_sgp", "Singapore", "lightblue", "darkblue")
plot_sing

####Task 4: Generate data summary
install.packages("skimr")
library(skimr)
skim_table <- skim(covid_cases, cases_chn, cases_vnm, cases_usa, cases_sgp)
skim_table
