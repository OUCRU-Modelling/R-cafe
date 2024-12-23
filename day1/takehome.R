# ====== R Scipt for take home exercise solution === 
renv::restore()
#Task 1
covid_cases <- readRDS("day1/data/covid_cases.rds")
class(covid_cases$date)

##Task 2
#the earliest date of data report in the dataset
first_report_date <- min(covid_cases$date, na.rm = TRUE)
first_report_date
# the latest date of data report in the dataset
last_report_date <- max(covid_cases$date, na.rm = TRUE)
last_report_date

#Create new column case_global for covid_cases, which represents the total cases across every country per report day
covid_cases[["case_global"]] <- rowSums(covid_cases[,!names(covid_cases) %in% "date"])
print(covid_cases$case_global)
print(covid_cases)
#Create new column percent_chn in covid_cases, which represents the percentage of global cases that China’s cases account for per report day.
covid_cases[["percent_chn"]] <- covid_cases$cases_chn/covid_cases$case_global *100
print(covid_cases)

###Task 3
#Create a function compute_percent that takes the dataset and the country code as input and return the percentage of the global cases that the given country’s cases account for per report day.

compute_percent <- function(covid_cases,country_code){
  country_collumn <- paste0("cases_", country_code)
  percent_column <- covid_cases[[country_collumn]]/covid_cases[["case_global"]] *100
  return(percent_column)
}
covid_cases$percent_vnm <- compute_percent(covid_cases, "vnm")
covid_cases$percent_usa <- compute_percent(covid_cases, "usa")
covid_cases$percent_sgp <- compute_percent(covid_cases, "sgp")
covid_cases$percent_sgp <- compute_percent(covid_cases, "chn")
print(covid_cases)

#Print the final covid_cases selecting only the following columns date, percent_chn, percent_vnm, percent_usa, percent_sgp
print(covid_cases$date)
percent_table <- data.frame(covid_cases$date, covid_cases$percent_vnm,covid_cases$percent_usa, covid_cases$percent_sgp)
print(percent_table)

###Task 3.5
#Create a function that returns the plot for number of cases reported for a country over time
install.packages("tidyverse")
library(ggplot2)
##plot for number of cases reported for a China over time
plot_col <- "cases_chn"
country <- "China"
ggplot() + geom_col(aes(x=covid_cases$date, y=covid_cases[[plot_col]]), fill ="cornflowerblue"
                    ) + geom_line(aes(x=covid_cases$date, y=covid_cases[[plot_col]]), color = "red"
                    ) + scale_x_date(limits = as.Date(c(first_report_date, last_report_date))
                                  ) + labs(y="Cases", x ="Date", title = paste0("Reported Covid cases for ", country))
                                  
##plot for number of cases reported for a Vietnam over time
plot_col2 <-"cases_vnm"
country2 <- "Vietnam"
ggplot() + geom_col(aes(x=covid_cases$date, y=covid_cases[[plot_col2]]), fill ="lightblue"
) + geom_line(aes(x=covid_cases$date, y= covid_cases[[plot_col2]]), color = "orange"
) + labs(y="Cases", x = "Date", title =paste0("Reported Covid cases for ", country2))
##plot for number of cases reported for a Singapore over time
plot_col3 <- "cases_sgp"
country3 <- "Singapore"
ggplot() + geom_col(aes(x=covid_cases$date, y= covid_cases[[plot_col3]]), fill = "lightgreen"
) + geom_line(aes(x=covid_cases$date, y = covid_cases[[plot_col3]]), color = "darkred"
)+ labs(y ="Cases", x = "Date", title =paste0("Reported Covid cases for", country3))

####Task 4: Generate data summary
install.packages("skimr")
library(skimr)
ls("package:skimr")
data_china <- data.frame(
  cases_chn = covid_cases$cases_chn,
  percent_chn = covid_cases$percent_chn
)
china_skim <- skim(data_china)
print(china_skim)
#Notable Observations for "China":
#The average number of case is 913 cases, China accounts for 39% of global cases
##There is big variation as the standard deviation is big
data_vietnam <- data.frame(
  cases_chn = covid_cases$cases_vnm,
  percent_chn = covid_cases$percent_vnm
)
vietnam_skim <- skim(data_vietnam)
print(vietnam_skim)
#The average number of case is 3 cases, Vietnam account for 0.01% of global cases
data_usa <- data.frame(
  cases_usa = covid_cases$cases_usa,
  percent_usa = covid_cases$percent_usa
)
usa_skim <- skim(data_usa)
print(usa_skim)
#The average number of case is 8166 cases, USA accounts for 6.16% of global cases
data_sgp <- data.frame(
  cases_sgp = covid_cases$cases_sgp,
  percent_sgp = covid_cases$percent_sgp
)
sgp_skim <- skim(data_sgp)
print(sgp_skim)
#The average number of case is 87 cases, Singapore accounts for 6.16% of global cases
# Task 1: read the data
file_path <- "R-cafe/day1/data/covid_cases.rds"
covid_cases <- readRDS(file_path)
#Task 2: Simple computations using dataset
first_report_date <- min(covid_cases$report_date, na.rm = TRUE)
last_report_date <- max(covid_cases$report_date, na.rm = TRUE)
mutate(case_global = sum(cases, na.rm = TRUE))
case_global = sum(cases, na.rm = TRUE)
percent_chn = ifelse(country == "China", (cases / case_global) * 100, NA)
#Task 3: Create a function
compute_percent <- function(dataset, country_code)
country_col <- paste0("cases_", country_code)  
percent_col <- paste0("percent_", country_code)
#return
file_path <- "R-cafe/day1/data/covid_cases.rds"
covid_cases <- readRDS(file_path)
percent_chn <- compute_percent(covid_cases, "chn")
#create 3 new columnes
covid_cases <- compute_percent(covid_cases, "vnm")
covid_cases <- compute_percent(covid_cases, "usa")
covid_cases <- compute_percent(covid_cases, "sgp")
# Print the selected columns
final_covid_cases <- covid_cases 
select(report_date, percent_chn, percent_vnm, percent_usa, percent_sgp)
print(final_covid_cases)
#task 4
skim_summary <- covid_cases, c(cases_chn, cases_vnm, cases_usa, cases_sgp)
