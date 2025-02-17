<<<<<<< HEAD
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
=======
# ====== R Script for take home exercise solution ======

#  ====== TASK 1  ======  
# hard code absolute paths (NOT RECOMMENDED)
covid_cases <- readRDS("/Users/anhptq/Desktop/R-cafe/day1/data/covid_cases.rds")

# relative path 
covid_cases <- readRDS("./day1/data/covid_cases.rds") 

# relative path with file.path
covid_cases <- readRDS(file.path(".", "day1", "data", "covid_cases.rds"))

# absolute path with getwd() and file.path
covid_cases <- readRDS(file.path(getwd(), "day1", "data", "covid_cases.rds"))

#  ====== TASK 2  ======

# ---- Compute first and last report date -----
# check datatype (optional, but recommended)
class(covid_cases$date) == "Date"

# compute earliest and latest date of data report 
first_report_date <- min(covid_cases$date, na.rm=TRUE)
last_report_date <- max(covid_cases$date, na.rm=TRUE)

# ------ Compute case_global -------- 
# select columns by indices
rowSums(covid_cases[, c(2:ncol(covid_cases)) ])
# or
rowSums(covid_cases[, -1])

rowSums(covid_cases[, !names(covid_cases) == "date"])
covid_cases[, !names(covid_cases) %in% "date"]

# select columns by grep
rowSums(covid_cases[,  grep("cases_", colnames(covid_cases)) ])

# create new column
covid_cases[["case_global"]] <- rowSums(covid_cases[,  -1])

# ------ Compute percent_chn -------- 
covid_cases[["percent_chn"]] <- covid_cases[["cases_chn"]]/covid_cases[["case_global"]]
# or 
covid_cases$percent_chn <- covid_cases$cases_chn/covid_cases$case_global*100

# Tip: use function round() to round the result to a specific decimal place
covid_cases$percent_chn <- round(covid_cases$cases_chn/covid_cases$case_global*100, 2)


# ====== TASK 3 ========
# Create a function
compute_percent <- function(data, country_code){
  
  # use paste to get case_column
  case_col <- paste("cases_", country_code, sep = "")
  
  # check whether the given country_code is valid
  if (! (case_col %in% colnames(data)) ){
    stop(paste("Invalid country code. Column", case_col, "not found."))
  }
  
  # check if case_global was computed 
  if (! ("case_global" %in% colnames(data)) ){
    warning("case_global not found, compute case_global instead")
    data[["case_global"]] <- data[,  grep("cases_", colnames(covid_cases)) ]
  }
  
  # compute percent and return
  data[[case_col]]/data[["case_global"]]*100
}

compute_percent(covid_cases, country_code = "aaa")

# Create 3 new columns using function
covid_cases[["percent_sgp"]] <- compute_percent(covid_cases, country_code = "sgp")
covid_cases[["percent_usa"]] <- compute_percent(covid_cases, country_code = "usa")
covid_cases[["percent_vnm"]] <- compute_percent(covid_cases, country_code = "vnm")

print(covid_cases[, c("date", "percent_chn", "percent_vnm", "percent_usa", "percent_sgp")])

# ======== TASK 3.5 ========= 
# Uncomment and run the following if you have not installed tidyverse or ggplot2
# install.packages("tidyverse")
library(ggplot2)

plot_cases <- function(data, plot_col = "cases_chn", country = "China", 
                       color = "red", fill = "cornflowerblue", min_date=NULL, max_date=NULL){
  
  if (! (plot_col %in% colnames(data)) ){
    stop(paste("Column", plot_col, "not found."))
  }
  
  min_date <- as.Date(ifelse(is.null(min_date), min(data$date, na.rm = TRUE), min_date))
  max_date <- as.Date(ifelse(is.null(max_date), max(data$date, na.rm = TRUE), max_date))

  ggplot() +
    geom_col( # layer for bar chart
      aes( # define columns for x, y axis
        x = data$date, # this is equivalent to covid_cases[["date"]]
        y = data[[plot_col]] 
      ), 
      fill = fill # choose color for bar chart
    ) +
    geom_line( # layer for line chart
      aes( # define columns for x, y axis
        x = data$date, 
        y = data[[plot_col]] 
      ), 
      color = color # choose color for line chart
    ) +
    xlim(min_date, max_date) +
    labs(
      y = "Cases",
      x = "Date",
      title = paste0("Reported Covid cases for ", country) # define title for the plot
    )
}

plot_cases(covid_cases, 
           plot_col = "cases_usa", country = "USA", color = "black", fill = "blueviolet",
           min_date = as.Date("2020-03-02"), max_date = as.Date("2020-04-30"))

# ======== TASK 4 ========= 
library(skimr)
skim(covid_cases, cases_chn, cases_vnm, cases_usa, cases_sgp)
# and list some observations
>>>>>>> main
