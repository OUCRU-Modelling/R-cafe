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
