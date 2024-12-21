# ====== R Script for take home exercise solution === 

install.packages("skimr")
skimr::skim(covid_cases)

# Task 1: Data import

covid_cases <- readRDS("~/GitHub/Workshop-R/day1/data/covid_cases.rds")

# Task 2: Simple computations using data set

first_report_date <- min(covid_cases$date) 
last_report_date <- max(covid_cases$date)

covid_cases$case_global <- rowSums(covid_cases[,-1])

covid_cases$percent_chn <- (covid_cases$cases_chn/covid_cases$case_global)*100

# Task 3: Create a function

compute_percent <- function(data, country_code) {
  
  country_col <- paste0("cases_", country_code)
  percent_col <- paste0("percent_", country_code)
  data[[percent_col]] <- (data[[country_col]]/data$case_global)
  
  return(data)
}

covid_cases <- compute_percent(covid_cases, "vnm")
covid_cases <- compute_percent(covid_cases, "usa")
covid_cases <- compute_percent(covid_cases, "sgp")
print(covid_cases[,c("date", "percent_vnm", "percent_usa", "percent_sgp")])


# Task 3.5: Create a function from given code

library(ggplot2)

plot_country <- function(data, country_code, country, color_bar, color_line,
                         min_date, max_date){
  
  plot_col <- paste0("cases_", country_code) # select the country
  
  ggplot() +
    labs(
      y = "Cases",
      x = "Date",
      title = paste0("Reported Covid cases for ", country) # title for the plot
    ) +
    geom_col( 
      aes( 
        x = data$date, 
        y = data[[plot_col]] 
      ), 
      fill = color_bar # choose color for bar chart
    ) +
    geom_line( 
      aes(
        x = data$date, 
        y = data[[plot_col]] 
      ), 
      color = color_line # choose color for line chart
    ) +
    scale_x_date( # set min and max date
      limits = c(as.Date(min_date), as.Date(max_date)),
      date_labels = "%Y-%m-%d"
    )
}

plot_country(
  data = covid_cases, 
  country_code = "usa", 
  country = "USA",
  color_bar = "lightpink", 
  color_line= "darkgreen",
  min_date = "2020-01-27", 
  max_date = "2020-04-20")


# Task 4: Generate data summary

skimr::skim(covid_cases[c("cases_chn", "cases_vnm", "cases_usa", "cases_sgp")])

# There were no missing values in these countries.
# The mean number of cases in the USA was the highest (8166 cases), the second highest was China (913 cases)
# The lowest average cases among these 4 countries was Vietnam with just ~ 3 cases.
# The USA also had the largest standard deviation (12 331) while Vietnam had the smallest (4.53).
# China had the highest median with 128 cases, while Vietnam had the lowest, with just 1 case.
# Singapore had moderate values in all categories.
# Histogram: All 4 countries had the left-skewed bar at the left, indicating that most values were low, 
# but the USA also had a bar on the right, showing that some days had higher cases counts, meaning that cases numbers fluctuated over time.




