#Task 1: Data import
library(readr)
covid_cases <- read_rds("data/covid_cases.rds")
#######################################################

#Task 2: Simple computations using dataset

#Check name, class and missing data of date
library(skimr)
skim(covid_cases)
    #name: date; class: Date; no missing => no more action
#Compute and assign the earliest data to first_report_date
first_report_date <- min(covid_cases$date)
#Compute and assign the latest data to last_report_date
last_report_date <- max(covid_cases$date)

#Create new column case_global which represents the total cases per report day
covid_cases$case_global <- rowSums(covid_cases[ , -1], na.rm = TRUE)

#Create new column percent_chn in covid_cases
covid_cases$percent_chn <- round(covid_cases$cases_chn/covid_cases$case_global*100,2)
##############################################

#Task 3: Create a function
compute_percent <- function (data, country_code) {
  col_name <- paste("cases_", country_code, sep = "")
  data[[col_name]]/data$case_global*100
}

compute_percent(data=covid_cases,country_code="chn") #test function

#create 3 new columns for covid_cases: percent_vnm, percent_usa, percent_sgp
covid_cases$percent_vnm <- compute_percent(data=covid_cases,country_code="vnm")
covid_cases$percent_usa <- compute_percent(data=covid_cases,country_code="usa")
covid_cases$percent_sgp <- compute_percent(data=covid_cases,country_code="sgp")

#Print the final covid_cases
print(covid_cases[,c("date","percent_chn", "percent_vnm", "percent_usa", "percent_sgp")])
###############################

#Task 3.5:Create a function
# Load ggplot2 library
library(ggplot2)

# Define the function create_plot
create_plot <- function(data, plot_col, country, bar_color = "cornflowerblue", line_color = "red", min_date = NULL, max_date = NULL) {
  # Check if plot_col exists in the data
  if (!plot_col %in% colnames(data)) {
    stop(paste("Invalid column name:", plot_col))
  }
  
  # Create the ggplot object
  plot <- ggplot() +
    geom_col( # Add bar chart
      aes(
        x = data$date, # x-axis: dates
        y = data[[plot_col]] # y-axis: cases for the selected country
      ),
      fill = bar_color # Set bar chart color
    ) +
    geom_line( # Add line chart
      aes(
        x = data$date, 
        y = data[[plot_col]]
      ),
      color = line_color # Set line chart color
    ) +
    labs(
      y = "Cases", # Label for y-axis
      x = "Date", # Label for x-axis
      title = paste0("Reported Covid cases for ", country) # Title of the plot
    )
  
  # Set x-axis limits if provided
  if (!is.null(min_date) & !is.null(max_date)) {
    plot <- plot + xlim(as.Date(min_date), as.Date(max_date)) # Limit x-axis to specified range
  }
  
  # Return the final plot
  return(plot)
}

# Test
create_plot(covid_cases, "cases_chn", "China", "blue", "green", "2020-01-01", "2020-12-31")

create_plot(covid_cases, "cases_chn", "China", "blue", "green")
#############################################################################

#Task 4: Generate data summary
library(skimr)
skim(covid_cases[,c("cases_chn","cases_vnm", "cases_usa","cases_sgp")])

#Note: In 4 countries:
#China and the USA: Both had significant spikes in number COVID-19 cases/day that shown by high maximum values and large SD.
#Vietnam had remarkable low case numbers with maximum 19 cases/day
#Singapore had moderate variability but generally low cases compared to countries like the USA.

