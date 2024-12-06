# ----- DATA IMPORT----- 
# You don't have to worry about this part for now
covid_cases <- readRDS("day1/data/covid_cases.rds")
# ----- END DATA IMPORT----- 


# ====== IN-CLASS EXERCISES TEMPLATE CODE ======== 
# Code to compute basic stats for number of case reports in China
case_col <- "cases_chn"

basic_stats <- c(
  min =  min(covid_cases[[case_col]]),
  q1 = quantile(covid_cases[[case_col]], 0.25, names=FALSE),
  median = median(covid_cases[[case_col]]),
  q3 = quantile(covid_cases[[case_col]], 0.75, names=FALSE),
  max = max(covid_cases[[case_col]])
)

basic_stats
# ====== END IN-CLASS EXERCISES TEMPLATE CODE ======== 

# ====== ADD YOUR CODE HERE ========
# TODO: Create a function to compute basics stats for a given column

