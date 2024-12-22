# ====== R Scipt for take home exercise solution === 

library(dplyr)
library(ggplot2)

### Task 1
df <- readRDS("~/GitHub/R-cafe/day1/data/covid_cases.rds")

### Task 2
first_report_date <- min(df$date)

last_report_date <- max(df$date)

full_dates <- seq(min(df$date), max(df$date), by = "day")

df <- merge(data.frame(date = full_dates), df, by = "date", all.x = TRUE)

df[is.na(df)] <- 0

df$case_global <- rowSums(df[,-1])

df <- df %>%
  mutate(percent_chn = cases_chn/case_global)

### Task 3

compute_percent <- function(data, country_code) {
  
  country_col <- paste0("cases_", country_code)
  
  percent_col <- paste0("percent_", country_code)
  
  data[[percent_col]] <- (data[[country_col]]/data$case_global)
  
  return(data)
}

df <- compute_percent(df, "vnm")
df <- compute_percent(df, "usa")
df <- compute_percent(df, "sgp")
print(df[,c("date", "percent_vnm", "percent_usa", "percent_sgp")])


### Task 3.5

# Uncomment and run the following if you have not installed tidyverse or ggplot2
# install.packages("tidyverse")

plot_case <- function(data, country_code, country,
                      color_col, color_line,
                      t1, t2){
  
  plot_col <- paste0("cases_", country_code)
  
  ggplot() +
    geom_col( # layer for bar chart
      aes( # define columns for x, y axis
        x = data$date, # this is equivalent to covid_cases[["date"]]
        y = data[[plot_col]] 
      ), 
      fill = color_col # choose color for bar chart
    ) +
    geom_line( # layer for line chart
      aes( # define columns for x, y axis
        x = data$date, 
        y = data[[plot_col]] 
      ), 
      color = color_line # choose color for line chart
    ) +
    labs(
      y = "Cases",
      x = "Date",
      title = paste0("Reported Covid cases for ", country) # define title for the plot
    ) +
    scale_x_date(
      limits = c(t1, t2)) + 
    theme_classic()
}

plot_case(data = df, country_code = "vnm", country = "Vietnam",
          color_col = "lightblue", color_line = "red",
          t1 = first_report_date, t2 = last_report_date)

### Task 4
skimr::skim(df[,c("cases_chn", "cases_vnm", "cases_usa", "cases_sgp")])

## China (Mean = 913 cases, sd = 2207, p50 = 128, 0 missing)
## Vietnam (Mean = 2.91 cases, sd = 4.53, p50 = 1, 0 missing)
## USA (Mean = 8166 cases, sd = 12331, p50 = 12, 0 missing)
## Singapore (Mean = 87.1 cases, sd = 217, p50 = 7.5, 0 missing)