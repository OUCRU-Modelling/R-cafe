# ====== R Scipt for take home exercise solution === 

library(dplyr)
library(ggplot2)

df <- readRDS("~/GitHub/R-cafe/day1/data/covid_cases.rds")

skimr::skim(df)

first_report_date <- min(df$date)

last_report_date <- max(df$date)

full_dates <- seq(min(df$date), max(df$date), by = "day")

df <- merge(data.frame(date = full_dates), df, by = "date", all.x = TRUE)

df[is.na(df)] <- 0

df$global_cases <- rowSums(df[,-1])

df <- df %>%
  mutate(percent_chn = cases_chn/global_cases)
