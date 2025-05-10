setwd("C:/Users/dhtha/OneDrive - Oxford University Clinical Research Unit/Documents/R-cafe/day 2")


#Task 1: data import
library(tidyverse)
covid_cases <- read_rds("data/covid_cases.rds")

#Task 2: Data cleaning and filtering
is_tibble(covid_cases) 
#result: false => change to tibble
covid_cases <- covid_cases %>%  
  pivot_longer(cols = -date, names_to = "country", 
               names_pattern = "cases_(.+)",
               values_to = "cases")
covid_cases
str(covid_cases)
library(skimr)
skim(covid_cases)
#filter: cases < 0 and "Filter the data so that we only have week 3-12 of 2020"
covid_cases %>%  filter(cases<0) %>%
  mutate(week = week(date)) %>%
  filter(cases > -1, week < 3 + 10)

#Task 3: data transformation
top_n <- 5 
top_countries <- covid_cases %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  slice_max(total_cases, n = top_n) %>%
  pull(country)

#Task 4: Data visulization
plot_data %>%
  ggplot(aes(x = date, y = pct_cases, fill = country)) +
  geom_area() +
  scale_y_continuous(
    "Percent of total cases",
    breaks = seq(0, 100, 10),
    labels = paste0(seq(0, 100, 10), "%")
  ) +
  scale_x_date(
    "Date",
    date_breaks = "1 week", date_labels = "W%W",
    minor_breaks = NULL
  ) +
  scale_fill_discrete(
    "Country",
    labels = c(
      "chn" = "China",
      "deu" = "Germany",
      "esp" = "Spain",
      "ita" = "Italy",
      "usa" = "USA"
    )
  ) +
  ggtitle("Percentage of COVID case counts per country for the first 10 weeks of 2020")