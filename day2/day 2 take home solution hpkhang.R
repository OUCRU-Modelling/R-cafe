# ------ Day 2 - Take home exercise ------ #

# Task 1: Data import
#--------------------
covid_cases <- readRDS("~/Documents/GitHub/Workshop-R/day1/data/covid_cases.rds")

library(tidyverse)

library(stringr)

# Task 2: Data cleaning and filtering
#------------------------------------

covid_cases <- covid_cases %>% pivot_longer(
  cols = -date,
  names_to = "country",
  values_to = "case",
) %>% mutate(
  country = str_remove(country, "cases_"),
  week = lubridate::week(date)
) %>% dplyr::filter(
  week >= 3 & week <=12,
  case >= 0
) 

skimr::skim(covid_case) 

# Task 3: Data transformation
#----------------------------

top_countries <- covid_cases %>% 
  group_by(country)  %>% 
  summarise(total = sum(case, na.rm = T)      
) %>% slice_max(total, n = 5) %>% 
  pull(country)


plot_data <- covid_cases %>% 
  mutate(
    country = if_else(country %in% top_countries$country, country, "Others"),
    country = forcats::as_factor(country)
  ) %>% 
  group_by(date, country) %>% 
  summarise(
    total_cases_per_country = sum(case, na.rm = T), 
    .groups = "drop") %>% 
  group_by(date) %>% 
  mutate(
    total_cases_per_date = sum(total_cases_per_country),
    pct_cases = ifelse(
      total_cases_per_date == 0,
      NA,
      total_cases_per_country * 100 / total_cases_per_date)
         ) %>% 
  ungroup() %>% 
  drop_na()

# Task 4: Data visualization
#----------------------------

plot_data <- plot_data %>%
  mutate(country = fct_relevel(country, "chn", "deu", "esp",
                               "ita", "usa", "Others"))

ggplot(data = plot_data, aes(x = date, y = pct_cases, fill = country)) +
  geom_area() +
  scale_x_date(date_labels = "W%U",
               date_breaks = "1 week",
               expand = c(0, 0)) +
  scale_y_continuous(
    labels = function(x) paste(x, "%"),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_discrete(
    name = "Country",
    breaks = c("chn", "deu", "esp", "ita", "usa", "Others"),
    labels = c("China", "Germany", "Spain", "Italy", "USA", "Others")
  ) +
  labs(
    title = "Percentage of COVID case counts per country for the first 10 weeks of 2020",
    x = "Date",
    y = "Percent of total cases"
  ) +
  theme_minimal()






