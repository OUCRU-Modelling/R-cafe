# ====== R Scipt for take home exercise solution ===


## Task 1
covid_cases <- readRDS("./day1/data/covid_cases.rds")
library(tidyverse)


## Task 2
covid_cases %>% pivot_longer(cols = -date,
                             names_to = "country",
                             values_to = "cases") %>%
  as_tibble() %>%
  skimr::skim()

incorrect_data <- covid_cases %>%
  pivot_longer(cols = -date,
               names_to = "country",
               values_to = "cases") %>%
  filter(cases < 0)

correct_data <- covid_cases %>%
  pivot_longer(cols = -date,
               names_to = "country",
               values_to = "cases") %>%
  anti_join(incorrect_data, by = c("date", "country", "cases"))

covid_cases <- correct_data %>%
  mutate(country = str_remove(country, "cases_")) %>%
  filter(week(date) >= 3 & week(date) <= 12)


## Task 3
top_countries <- covid_cases %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  slice_max(total_cases, n = 5) %>%
  pull(country)

plot_data <- covid_cases %>%
  mutate(country = as.factor(ifelse(
    country %in% top_countries, country, "Others"
  ))) %>%
  group_by(date, country) %>%
  summarise(total_cases = sum(cases)) %>%
  mutate(
    total_cases_per_date = sum(total_cases),
    pct_cases = ifelse(
      total_cases_per_date == 0,
      NA,
      total_cases * 100 / total_cases_per_date
    ),
    week = week(date)
  ) %>%
  na.omit()

plot_data %>%
  mutate(country = fct_relevel(country, "chn", "deu", "esp",
                               "ita", "usa", "Others")) %>% 

## Task 4
ggplot(aes(x = date, y = pct_cases, fill = country)) +
  geom_area(color = "black", size = 0.5) +
  scale_x_date(date_labels = "W%U",
               date_breaks = "1 week",
               expand = c(0, 0)) +
  scale_y_continuous(
    labels = function(x)
      paste(x, "%"),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_discrete(
    name = "Country",
    breaks = c("chn", "deu", "esp", "ita", "usa", "Others"),
    labels = c("China", "Germany", "Spain", "Italy", "USA", "Others")
  ) +
  labs(title = "Percentage of COVID case counts per country for the first 10 weeks of 2020", x = "Date", y = "Percent of total cases") +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12, color = "black"))
