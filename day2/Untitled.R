# tidydata
library(tidyverse)
ggplot(
  data = ungroup(summarise(group_by(mtcars, gear), mean_mpg = mean(mpg))),
  aes(x = gear, y = mean_mpg)
) +
  geom_col()
covid_cases <- readRDS("~/Documents/GitHub/R-cafe/day1/data/covid_cases.rds")
View(covid_cases)
covid_cases <- as_tibble(covid_cases)
pivot_longer(covid_cases, cols = -date, names_to = "country", values_to = "cases"
             ) %>%  mutate(country = str_remove(
               country, "cases_"), year = year(date), month = month (
                 date, label = TRUE, abbr=TRUE)) %>% group_by(country) %>% summarise(
                   total_cases = sum(cases)
                   ) %>% mutate(
                     percent = round(
                       total_cases/sum(total_cases)*100,4),percent_text = paste0(percent,"%"))
                   
#pivot_longer(covid_cases, cols = -c(date, cases_chn), names_to = "country", values_to = "cases")
#pivot_longer(covid_cases, cols = starts_with("cases"), names_to = "country", values_to = "cases")
