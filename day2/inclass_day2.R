# ----------- Day 2 ----------- #
# ========= tidyverse ========= #

library(tidyverse)

as_tibble(mtcars)

# Original code
ggplot(
  data = ungroup(summarise(group_by(mtcars, gear), mean_mpg = mean(mpg))),
  aes(x = gear, y = mean_mpg)
) +
  geom_col()

# tidyverse style
mtcars %>% 
  group_by(gear) %>% 
  summarise(mean_mpg = mean(mpg)) %>% 
  ungroup() %>% 
  ggplot(aes(x = gear, y = mean_mpg)) + 
  geom_col()

# transform data
covid_cases <- readRDS(file.path(".", "day1", "data", "covid_cases.rds"))
covid_cases # violate the tidy data rule

pivot_longer(
  covid_cases,
  cols = -date , # keep the non-pivoted column, tidy select rule
  names_to = "country", 
  names_pattern = "cases_(.+)",   # ".": take all the characters, "+" more than 1 chac
  values_to = "case"
)

covid_cases %>% pivot_longer(
  cols = starts_with("cases") , # keep the non-pivoted column, tidy-select rule
  names_to = "country",   # grab the name of the column and turn it into a column
  values_to = "case"      # take the cases of countries to a case column
) %>% mutate(
  country = str_remove(country, "cases_"),   # remove the string "case_"
  year = year(date),       # take the year of the date column
  month = lubridate::month(date,label = F)
) %>% group_by(
  country
  ) %>% summarise(
    total = sum(case)       # sum of cases of each country
  ) %>% mutate(                  
    per = paste0(round((total/sum(total))*100,3),"%")   # prop of each country's cases over total
)


Sys.setlocale("LC_TIME", "Vietnamese_Vietnam.utf8")


covid_cases %>% 
  select(date, cases_chn, cases_vnm, cases_kor) %>% 
  pivot_longer(
  cols = starts_with("cases") , # keep the non-pivoted column, tidy-select rule
  names_to = "country",   # grab the name of the column and turn it into a column
  values_to = "case"      # take the cases of countries to a case column
) %>% mutate(
  country = str_remove(country, "cases_"),   # remove the string "case_"
  year = year(date),       # take the year of the date column
  month = lubridate::month(date,label = F
  )) %>% ggplot(aes(x = date, y = case, 
                   group = country,
               color = country)) +
                 geom_line() +
                 geom_point() +
                 scale_y_continuous(breaks = seq(0, 20000, 1000)) +
  theme_bw() + 
    ggtitle(" Covid cases in 2020")














