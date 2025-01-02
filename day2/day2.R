library(tidyverse)
mtcars

ggplot(
  data = ungroup(summarise(group_by(mtcars, gear), mean_mpg = mean(mpg))),
  aes(x = gear, y = mean_mpg)
) +
  geom_col()

mtcars %>% 
  group_by(gear) %>% 
  summarise(mean_mpg = mean(mpg)) %>% 
  ungroup() %>%
  ggplot(aes(x = gear, y = mean_mpg)) +
  geom_col()

###

covid_cases <- readRDS("./day1/data/covid_cases.rds")

covid_cases <- as_tibble(covid_cases)

## pivot_longer

invisible(Sys.setlocale("LC_TIME", "English"))

pivot_longer(covid_cases,
             cols = -date,
             names_to = "country",
             values_to = "cases") %>% 
  mutate(country2 = str_remove(country,"cases_"),
         year = year(date),
         month = month(date,label = TRUE,abbr = TRUE)) %>% 
  group_by(country2) %>% 
  summarise(total = sum(cases)) %>% 
  mutate(percentage = paste(round(total*100/sum(total),5),"%",sep = " "))
 




