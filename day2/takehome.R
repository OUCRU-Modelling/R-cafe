library(tidyverse)
library(skimr)
## task 1
covid_cases <- readRDS("./day1/data/covid_cases.rds")

## task 2

# Does the data follow the tidy data standard? => No

# pivot
covid_cases <- covid_cases %>%
  pivot_longer(cols = -date,names_to = "country",values_to = "cases") 

# skim
skim(covid_cases)
# => cases variable have negative variables

# filter out
covid_cases <- covid_cases %>% filter(cases >= 0 & 
                                week(date) %in% 3:12 & 
                                year(date) == 2020)

## task 3

top_countries <- covid_cases %>% 
  group_by(country) %>% 
  summarise(total = sum(cases)) %>% 
  slice_max(order_by = total,n = 5) %>% 
  mutate(country = str_remove(country,"cases_")) %>% 
  pull(country)

## task 4

plot_data <- covid_cases %>% 
  mutate(country = str_remove(country,"cases_") %>% fct_lump_n(cases,n = 5)) %>% 
  group_by(date,country) %>% 
  summarise(cases = sum(cases)) %>% 
  mutate(pct_cases = cases/sum(cases)) %>% 
  na.omit() %>% 
  ggplot() +
  geom_area(aes(x = date, y = pct_cases,fill = country))+
  scale_y_continuous(name = "Percent of total cases",
                     breaks = seq(0,1,0.1),
                     labels = scales::percent_format())+
  scale_x_date(name = "Date",date_breaks = "1 week",
               date_labels = "W%V")+
  scale_fill_discrete(name = "Country", 
                      labels = c("China","Germany","Spain","Italy","USA","Others"))+
  ggtitle("Percentage of COVID case counts per country for the first 10 weeks of 2020")

