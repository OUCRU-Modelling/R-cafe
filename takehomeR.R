#Liberary
install.packages("tidyverse")
install.packages("skimr")
library(tidyverse)
library(skimr)

#Task 1: Data import
covid_cases<-readRDS("./day1/data/covid_cases.rds")

#Task 2: Data cleaning and filtering
covid_cases<-covid_cases %>% 
  pivot_longer (cols=-date,names_to="country",values_to="cases")
skim(covid_cases)

#Filter out
covid_cases<-covid_cases%>%
  filter(cases>=0 & 
           week(date) >= 3 & week (date) <= 12 & 
           year(date)==2020)
View(covid_cases)

#task 3: data transformation
covid_cases %>% 
  group_by(country) %>% 
  summarise(total_cases=sum(cases)) %>% 
  slice_max(
    order_by = total_cases,
    n=5) %>% 
pull(country)
#Task 4: Data visualisation
plot_data<-covid_cases %>% 
  mutate(country=str_remove(country, "cases_") %>% fct_lump_n(cases, n=5)) %>% 
  group_by(date, country) %>%
  summarise(cases=sum(cases)) %>% 
  mutate(pct_cases=cases/sum(cases)) %>%
  na.omit() %>% 
  ggplot(aes(x=date, y= pct_cases, fill=country)) + 
  geom_area () +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_date (date_breaks = "1 week", date_labels = "W%V") +
  ggtitle(" Percentage of COVID-19 case counts per country for the first 10 weeks of 2020")
plot_data
