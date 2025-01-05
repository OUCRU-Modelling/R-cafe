#Task 1: Data inport
covid_cases <- readRDS("day1/data/covid_cases.rds")
install.packages("tidyverse")
library(tidyverse)

#Task 2: Data cleaning and filtering
head(covid_cases)
str(covid_cases)
COVID <- pivot_longer(
  covid_cases, 
  cols = -date,
  names_to = "country",
  values_to = "cases")
head(COVID)  
str(COVID)

# Filter week 3-12 2020
COVID_filtered <- COVID %>%
  filter(cases > 0,
         year(date) == 2020,
         week(date) %in% seq(3,12))

### task 3: data transformation
##
COVID_other <- COVID_filtered %>%
  group_by(country) %>%
  summarise(total_number_case = sum(cases)) %>% 
  arrange(desc(total_number_case)) %>% 
  head(n = 5)

top_countries <- COVID_other %>%
  pull(country)
top_countries

##
COVID_filtered <- COVID_filtered %>%
  mutate(Top5_highest = if_else(
    country %in% `top_countries`,
    country,
    "Other"
  ))

##
COVID_other2 <- COVID_filtered %>%
  group_by(date, Top5_highest) %>% 
  summarise(total_number_case = sum(cases)) %>% 
  arrange(desc(total_number_case)) %>% 
  head(n = 5) 

###Task 4: Data visualization
ggplot() +
geom_area(aes(x = date, y = pct_cases,fill = country)) +
scale_y_continuous(name = "Percent of total cases", breaks = seq(0,1,0.1)) +
scale_x_date(name = "Date",date_breaks = "1 week") +
scale_fill_discrete(name = "Country", 
                    labels = c("China","Germany","Spain","Italy","USA","Others")) +
ggtitle("Percentage of COVID case counts per country for the first 10 weeks of 2020")

