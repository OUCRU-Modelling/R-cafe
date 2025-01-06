# Take home exercise Day 2 === 

#Task 1: data import
covid_cases <- readRDS("../R-cafe/day1/data/covid_cases.rds")

install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(skimr)
library(lubridate)

##Task 2: Data cleansing and filtering

#pivot the data into a tibble 
covid_cases <- covid_cases %>% 
pivot_longer(cols = starts_with("cases"),
             names_to = "country",
             values_to = "cases") %>% 
  mutate(date = as.Date(date), country = str_remove(country,"cases_"), cases = as.numeric(cases)) 

#quick skim on the data
print(covid_cases)
skim(covid_cases)

# filter out incorrect/impossible data
covid_cases <- covid_cases %>% 
  filter(!is.na(cases) & cases >-1)
skim(covid_cases)


#Filter the data for the week 3-12 of 2020
covid_cases <- covid_cases %>% 
  filter(year(date) == 2020 & week(date) > 3 & week(date) <=12)
print(covid_cases)

###Task 3: Data transformation
#Group the data and calculate the total number of cases per country
#Select the top 5 countries with highest total cases
top_countries <- covid_cases %>% 
  group_by(country) %>% 
  summarise(total_cases = sum(cases)) %>% 
  slice_max(order_by = total_cases, n = 5 ) 

print(top_countries)

#Group the data by country, calculate the total number of cases per country
#keep the names of countries in the top 5,  other Countries will be changed to "Others"
#Turn country column into a factor type 
covid_cases_modified  <- covid_cases %>% 
  group_by(country) %>% 
  mutate(country = if_else(country %in% top_countries$country, country, "Others")) %>% 
  summarise(total_cases = sum(cases)) %>% 
  mutate(country = factor(country))

print(covid_cases_modified)


#Group the data by date and country
#calculate the total number of cases per date per country
#calculate the percentage of total cases per date per country
plot_data <- covid_cases %>% 
  group_by(date, country) %>% 
  mutate(country = if_else(country %in% top_countries$country, country, "Others")) %>% 
  summarise(total_cases = sum(cases), .groups = "drop") %>% 
  group_by(date) %>% 
  mutate(total_cases_date = sum(total_cases), pct_cases = total_cases/total_cases_date *100)

print(plot_data)

####Task 4 Data visualization

   ggplot(data = plot_data %>% 
    mutate(country = case_when(
      country == "chn" ~ "China", 
      country == "deu" ~ "Germany",
      country == "ita" ~ "Italy",
      country == "usa" ~ "USA", 
      country == "esp" ~ "Spain",
      TRUE~ country) %>%  fct_relevel("Others", after = Inf)), mapping = aes(x = date, y = pct_cases, fill = country)) + 
    geom_area() + 
    labs(x = "Date", y = "Percentage of Total Cases") + 
    scale_y_continuous(labels = scales::label_percent(scale = 1), breaks = seq(0, 100, by = 10)) +
    scale_x_date(date_breaks = "1 week", date_labels = "W%W") + 
    ggtitle("Percentage of COVID-19 Case Counts per Country for the First 10 Weeks of 2020")
  