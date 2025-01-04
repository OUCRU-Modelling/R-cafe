#Task 1: Data import
# Read data
covid_cases <- readRDS("../R-cafe/day1/data/covid_cases.rds")
library(tidyverse)

#Task2: Data cleaning and filtering
#pivot the data into a tibble that follows the tidy data standard
covid_cases <- as_tibble(covid_cases)
class(covid_cases)
covid_cases <- covid_cases %>% pivot_longer(cols = -date, names_to = "country", values_to = "cases"
                             ) %>% mutate(country = str_remove(country, "cases_"))
#filter out incorrect/impossible data
#Filter the data so that we only have week 3-12 of 2020
#Save the results back into covid_cases
str(covid_cases)
skim(covid_cases)
covid_cases <- covid_cases %>% dplyr::filter(cases >=0) %>% mutate(week = week(date)) %>% 
  dplyr::filter(between(week,3,12))

#Task 3: Data transformation
#Group the data and calculate the total number of cases per country
#Select the top 5 countries with highest total cases
#Extract the country codes and save them into a new object
top_countries <- covid_cases %>% group_by(country) %>% summarise(
  total_cases = sum(cases)) %>% dplyr::slice_max(total_cases, n=5) %>% dplyr::pull(country) #extract country codes

# Group the data again, calculate the total number of cases per country again
# keep the names of countries in the top 5 (generated above). Countries on in the top 5 will be changed to "Others"
# Turn this column into a factor type
cases_per_country <- covid_cases %>% group_by(country) %>% summarise(
  total_cases = sum(cases)) %>% mutate(country = replace (
    country, !(country %in% top_countries), "Others")) %>% mutate(country = as_factor(country))
str(cases_per_country)

# Group the data again, this time, group by date and country
# Calculate the total number of cases per date per country
# Create a new column called pct_cases, which is the percentage of total cases per date per country
# remove rows with NA from the tibble
# Save all of this into a new object, e.g. plot_data

cases_per_date <- covid_cases %>% group_by(date,country) %>% summarise(
  total_cases = sum(cases)) %>% mutate (percent_cases = round(
    total_cases/sum(total_cases)*100,4)) %>% na.omit()

# Task 4: Data visualization
plot_data <- covid_cases %>% group_by (week, country) %>% summarise(
  total_cases = sum(cases)) %>% mutate (percent_cases = round(
    total_cases/sum(total_cases),4)) %>% na.omit() %>% 
  mutate(country = replace (
      country, !(country %in% top_countries), "Others")) %>%
  mutate(country = as_factor(country))
plot_area <- plot_data %>% group_by (week, country) %>% 
  summarise(total_cases_n = sum (total_cases)) %>% 
  mutate (percent_cases = round(
    total_cases_n/sum(total_cases_n),4))
#a$country <- fct_lump(a$country,other_level = "Others")
plot_area$country <- factor(plot_area$country, levels = c("chn", "deu", "esp", "ita","usa", "Others"))
ggplot(plot_area, aes (week, percent_cases, fill = country)) + 
  geom_area() + 
  scale_x_continuous(breaks = seq(3,12,1),"Date", labels = c("W03","W04","W05","W06", "W07", "W08","W09","W10","W11","W12")) +
  scale_y_continuous(breaks = seq(0,1,by=0.1), labels = scales::label_percent(),"Percent of total cases") + 
  scale_fill_discrete(labels=c("China", "Germany", "Spain", "Italy", "USA", "Others")) +
  labs(fill = "Country") + ggtitle("Percentage of COVID case counts per country for the first 10 weeks of 2020")
  

  
