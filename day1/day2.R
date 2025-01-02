#How to use tidyverse
install.packages("tidyverse")
library(tidyverse)
# Piping with %>%
ggplot(
  data = ungroup(summarise(group_by(mtcars, gear), mean_mpg = mean(mpg))),
  aes(x = gear, y = mean_mpg)
) +
  geom_col()
#vs
mtcars %>% 
  group_by(gear) %>% 
  summarise(mean_mpg = mean(mpg)) %>% 
  ungroup() %>% 
  ggplot(aes(x = gear, y = mean_mpg)) +
  geom_col()
#excer day 2
summary(covid_cases)
?pivot_longer
pivot_longer(
  covid_cases,
  cols=-date,
  names_to= "country",
  values_to= "cases"
)
#tidy select_chon cac cot voi mot so dieu kien
pivot_longer(
  covid_cases,
  cols= starts_with("cases"),
  names_to= "country",
  values_to= "cases"
)

pivot_longer(
  covid_cases,cols=starts_with(("cases")),
  names_to="country",
  values_to = "cases"
) %>% mutate(
  country=str_remove(country, "cases_")
)
# Lay year tu date
pivot_longer(
  covid_cases,cols=starts_with(("cases")),
  names_to="country",
  values_to = "cases"
) %>% mutate(
  country=str_remove(country, "cases_"),
  year=year(date),
  month=month(date)
) 
# Lay year tu date sau do label lai chu cai
pivot_longer(
  covid_cases,cols=starts_with(("cases")),
  names_to="country",
  values_to = "cases"
) %>% mutate(
  country=str_remove(country, "cases_"),
  year=year(date),
  month=month(date, label=TRUE)
) 
#Lay du lieu theo quoc gia
pivot_longer(
  covid_cases,cols=starts_with(("cases")),
  names_to="country",
  values_to = "cases"
) %>% mutate(
  country=str_remove(country, "cases_"),
  year=year(date),
  month=month(date, label=TRUE)
) %>%   group_by(country) %>% 
  summarize(total_cases=sum(cases)) %>% 
  mutate(pct_cases=round(total_cases/sum(total_cases)*100,4) %>% 
  mutate(pct_text=paste0(pct_cases,"%")
)
# ggplot2
select (date,cases_chn, cases_vnm, cases_kor) %>% 
pivot_longer(
  covid_cases,cols=starts_with(("cases")),
  names_to="country",
  values_to = "cases"
) %>% mutate(
  country=str_remove(country, "cases_"),
  year=year(date),
  month=month(date, label=TRUE)
) %>% ggplot(aes(x=date, y=cases, group=country,color=country))+
  geom_line()+
  heom_point()+
  scales
 
