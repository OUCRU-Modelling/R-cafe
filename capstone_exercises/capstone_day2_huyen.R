install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(dplyr)
install.packages("here")
library(here)
install.packages("stringr")
library(stringr)
install.packages("Hmisc")
library(Hmisc)
install.packages("skimr")
library(skimr)

TSdata <- read_xls("./capstone_exercises/data/2-10-2020-_03TS_V1_Data.xls")
as_tibble(TSdata)

enr <- read_excel("./capstone_exercises/data/2-10-2020-_03TS_V1_Data.xls", sheet = "ENR") %>%
  setNames(tolower(names(.)))
enr
excel_sheets("./capstone_exercises/data/2-10-2020-_03TS_V1_Data.xls")
adm <- read_excel("./capstone_exercises/data/2-10-2020-_03TS_V1_Data.xls", sheet = "ADM") %>%
  setNames(tolower(names(.)))
enr %>% summary
enr %>% str
enr %>% skim()
enr %>% skim(age16)
adm %>% summary
adm %>% skim()

### mutate function = create new colums that are functions of existing variables
enr <- enr %>% 
  mutate(
    age16 = case_when(age16 == "Y" ~ "yes",
                      age16 == "N" ~ "no",
                      age16 == "UNKNOWN" ~ NA_character_),
    age16 = factor(age16, levels = c("no", "yes"), labels = c(0, 1))
  )

##will this work?
enr <- enr %>% 
  mutate(    age16 = factor(age16, levels = c("N", "Y"), labels = c(0, 1))
  )
enr %>% select(age16)
enr %>% skim(age16)

##checking unique values
enr %>% select(age16) %>% unique #option 1
unique(enr$age16) #option 1, also shows the levels

### MUTATE ACROSS 
enr <- enr %>%
  mutate(across(
    where(is.character),
    function(a){ 
      case_when(
      a == "Y" ~ "yes",
      a == "N" ~ "no",
      a == "UNKNOWN" ~ NA_character_,
      TRUE ~ .x
      )
      }
  )) %>%
  mutate(across(
    where(~ all(.x %in% c("yes", "no", NA), na.rm = TRUE)),
    ~ factor(.x, levels = c("no", "yes"), labels = c(0, 1))
  ))
enr %>% skim()
