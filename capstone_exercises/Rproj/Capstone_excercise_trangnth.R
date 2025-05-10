#install packages (if needed)
install.packages(c("readxl", "dplyr", "here", "stringr", "Hmisc"))
setwd("C:/Users/dhtha/OneDrive - Oxford University Clinical Research Unit/Documents/R-cafe/capstone_exercises/Rproj")

# Library
library(readxl)
library(dplyr)
library(here)
library(stringr)
library(Hmisc)
library(skimr)


file_path <- "data/2-10-2020-_03TS_V1_Data.xls"  # Path to the file
# Read the ENR sheet
enr <- read_excel(file_path, sheet = "ENR") %>%
  setNames(tolower(names(.)))

# Read the ADM sheet
adm <- read_excel(file_path, sheet = "ADM") %>%
  setNames(tolower(names(.)))

#check data
summarise(enr)
skim(enr)
str(enr)

summarise(adm)
skim(adm)
str(adm)

#clean data
#For enr
enr <- enr %>%
  mutate(across(
    where(is.character),
    ~ case_when(
      .x == "Y" ~ "yes",
      .x == "N" ~ "no",
      .x == "UNKNOWN" ~ NA_character_,
      TRUE ~ .x
    )
  )) %>%
  mutate(across(
    where(~ all(.x %in% c("yes", "no", NA), na.rm = TRUE)),
    ~ factor(.x, levels = c("no", "yes"), labels = c(0, 1))
  ))

#For adm
adm <- adm %>%
  mutate(across(
    where(is.character),
    ~ case_when(
      .x == "Y" ~ "yes",
      .x == "N" ~ "no",
      .x == "UNKNOWN" ~ NA_character_,
      TRUE ~ .x
    )
  )) %>%
  mutate(across(
    where(~ all(.x %in% c("yes", "no", NA), na.rm = TRUE)),
    ~ factor(.x, levels = c("no", "yes"), labels = c(0, 1))
  ))

#Read file allocation
allocation_data <-read_excel("data/03TS_Randlist.xlsx", sheet = "Allocation") %>%
  setNames(tolower(names(.)))

randomlist <- allocation_data %>% 
  filter(row_number()<=272) %>% 
  rename(arm=r.arm) %>% 
  mutate(
    pat.id = str_replace(pat.id,".*-",""),
    usubjid = paste("003",pat.id,sep="-"), 
    arm= case_when(
      arm == "14 ampoules: TETANUS ANTITOXIN (IM) + 2 prefilled-syringes : TETAGAM®P (intrathecal))" ~ "equine and intrathecal",
      arm == "14 ampoules: TETANUS ANTITOXIN (IM)" ~ "equine and sham",
      arm == "12 prefilled-syringes : TETAGAM®P (IM) + 2 prefilled-syringes : TETAGAM®P (intrathecal))" ~ "human and intrathecal",
      arm == "12 prefilled-syringes : TETAGAM®P (IM)" ~ "human and sham"
    ) 
      
  )

head(randomlist)

violation_file_path <- "data/Protocol violations, exclusions, withdrawals.xlsx"
pilot <- read_excel(violation_file_path, sheet = "Pilot")%>% 
  setNames(tolower(names(.)))

mv_before_im<- read_excel(violation_file_path, sheet = "MV before IM")%>% 
  setNames(tolower(names(.)))

mv_before_it<- read_excel(violation_file_path, sheet = "MV before IT")%>% 
  setNames(tolower(names(.)))

protocol_violation <- read_excel(violation_file_path, sheet = "Protocol violation") %>%
  setNames(tolower(names(.)))

withdrawals <- read_excel(violation_file_path, sheet = "Withdrawals") %>%
  setNames(tolower(names(.)))

it_per_protocol <- read_excel(violation_file_path, sheet = "IT Per protocol") %>%
  setNames(tolower(names(.)))

im_per_protocol <- read_excel(violation_file_path, sheet = "IM per protocol") %>%
  setNames(tolower(names(.)))

im_itt <- read_excel(violation_file_path, sheet = "IM ITT") %>%
  setNames(tolower(names(.)))

#rename column id
im_itt <- im_itt %>% rename(usubjid = "id")

# Combine usibjids for exclusion
excluded_subjects <- unique(c(pilot$usubjid, withdrawals$usubjid))

#Identify and exclude patient
# Filter ENR and ADM to remove excluded subjects
enr <- enr %>% filter(!usubjid %in% excluded_subjects)
adm <- adm %>% filter(!usubjid %in% excluded_subjects)

#Merge datasets:
baseline_data <- enr %>%
  select(-c(entry, studyid, siteid, subjid, event )) %>%
  left_join(adm, by = "usubjid") %>%
  left_join(randomlist %>% select(usubjid, arm), by = "usubjid")

#Add BMI and label:
baseline_data_raw <- upData(
  baseline_data,
  bmi = weight / ((height / 100) ^ 2),
  labels = c(
    age = "Age (years)",
    icudays = "Days in ICU"
  )
)

