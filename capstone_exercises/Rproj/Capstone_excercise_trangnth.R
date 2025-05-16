#I.. DADTA PREPARATION

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

#II. TABLE CREATION
#1. Baseline characteristics: 
#Patient's detail: sex, age, bmi

# summary numeric variables (age, bmi)
numeric_summary <- baseline_data_raw %>%
  summarise(
    age_median = median(age, na.rm = TRUE),
    age_Q1 = quantile(age, 0.25, na.rm = TRUE),
    age_Q3 = quantile(age, 0.75, na.rm = TRUE),
    age_min = min(age, na.rm = TRUE),
    age_max = max(age, na.rm = TRUE),
    
    bmi_median = median(bmi, na.rm = TRUE),
    bmi_Q1 = quantile(bmi, 0.25, na.rm = TRUE),
    bmi_Q3 = quantile(bmi, 0.75, na.rm = TRUE),
    bmi_min = min(bmi, na.rm = TRUE),
    bmi_max = max(bmi, na.rm = TRUE)
  )

# summary category variable (sex)
sex_summary <- baseline_data_raw %>%
  count(sex) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# print value
print(numeric_summary)
print(sex_summary)

#Past Medical History: Variables like ADM.HYPERTENSION, ADM.MYOCARDIALINFART, ADM.SEVERELIVER,etc
vars_hist <- c("hypertension", "myocardialinfart", "angina", "perivascular", "chronicpul",
  "connectivetissue", "mildliver", "hemiplegia", "diawithchronic", "severeliver",
  "aids", "cardiacfailureiii", "cardiacfailureiv", "cerebrovascular", "severeresp",
  "pepticulcer", "diabetes", "severekidney", "malignancy", "tumour", "dementia")

# T???o b???ng th???ng kê t???n su???t cho t???ng bi???n (s??? lu???ng và ph???n tram)
result <- sapply(baseline_data_raw[vars_hist], function(x) {
  count <- sum(x == 1, na.rm = TRUE)
  percent <- round(mean(x == 1, na.rm = TRUE) * 100, 1)
  paste0(count, " (", percent, "%)")
})

# Chuy???n v??? d???ng data frame d??? xem
summary_table <- data.frame(Variable = names(result), Count_Percentage = as.vector(result))

#Other comorbiditys
# Tính t???ng s??? dòng h???p l??? (dùng d??? tính ph???n tram)
n_total <- nrow(baseline_data_raw)
# T???ng s??? tru???ng h???p có d??? li???u ??? comorbidityoth1 ho???c comorbidityoth2
others_combined_count <- sum(
  !is.na(baseline_data_raw$comorbidityoth1) & trimws(baseline_data_raw$comorbidityoth1) != "" |
    !is.na(baseline_data_raw$comorbidityoth2) & trimws(baseline_data_raw$comorbidityoth2) != ""
)
# Tính ph???n tram
others_combined_percent <- round(others_combined_count / n_total * 100, 1)
# T???o b???ng 1 dòng cho "Others"
others_summary <- data.frame(
  Variable = "Others",
  Value = "Others",
  Count_Percentage = paste0(others_combined_count, " (", others_combined_percent, "%)")
)
# G???p v???i b???ng binary_summary có s???n
summary_all <- rbind(binary_summary, others_summary)
# Hi???n th??? k???t qu???
print(summary_all)

#Patient History: Duration of illness, incubation period, respiratory rate, platelet count, etc.
