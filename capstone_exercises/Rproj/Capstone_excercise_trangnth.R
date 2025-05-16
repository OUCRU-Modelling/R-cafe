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

# Binary variables list from Past Medical History
vars_hist <- c("hypertension", "myocardialinfart", "angina", "perivascular", "chronicpul",
               "connectivetissue", "mildliver", "hemiplegia", "diawithchronic", "severeliver",
               "aids", "cardiacfailureiii", "cardiacfailureiv", "cerebrovascular", "severeresp",
               "pepticulcer", "diabetes", "severekidney", "malignancy", "tumour", "dementia",
               "electivesurgery", "emergencysurgery"
)
# summary table for binary variables
result <- sapply(baseline_data_raw[vars_hist], function(x) {
  count <- sum(x == 1, na.rm = TRUE)
  percent <- round(mean(x == 1, na.rm = TRUE) * 100, 1)
  paste0(count, " (", percent, "%)")
})
# transfer result to data frame
summary_table <- data.frame(
  Variable = names(result),
  Count_Percentage = as.vector(result)
)
# total row (used to calculate percentage)
n_total <- nrow(baseline_data_raw)
# count data in  comorbidityoth1 or comorbidityoth2
others_combined_count <- sum(
  !is.na(baseline_data_raw$comorbidityoth1) & trimws(baseline_data_raw$comorbidityoth1) != "" |
    !is.na(baseline_data_raw$comorbidityoth2) & trimws(baseline_data_raw$comorbidityoth2) != ""
)
# Calculate percentage for text value (comorbidityother1 & 2)
others_combined_percent <- round(others_combined_count / n_total * 100, 1)
# Create "Others" including comorbidityother1 & 2
others_summary <- data.frame(
  Variable = "Others",
  Count_Percentage = paste0(others_combined_count, " (", others_combined_percent, "%)")
)
# Merge previous table and Others
summary_all <- rbind(summary_table, others_summary)
# Print value
print(summary_all)


#Patient History: Duration of illness, incubation period, respiratory rate, platelet count, etc.
#Tetanus Severity Score (TSS):
# Calculate TSS for each participants (usubjid)
baseline_data_raw <- baseline_data_raw %>%
  mutate(
    TSS_age = case_when(
      age <= 70 ~ 0,
      age <= 80 ~ 5,
      age > 80 ~ 10
    ),
    TSS_timetoadm = case_when(
      timetoadm <= 2 ~ 0,
      timetoadm >= 3 & timetoadm <= 5 ~ -5,
      timetoadm > 5 ~ -6
    ),
    TSS_diffbreath = case_when(
      diffbreath == "Y" ~ 4,
      diffbreath == "N" ~ 0,
      TRUE ~ 0  # m???c d???nh n???u d??? li???u thi???u
    ),
    TSS_asa = case_when(
      asa == 1 ~ 0,
      asa == 2 ~ 3,
      asa == 3 ~ 5,
      asa == 4 ~ 5,
      asa == 5 ~ 9
    ),
    TSS_wound = case_when(
      wound == 1 ~ 7,
      wound == 2 ~ 0
    ),
    TSS_maxsbp = case_when(
      maxsbp <= 130 ~ 0,
      maxsbp <= 140 ~ 2,
      maxsbp > 140 ~ 4
    ),
    TSS_maxhr = case_when(
      maxhr <= 100 ~ 0,
      maxhr <= 110 ~ 1,
      maxhr <= 120 ~ 2,
      maxhr > 120 ~ 4
    ),
    TSS_minhr = case_when(
      minhr <= 110 ~ 0,
      minhr > 110 ~ -2
    ),
    TSS_maxtemp = case_when(
      maxtemp <= 38.5 ~ 0,
      maxtemp <= 39 ~ 4,
      maxtemp <= 40 ~ 6,
      maxtemp > 40 ~ 8
    ),
    TSS_total = TSS_age + TSS_timetoadm + TSS_diffbreath + TSS_asa + TSS_wound +
      TSS_maxsbp + TSS_maxhr + TSS_minhr + TSS_maxtemp
  )
#Show total value of TTS
baseline_data_raw %>% select(usubjid, TSS_total)

#SOFA score

baseline_data_raw <- baseline_data_raw %>%
  mutate(
    # SpO2/FiO2 ratio
    spo2_fio2 = spo2 / fio2,
    SOFA_spo2fio2 = case_when(
      is.na(spo2_fio2) ~ 0,
      spo2_fio2 > 301 ~ 0,
      spo2_fio2 >= 221 ~ 1,
      spo2_fio2 >= 142 ~ 2,
      spo2_fio2 >= 67 ~ 3,
      spo2_fio2 < 67 ~ 4
    ),
    
    # Mean Arterial Pressure (MAP) - only if vaso != "Y"
    map = worstdbp + ((worstsbp - worstdbp)/3),
    SOFA_map = case_when(
      vaso == "Y" ~ 4,  # override
      is.na(map) ~ 0,
      map >= 70 ~ 0,
      map < 70 ~ 1
    ),
    
    # Bilirubin (bili)
    SOFA_bili = case_when(
      is.na(bili) | bili < 20 ~ 0,
      bili <= 32 ~ 1,
      bili <= 101 ~ 2,
      bili <= 204 ~ 3,
      bili > 204 ~ 4
    ),
    
    # Platelet count
    SOFA_plt = case_when(
      is.na(plt) ~ 0,
      plt >= 150 ~ 0,
      plt >= 100 ~ 1,
      plt >= 50 ~ 2,
      plt >= 20 ~ 3,
      plt < 20 ~ 4
    ),
    
    # Creatinine
    SOFA_creat = case_when(
      is.na(creat) ~ 0,
      creat < 110 ~ 0,
      creat <= 170 ~ 1,
      creat <= 299 ~ 2,
      creat <= 440 ~ 3,
      creat > 440 ~ 4
    ),
    
    # GCS
    SOFA_gcs = case_when(
      is.na(gcs) ~ 0,
      gcs == 15 ~ 0,
      gcs >= 13 ~ 1,
      TRUE ~ 2  # N???u GCS < 13, có th??? thêm phân m???c tùy theo chu???n b???n dùng
    ),
    
    # Total SOFA Score
    SOFA_total = SOFA_spo2fio2 + SOFA_map + SOFA_bili + SOFA_plt + SOFA_creat + SOFA_gcs
  )
# table including usubjid & SOFA score
sofa_summary <- baseline_data_raw %>%
  select(usubjid, SOFA_total)

# Show value
print(sofa_summary)

library(dplyr)

baseline_data_raw <- baseline_data_raw %>%
  mutate(
    # Temperature
    apache_temp = case_when(
      is.na(maxtemp) ~ 0,
      maxtemp >= 41 ~ 4,
      maxtemp >= 39 ~ 3,
      maxtemp >= 38.5 ~ 1,
      maxtemp >= 36 ~ 0,
      TRUE ~ 0
    ),
    
    # Mean Arterial Pressure
    map = worstdbp + ((worstsbp - worstdbp)/3),
    apache_map = case_when(
      is.na(map) ~ 0,
      map >= 160 ~ 4,
      map >= 130 ~ 3,
      map >= 110 ~ 2,
      map >= 70 ~ 0,
      map >= 50 ~ 2,
      map >= 40 ~ 3,
      map < 40 ~ 4
    ),
    
    # Heart rate: highest of maxhr/minhr
    hr_highest = pmax(maxhr, minhr, na.rm = TRUE),
    apache_hr = case_when(
      is.na(hr_highest) ~ 0,
      hr_highest >= 180 ~ 4,
      hr_highest >= 140 ~ 3,
      hr_highest >= 110 ~ 2,
      hr_highest >= 70 ~ 0,
      hr_highest >= 55 ~ 2,
      hr_highest >= 40 ~ 3,
      hr_highest < 40 ~ 4
    ),
    
    # Respiratory rate
    apache_resp = case_when(
      is.na(resp) ~ 0,
      resp >= 50 ~ 4,
      resp >= 35 ~ 3,
      resp >= 25 ~ 1,
      resp >= 12 ~ 0,
      resp >= 10 ~ 1,
      resp >= 6 ~ 2,
      resp < 6 ~ 4
    ),
    
    # PaO2 (only if fio2 < 0.5)
    apache_pao2 = case_when(
      is.na(pao2) | fio2 >= 0.5 ~ 0,
      pao2 > 70 ~ 0,
      pao2 >= 61 ~ 1,
      pao2 >= 55 ~ 2,
      pao2 < 55 ~ 4
    ),
    
    # pH
    apache_ph = case_when(
      is.na(ph) ~ 0,
      ph >= 7.7 ~ 4,
      ph >= 7.6 ~ 3,
      ph >= 7.5 ~ 1,
      ph >= 7.33 ~ 0,
      ph >= 7.25 ~ 2,
      ph >= 7.15 ~ 3,
      ph < 7.15 ~ 4
    ),
    
    # Sodium (na)
    apache_na = case_when(
      is.na(na) ~ 0,
      na >= 180 ~ 4,
      na >= 160 ~ 3,
      na >= 155 ~ 2,
      na >= 150 ~ 1,
      na >= 130 ~ 0,
      na >= 120 ~ 2,
      na >= 111 ~ 3,
      na < 111 ~ 4
    ),
    
    # Potassium (k)
    apache_k = case_when(
      is.na(k) ~ 0,
      k >= 7 ~ 4,
      k >= 6 ~ 3,
      k >= 5.5 ~ 1,
      k >= 3.5 ~ 0,
      k >= 3 ~ 1,
      k >= 2.5 ~ 2,
      k < 2.5 ~ 4
    ),
    
    # Creatinine (with doubling if renalfailure == "Y")
    apache_creat_raw = case_when(
      is.na(creat) ~ 0,
      creat >= 210 ~ 4,
      creat >= 178 ~ 3,
      creat >= 133 ~ 2,
      creat >= 54 ~ 0,
      creat < 54 ~ 2
    ),
    apache_creat = if_else(renalfailure == "Y", apache_creat_raw * 2, apache_creat_raw),
    
    # Hematocrit (hct)
    apache_hct = case_when(
      is.na(hct) ~ 0,
      hct >= 60 ~ 4,
      hct >= 50 ~ 2,
      hct >= 46 ~ 1,
      hct >= 30 ~ 0,
      hct >= 20 ~ 2,
      hct < 20 ~ 4
    ),
    
    # White blood count (wbc)
    apache_wbc = case_when(
      is.na(wbc) ~ 0,
      wbc >= 40 ~ 4,
      wbc >= 20 ~ 2,
      wbc >= 15 ~ 1,
      wbc >= 3 ~ 0,
      wbc >= 1 ~ 2,
      wbc < 1 ~ 4
    ),
    
    # GCS
    apache_gcs = 15 - gcs,
    
    # Age (from enr.age)
    apache_age = case_when(
      is.na(enr.age) ~ 0,
      enr.age < 45 ~ 0,
      enr.age <= 54 ~ 2,
      enr.age <= 64 ~ 3,
      enr.age <= 74 ~ 5,
      enr.age > 74 ~ 6
    ),
    
    # Elective surgery
    apache_elective = if_else(electivesurgery == "Y", 5, 0),
    
    # Emergency surgery
    apache_emergency = if_else(emergencysurgery == "Y", 5, 0),
    
    # Immune suppression (any Y in 5 variables)
    apache_immune = if_else(
      immunocompromised == "Y" | severeresp == "Y" | cardiacfailureiv == "Y" |
        diawithchronic == "Y" | severeliver == "Y",
      5, 0
    ),
    
    # T???ng di???m APACHE II
    APACHEII_total = apache_temp + apache_map + apache_hr + apache_resp + apache_pao2 +
      apache_ph + apache_na + apache_k + apache_creat + apache_hct +
      apache_wbc + apache_gcs + apache_age + apache_elective +
      apache_emergency + apache_immune
  )
apache_summary
