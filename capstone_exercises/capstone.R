
#load packages necessay for data analysis
library(tidyverse)
install.packages(c("gtsummary", "broom.helpers"))
library(gtsummary)
library(readxl)
library(dplyr)
library(janitor)


# load the data
file_path <- "data/2-10-2020-_03TS_V1_Data.xls"
ENR <- read_excel(file_path,  sheet = "ENR")
ADM  <- read_excel(file_path,  sheet = "ADM")
randlist <- read_excel("data/03TS_Randlist.xlsx", sheet =1)
withdrawal <- read_excel("data/Protocol violations, exclusions, withdrawals.xlsx", sheet = "Withdrawals")
exclude_IT_PP  <- read_excel("data/Protocol violations, exclusions, withdrawals.xlsx", sheet = "IT Per protocol")
exclude_IM_PP  <- read_excel("data/Protocol violations, exclusions, withdrawals.xlsx", sheet = "IM per protocol")


# Exclude patients recruited in the pilot phase 
colnames(ENR)
colnames(ADM)
ENR <- ENR %>% filter(!str_detect(SUBJID, "^P0[1-5]"))
ADM <- ADM %>% filter(!str_detect(SUBJID, "^P0[1-5]"))

# create BMI column in ADM
is.na(ADM$WEIGHT)
is.na(ADM$HEIGHT)
ADM <- ADM %>% mutate(BMI = WEIGHT/(HEIGHT/100)^2) 

# create SUBJID for randlist
randlist <- randlist %>% mutate (SUBJID = str_remove(pat.id, "03TS-"))
sum(is.na(randlist$SUBJID)) 

#join ENR and ADM data
demo_data <- left_join(ENR, ADM, by = "SUBJID")
joined_data <- left_join(demo_data, randlist, by = "SUBJID")
joined_data <- joined_data %>% 
  mutate(IT_intervention = ifelse(str_detect(r.arm, "intrathecal"),"Intrathecal treatment", "Sham procedure" )) %>% 
  mutate(IM_intervention = ifelse(str_detect(r.arm, "TETANUS ANTITOXIN"),"Equine antitoxin", "Human antitoxin" )) %>% 
  mutate(SEX = factor(SEX,
    levels = c("F", "M"),
    labels = c("Female", "Male")))
 
# define target populations for analysis
IT_ITT <- joined_data %>% filter(!is.na(RANDTC)) %>% 
  filter(!USUBJID.x %in% withdrawal$USUBJID)
IM_ITT <- joined_data %>% filter(PREHTIG == "N") %>% 
  filter(!USUBJID.x %in% withdrawal$USUBJID)
  
IT_PP<- IT_ITT %>% filter(!USUBJID.x %in% exclude_IT_PP$USUBJID)
IM_PP <- IM_ITT %>% filter(!USUBJID.x %in% exclude_IM_PP$USUBJID)
IM_ALL <- joined_data %>% filter(!USUBJID.x %in% withdrawal$USUBJID)

# Summary of patient charactieristics

table_IT <- IT_ITT %>% 
  tbl_summary(
    by = IT_intervention,
    include = c(AGE,SEX,BMI, TIMETOADM, INCUBATIONPERIOD, INCUPERIODONSET, ABLETTSCORE),
    label = list(
      SEX ~ "Sex", 
      AGE ~ "Age (years)", 
      BMI ~ "Body mass index (kg/m²)", 
      TIMETOADM ~ "Duration of illness (days)", 
      INCUBATIONPERIOD ~ "Incubation period (days)", 
      INCUPERIODONSET ~ "Period of oneset (hours)", 
      ABLETTSCORE ~ "Ablett Score"),
    statistic = list(
      all_continuous() ~ "{median} ({p25} - {p75})",
      all_categorical() ~ "{n}/{N} ({p}%)"), 
    digits = list(all_continuous() ~ 1), 
    missing = "no"
    ) %>%  
    modify_spanning_header(all_stat_cols() ~ "**Intrathecal Intervention**")
print(table_IT)

table_IM <- IM_ITT %>% 
  tbl_summary(
    by = IM_intervention,
    include = c(AGE,SEX,BMI, TIMETOADM, INCUBATIONPERIOD, INCUPERIODONSET, ABLETTSCORE),
    label = list(
      SEX ~ "Sex", 
      AGE ~ "Age (years)", 
      BMI ~ "Body mass index (kg/m²)", 
      TIMETOADM ~ "Duration of illness (days)", 
      INCUBATIONPERIOD ~ "Incubation period (days)", 
      INCUPERIODONSET ~ "Period of oneset (hours)", 
      ABLETTSCORE ~ "Ablett Score"),
    statistic = list(
      all_continuous() ~ "{median} ({p25} - {p75})",
      all_categorical() ~ "{n}/{N} ({p}%)"), 
    digits = list(all_continuous() ~ 1), 
    missing = "no"
  ) %>%  
  modify_spanning_header(all_stat_cols() ~ "**Intrathecal Intervention**")
print(table_IM)

tbl_merge(
  tbls = list(table_IT, table_IM),
  tab_spanner = c("**Intrathecal Treatment**", "**Intramuscular treatment**")) %>% 
  modify_caption("**Table 1. Baseline Characteristics of the Study Population**")

# Prepare the data to summary the primary outcome

VENT <- read_excel(file_path,  sheet = "VENT") %>% 
   filter(!str_detect(SUBJID, "^P0[1-5]"))
joined_data_vent <- left_join(joined_data, VENT, by ="SUBJID")

MV_before_IM <- read_excel("data/Protocol violations, exclusions, withdrawals.xlsx", sheet = "MV before IM")
MV_before_IT <- read_excel("data/Protocol violations, exclusions, withdrawals.xlsx", sheet = "MV before IT")
IT_ITT_vent <- joined_data_vent %>% filter(!is.na(RANDTC)) %>% 
  filter(!USUBJID.x %in% withdrawal$USUBJID) %>% 
  filter(!USUBJID.x %in% MV_before_IT$USUBJID)

IM_ITT_vent <- joined_data_vent %>% filter(PREHTIG == "N") %>% 
  filter(!USUBJID.x %in% withdrawal$USUBJID) %>% 
  filter(!USUBJID.x %in% MV_before_IM$USUBJID) 

IT_PP_vent <- IT_ITT_vent %>% filter(!USUBJID.x %in% exclude_IT_PP$USUBJID)

IM_PP_vent <- IM_ITT_vent %>% filter(!USUBJID.x %in% exclude_IM_PP$USUBJID)

# primary outcome, reqirement for MV
tbl_IT_ITT_vent <- IT_ITT_vent %>% 
  mutate(VENSTART = factor(VENSTART, levels= c("Y","N"), labels = c("MV", "No MV"))) %>% 
  tbl_cross(
   row = IT_intervention,
   col = VENSTART,
   percent = "row") %>% 
   modify_header(label = "**Intervention**") %>%
   modify_spanning_header(starts_with("stat_") ~ "**Intention-to-treat population**") %>% 
  add_p()
  print(tbl_IT_ITT_vent)  
  
  tbl_IM_ITT_vent <- IM_ITT_vent %>% 
    mutate(VENSTART = factor(VENSTART, levels= c("Y","N"), labels = c("MV", "No MV"))) %>% 
    tbl_cross(
      row = IM_intervention,
      col = VENSTART,
      percent = "row") %>% 
    modify_header(label = "**Intervention**") %>%
    modify_spanning_header(starts_with("stat_") ~ "**Intention-to-treat population**") %>% 
  add_p()
  print(tbl_IM_ITT_vent) 
  
  tbl_IT_PP_vent <- IT_PP_vent %>% 
    mutate(VENSTART = factor(VENSTART, levels= c("Y","N"), labels = c("MV", "No MV"))) %>% 
    tbl_cross(
      row = IT_intervention,
      col = VENSTART,
      percent = "row") %>% 
    modify_header(label = "**Intervention**") %>%
    modify_spanning_header(starts_with("stat_") ~ "**Per-protocol population**") %>% 
    add_p()
  print(tbl_IT_PP_vent) 
  
  tbl_IM_PP_vent <- IM_PP_vent %>% 
    mutate(VENSTART = factor(VENSTART, levels= c("Y","N"), labels = c("MV", "No MV"))) %>% 
    tbl_cross(
      row = IM_intervention,
      col = VENSTART,
      percent = "row") %>% 
    modify_header(label = "**Intervention**") %>%
    modify_spanning_header(starts_with("stat_") ~ "**Per-protocol population**") %>% 
    add_p()
  print(tbl_IM_PP_vent) 
  
  # Summary Adverse Events (AE)
  AE <- read_excel("data/AE.SAE DATA SHEET.xls", sheet = "ALL AEs")
  AE_rel <- read_excel("data/AE.SAE DATA SHEET.xls", sheet = "pos related and related")
  SAE <- read_excel("data/AE.SAE DATA SHEET.xls", sheet = "S_AE_GridSAE")
  
  