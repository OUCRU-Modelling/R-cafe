# ----------- Exercise ----------- #
# ----------- Capstone ----------- #

# Load Packages
#-----------
library(gtsummary)
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(data.table)
library(Hmisc)
library(stats)
library(stringr)
library(ggplot2)
library(purrr)
library(gt)

# Load Data
#-----------

###`Raw data`###

file_path <- "capstone_exercise/2-10-2020-_03TS_V1_Data.xls"
sheet_names <- excel_sheets(file_path)

all_data <- lapply(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet) %>%
    mutate(across(where(is.character), ~ recode(.x, "Y" = "yes", "N" = "no", "UNKNOWN" = NA_character_))) %>%
    mutate(across(where(~ all(.x %in% c("yes", "no", NA), na.rm = TRUE)), ~ factor(.x, levels = c("no", "yes"), labels = c(0, 1)))) %>%
    rename_with(tolower)
  return(df)
}) %>% setNames(tolower(sheet_names))

list2env(all_data, envir = .GlobalEnv)

###`Allocation data`###

randolist <- read_excel("capstone_exercise/03TS_Randlist.xlsx", sheet = "Allocation") %>%
  rename_with(tolower) %>%
  filter(row_number() <= 272) %>%
  rename(arm = r.arm) %>% 
  mutate(
    pat.id = str_remove(pat.id, ".*-"),
    usubjid = paste("003", pat.id, sep = "-"),
    arm = recode(arm,
                 "14 ampoules: TETANUS ANTITOXIN (IM) + 2 prefilled-syringes : TETAGAM®P (intrathecal))" = "equine and intrathecal",
                 "14 ampoules: TETANUS ANTITOXIN (IM)" = "equine and sham",
                 "12 prefilled-syringes : TETAGAM®P (IM) + 2 prefilled-syringes : TETAGAM®P (intrathecal))" = "human and intrathecal",
                 "12 prefilled-syringes : TETAGAM®P (IM)" = "human and sham"
    )
  )

###`Violation data`###

violations_file_path <- "capstone_exercise/Protocol violations, exclusions, withdrawals.xlsx"
all_sheets <- excel_sheets(violations_file_path)

violations_data <- lapply(all_sheets, function(sheet) {
  read_excel(violations_file_path, sheet = sheet) %>%
    rename_with(tolower)  
}) %>%
  setNames(all_sheets)

names(violations_data) <- gsub(" ", "_", tolower(all_sheets))

list2env(violations_data, envir = .GlobalEnv)

###`Excluded data`###

excluded_subjects <- unique(c(pilot$usubjid, withdrawals$usubjid))

dataset_names <- c(
  "vent", "enr", "adm", "vent_ventilation", "vent_trachesupp", "daily",
  "daily_daily", "comp", "fu", "daily_fu", "daily_fu_gridfu", "s_ae"
)

filtered_datasets <- map(setNames(dataset_names, dataset_names), ~ {
  get(.x) %>% filter(!usubjid %in% excluded_subjects)
})

list2env(filtered_datasets, envir = .GlobalEnv)

###`Baseline data`###

baseline_data <- enr %>%
  select(-entry) %>%
  left_join(adm, by = "usubjid") %>%
  left_join(randolist %>% select(usubjid, arm), by = "usubjid")

baseline_data_raw <- baseline_data %>%
  mutate(bmi = weight / ((height / 100) ^ 2))


###`Baseline table`###

generate_baseline_summary <- function(baseline_data) {
  library(dplyr)
  library(gtsummary)
  
  # Select relevant columns
  baseline_data <- baseline_data %>%
    select(usubjid, age, sex, bmi, source, tetanus, icudays, outcome, hypertension, 
           myocardialinfart, angina, perivascular, chronicpul, connectivetissue, 
           mildliver, hemiplegia, diawithchronic, severeliver, aids, cardiacfailureiii, 
           cerebrovascular, severeresp, pepticulcer, diabetes, severekidney, malignancy, 
           tumour, dementia, renalfailure, electivesurgery, emergencysurgery, timetoadm, 
           incubationperiod, incuperiodonset, wound, diffbreath, ablettscore, asa, 
           maxtemp, resp, fio2, spo2, pao2, ph, plt, wbc, hct, maxhr, minhr, maxsbp, 
           worstdbp, worstsbp, vaso, na, k, creat, Arm) %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  factor_vars <- setdiff(names(baseline_data)[sapply(baseline_data, is.factor)], "Arm")
  
  value_formula <- as.formula(paste("c(", paste(factor_vars, collapse = ", "), ") ~ '1'"))
  
  baseline_table <- baseline_data %>%
    select(-usubjid) %>%
    tbl_summary(
      by = Arm, 
      missing = "no", # Handle missing values
      statistic = list(
        all_continuous() ~ "{median} ({p25}, {p75})", 
        all_categorical() ~ "{n}/{N} ({p}%)"
      ),
      digits = list(all_continuous() ~ 1),
      type = list(fio2 ~ "continuous"),
      value = value_formula  # Apply "1" to all factor variables
    ) %>%
    modify_header(label ~ "Variable") %>%
    bold_labels()
  
  return(baseline_table)
}





# Population
#-----------

im_itt <- violations_data[["im_itt"]]
it_per_protocol <- violations_data[["it_per_protocol"]]
im_per_protocol <- violations_data[["im_per_protocol"]]

# IT-ITT: All patients with a non-missing randomization date
baseline_it_itt <- baseline_data_raw %>% 
  mutate( 
    Arm = recode(arm, 
                 "equine and intrathecal" = 0, 
                 "human and intrathecal" = 0,
                 "human and sham" = 1,
                 "equine and sham" = 1),
    Arm = factor(Arm, levels = c(0,1), labels = c("Intrathecal treatment", "Sham procedure"))
  )

# IM-ITT: Patients in IT-ITT who did NOT receive antitoxin before admission
baseline_im_itt <- baseline_data_raw %>% 
  filter(!usubjid %in% im_itt$usubjid) %>% 
  mutate(
    Arm=recode(arm, "equine and intrathecal" = 0, 
               "equine and sham" = 0,
               "human and sham" = 1,
               "human and intrathecal" = 1),
    Arm=factor(Arm, levels=c(0,1), labels=c("Equine IM","Human IM"))
  )

# IM-ALL Population: Patients who received intramuscular antitoxin (ADM.PREHTIG == "Y")
baseline_im_all <- baseline_data_raw %>%
  mutate(
    Arm = recode(
      arm,
      "equine and intrathecal" = 0,
      "equine and sham" = 0,
      "human and sham" = 1,
      "human and intrathecal" = 1
    ),
    Arm = ifelse(prehtig == 1, 2, Arm),
    Arm = factor(
      Arm,
      levels = c(0, 1, 2),
      labels = c("Equine IM", "Human IM", "Equine IM pre hospital")
    )
  )

# IT-PP Population: Patients who received the intrathecal procedure (ADM.DATEHTIG is not missing)

baseline_it_pp <- baseline_data_raw %>% 
  filter(!usubjid %in% it_per_protocol$usubjid) %>% 
  mutate( 
    Arm = recode(arm, 
                 "equine and intrathecal" = 0, 
                 "human and intrathecal" = 0,
                 "human and sham" = 1,
                 "equine and sham" = 1),
    Arm = factor(Arm, levels = c(0,1), labels = c("Intrathecal treatment", "Sham procedure"))
  )

# IM-PP Population: Patients in IM-TT received the allocated intramuscular treatment

baseline_im_pp <- baseline_data_raw %>% 
  filter(!usubjid %in% im_per_protocol$usubjid) %>% 
  mutate( 
    Arm=recode(arm,
               "equine and intrathecal" = 0, 
               "equine and sham" = 0,
               "human and sham" = 1,
               "human and intrathecal" = 1),
    Arm=factor(Arm, levels=c(0,1), labels=c("Equine IM","Human IM"))
  )

# Summary table
generate_baseline_summary(baseline_it_itt)
generate_baseline_summary(baseline_im_itt)
generate_baseline_summary(baseline_im_all)
generate_baseline_summary(baseline_it_pp)
generate_baseline_summary(baseline_im_pp)


# Adverse event Table
#---------------

ae_file_path <- "capstone_exercise/AE.SAE DATA SHEET.xls"

sae_gridae <- read_excel(ae_file_path,sheet = "S_AE_GridAE")
sae_gridsae <- read_excel(ae_file_path,sheet = "S_AE_GridSAE")

names(sae_gridae) <- tolower(names(sae_gridae))
names(sae_gridsae) <- tolower(names(sae_gridsae))

sae_gridae <- sae_gridae %>%
  filter(!usubjid %in% excluded_subjects)

sae_gridsae <- sae_gridsae %>%
  filter(!usubjid %in% excluded_subjects)

any_ae <- sae_gridae %>% 
  filter(ctcaename!="Nasogastric tube" & ctcaename!="Urinary Catheter" & ctcaename!="Tracheostomy" & ctcaename!="Mechanical ventilation" & ctcaename!="ANSD") 

baseline_data <- baseline_data_raw %>% 
  mutate( 
    Arm = recode(arm, 
                 "equine and intrathecal" = 0, 
                 "human and intrathecal" = 0,
                 "human and sham" = 1,
                 "equine and sham" = 1),
    Arm = factor(Arm, levels = c(0,1), labels = c("Intrathecal treatment", "Sham procedure"))
  )

data_ae <- any_ae %>% left_join(select(baseline_data, c(usubjid, Arm)))
generate_baseline_summary(baseline_im_pp)

# Plot
# -------

# `pipecuronium_wide`
pipecuronium_wide <- daily_daily %>%
  pivot_wider(id_cols = usubjid, names_from = daily_daily_seq, values_from = "pipecuronium") %>% 
  rename_with(~ paste0("pipecuronium", .), -usubjid) %>%
  mutate(
    total_pipecuronium = rowSums(select(., starts_with("pipecuronium")), na.rm = TRUE),
    length_pipecuronium = select(., starts_with("pipecuronium")) %>% apply(1, function(x) sum(!is.na(x)))
  ) %>%
  left_join(vent, by = "usubjid") %>%
  filter(venstart == "1") %>%
  select(usubjid, total_pipecuronium, length_pipecuronium, venstart)

# Attach Arm and Total_PIPE`
pipecuronium.full <- left_join(pipecuronium_wide, randolist %>% select(usubjid, arm), by = "usubjid")

# Create population
pipecuronium.full <-
  left_join(
    pipecuronium_wide %>% select(usubjid, total_pipecuronium, length_pipecuronium, venstart),
    randolist %>% select(usubjid, arm)
  )

pipecuronium.it.itt <-
  pipecuronium.full %>%
  mutate(
    Arm=recode(arm, "equine and intrathecal" = 1, 
               "human and intrathecal" = 1,
               "human and sham" = 0,
               "equine and sham" = 0),
    Arm=factor(Arm, levels=c(0,1), labels=c("Sham procedure","IT treatment")),
    Total_PIPE=sqrt(total_pipecuronium)
  )

pipecuronium.it.protocol <-
  pipecuronium.full %>%
  mutate(
    Arm=recode(arm, "equine and intrathecal" = 1, 
               "human and intrathecal" = 1,
               "human and sham" = 0,
               "equine and sham" = 0),
    Arm=factor(Arm, levels=c(0,1), labels=c("Sham procedure","IT treatment")),
    Total_PIPE=sqrt(total_pipecuronium)
  ) %>%
  filter(!usubjid %in% it_per_protocol$usubjid) 

pipecuronium.im.itt <-
  pipecuronium.full %>%
  mutate(
    Arm=recode(arm, "equine and intrathecal" = 0, 
               "equine and sham" = 0,
               "human and sham" = 1,
               "human and intrathecal" = 1),
    Arm=factor(Arm, levels=c(0,1), labels=c("Equine IM","Human IM")),
    Total_PIPE=sqrt(total_pipecuronium)
  ) %>% 
  filter(!usubjid %in% im_itt$usubjid) 

pipecuronium.im.protocol <-
  pipecuronium.full %>%
  mutate(
    Arm=recode(arm, "equine and intrathecal" = 0, 
               "equine and sham" = 0,
               "human and sham" = 1,
               "human and intrathecal" = 1),
    Arm=factor(Arm, levels=c(0,1), labels=c("Equine IM","Human IM")),
    Total_PIPE=sqrt(total_pipecuronium)
  )  %>% 
  filter(!usubjid %in% im_per_protocol$usubjid) 

pipecuronium.im.all <-
  pipecuronium.full %>%
  left_join(adm) %>% select(usubjid, prehtig, total_pipecuronium, length_pipecuronium, arm) %>%
  mutate(
    Arm=recode(arm, "equine and intrathecal" = 0, 
               "equine and sham" = 0,
               "human and sham" = 1,
               "human and intrathecal" = 1),
    Arm=ifelse(prehtig=="1",2,Arm),
    Arm=factor(Arm, levels=c(0,1,2), labels=c("Equine IM","Human IM","Equine IM pre hospital")),
    Total_PIPE=sqrt(total_pipecuronium)
  )
# Plot function
plot_pipecuronium <- function(data, title) {
  ggplot(data, aes(x = Total_PIPE, fill = Arm)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    theme_minimal() +
    labs(title = title,
         x = "Total Pipecuronium (sqrt-transformed)",
         y = "Frequency",
         fill = "Treatment Arm")
}

# Plot list

# Histogram

list(
  "IT ITT Population" = pipecuronium.it.itt,
  "IT Protocol Population" = pipecuronium.it.protocol,
  "IM ITT Population" = pipecuronium.im.itt,
  "IM Protocol Population" = pipecuronium.im.protocol,
  "IM All Population" = pipecuronium.im.all
) %>% purrr::imap(~ plot_pipecuronium(.x, .y))

# violin plot
plot_violin_pipecuronium <- function(data, title) {
  ggplot(data, aes(x = Arm, y = Total_PIPE, fill = Arm)) +
    geom_violin(trim = FALSE, alpha = 0.4) +
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
    geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 1.5, alpha = 0.6) +
    scale_y_continuous("Total dose of pipecuronium", 
                       breaks = seq(0, 50, 10), 
                       labels = seq(0, 50, 10)^2) +
    labs(title = title, x = "Arm", y = "Total Dose") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0), legend.position = "bottom")
}


list(
  "IT ITT Population" = pipecuronium.it.itt,
  "IT Protocol Population" = pipecuronium.it.protocol,
  "IM ITT Population" = pipecuronium.im.itt,
  "IM Protocol Population" = pipecuronium.im.protocol,
  "IM All Population" = pipecuronium.im.all
) %>% purrr::imap(~ plot_violin_pipecuronium(.x, .y))

# CDF plot
plot_cdf_pipecuronium <- function(data, title) {
  ggplot(data, aes(x = Total_PIPE, color = Arm)) +
    stat_ecdf(geom = "step", linewidth = 1) +
    scale_x_continuous("Total dose of pipecuronium", 
                       breaks = seq(0, 50, 10), 
                       labels = seq(0, 50, 10)^2) +
    labs(title = title, y = "Cumulative Probability") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

list(
  "IT ITT Population" = pipecuronium.it.itt,
  "IT Protocol Population" = pipecuronium.it.protocol,
  "IM ITT Population" = pipecuronium.im.itt,
  "IM Protocol Population" = pipecuronium.im.protocol,
  "IM All Population" = pipecuronium.im.all
) %>% purrr::imap(~ plot_cdf_pipecuronium(.x, .y))
