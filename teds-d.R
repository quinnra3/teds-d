###############################
# TEDS-D
# 10-20-2026  
# Quinn Anderson, RC
###############################

library(tidyverse)
library(gtsummary)
library(gt)
library(broom)
library(flextable)
library(haven)
library(janitor)
library(labelled)

##################### 
# import .Rdata files
#####################

# import from computer library (check )
path <- "C:/Users/qra2000/OneDrive - Columbia University Irving Medical Center/Behavioral Health RC/AAAV4952 Loneliness/TEDS/teds/data/"

list.files(path)

files <- list.files(path, pattern = "\\.RData$", full.names = TRUE, ignore.case = TRUE)
length(files)
basename(files)

for (f in files) {
  year <- regmatches(basename(f), regexpr("\\d{4}", basename(f)))
  env <- new.env()
  load(f, envir = env)
  obj_name <- ls(env)[1] 
  assign(paste0("teds_", year), get(obj_name, envir = env))
}

# check that it worked
nrow(teds_2015)
nrow(teds_2016)
nrow(teds_2017)
nrow(teds_2018)
nrow(teds_2019)
nrow(teds_2020)
nrow(teds_2021)
nrow(teds_2022)
nrow(teds_2023)
nrow(teds_2024)

sapply(list(teds_2015, teds_2016, teds_2017, teds_2018, teds_2019,
            teds_2020, teds_2021, teds_2022, teds_2023, teds_2024), nrow)

class(teds_2015)
class(teds_2022)

# fix 2020 and 2021

teds_2020 <- teds_2020 |> 
  mutate(
    across(where(haven::is.labelled), ~ as.numeric(haven::zap_labels(.)))
  )

teds_2021 <- teds_2021 |> 
  mutate(
    across(where(haven::is.labelled), ~ as.numeric(haven::zap_labels(.)))
  )

# should be all 0s
sapply(mget(paste0("teds_", 2015:2024)), function(x) sum(sapply(x, haven::is.labelled)))

############################################
# check all covariate names across all years
############################################

covars_all <- c("RACE", "ETHNIC", "EMPLOY", "MARSTAT", "LIVARAG", "HLTHINS", 
                "ALCFLG", "COKEFLG", "PCPFLG", "HALLFLG", "MTHAMFLG", 
                "STIMFLG", "BENZFLG", "SEDHPFLG", "OTHERFLG", "DSMCRIT",
                "PRIMPAY", "REASON", "LOS", "SUB1", "SUB2", "SUB3",
                "FREQ1", "FREQ2", "FREQ3", "SERVICES", "AGE", "PRIMPAY", "SEX", "GENDER",
                "NOPRIOR", "PSOURCE")

year_list <- list(teds_2015, teds_2016, teds_2017, teds_2018, teds_2019,
                  teds_2020, teds_2021, teds_2022, teds_2023, teds_2024)

names(year_list) <- 2015:2024

presence_matrix <- sapply(year_list, function(df) covars_all %in% names(df))

rownames(presence_matrix) <- covars_all

presence_matrix  # FALSE anywhere = that variable is missing/renamed in that year

##################### 
# harmonize var names
#####################

teds_2015 <- teds_2015 |> rename(SEX = GENDER)
teds_2016 <- teds_2016 |> rename(SEX = GENDER)
teds_2017 <- teds_2017 |> rename(SEX = GENDER)
teds_2018 <- teds_2018 |> rename(SEX = GENDER)
teds_2019 <- teds_2019 |> rename(SEX = GENDER)
teds_2020 <- teds_2020 |> rename(SEX = GENDER)
teds_2021 <- teds_2021 |> rename(SEX = GENDER)
teds_2022 <- teds_2022 |> rename(SEX = GENDER)

sapply(list(teds_2015, teds_2016, teds_2017, teds_2018, teds_2019,
            teds_2020, teds_2021, teds_2022, teds_2023, teds_2024),
       function(df) "SEX" %in% names(df)
)

##################### 
# combine 2015-2024
#####################

teds_all <- bind_rows(
  teds_2015 |> mutate(source_year = 2015),
  teds_2016 |> mutate(source_year = 2016),
  teds_2017 |> mutate(source_year = 2017),
  teds_2018 |> mutate(source_year = 2018),
  teds_2019 |> mutate(source_year = 2019),
  teds_2020 |> mutate(source_year = 2020),
  teds_2021 |> mutate(source_year = 2021),
  teds_2022 |> mutate(source_year = 2022),
  teds_2023 |> mutate(source_year = 2023),
  teds_2024 |> mutate(source_year = 2024)
)

# check that it worked
nrow(teds_all) # n = 14824608
dim(teds_all)  # n = 14824608

# check selection criteria vars
teds_all |> count(SERVICES)
teds_all |> count(AGE)
teds_all |> count(DSMCRIT)

# check outcomes
teds_all |> count(REASON)
teds_all |> count(LOS)
teds_all |> count(METHUSE)

# check exposures
teds_all |> count(SUB1)
teds_all |> count(FREQ1)

teds_all |> count(SUB2)
teds_all |> count(FREQ2)

teds_all |> count(SUB3)
teds_all |> count(FREQ3)

##################### 
# -9s to NAs
#####################

teds_all <- teds_all |> 
  mutate(across(c(SUB1, SUB2, SUB3, FREQ1, FREQ2, FREQ3,
                  SEX, RACE, ETHNIC, EMPLOY, MARSTAT, LOS,
                  LIVARAG, HLTHINS, PRIMPAY, DSMCRIT, PSYPROB,
                  NOPRIOR, METHUSE, PSOURCE),
                ~ na_if(.x, -9)))

# check
teds_all |> count(SUB1)
teds_all |> count(FREQ1)
teds_all |> count(SUB2)
teds_all |> count(FREQ2)
teds_all |> count(SUB3)
teds_all |> count(FREQ3)
teds_all |> count(SEX)
teds_all |> count(RACE)
teds_all |> count(ETHNIC) 
teds_all |> count(EMPLOY)
teds_all |> count(MARSTAT)
teds_all |> count(LIVARAG)
teds_all |> count(HLTHINS) 
teds_all |> count(PRIMPAY)
# teds_all |> count(DSMCRIT) # for now, only 5, 12 DSMCRIT 
teds_all |> count(PSYPROB)
teds_all |> count(LOS) |> print(n=38)
teds_all |> count(METHUSE)
teds_all |> count(PSOURCE)
teds_all |> count(NOPRIOR)



####################
# DSMCRIT vs PSYPROB
####################

teds_mh <- teds_all |> 
  filter(
    SERVICES %in% c(3, 4, 5, 6, 7),
    AGE >= 3,
  )

nrow(teds_mh) # n = 11709446

# among PSYPROB = 1, characteristics of DSMCRIT
mh_table <- teds_mh |> 
  filter(PSYPROB == 1) |> 
  count(DSMCRIT) |> 
  mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
#  arrange(desc(n))

# DSMCRIT only
teds_mh |>
  count(DSMCRIT) |>
  mutate(pct = n / sum(n) * 100) |>
  arrange(desc(n)) |>
  slice_head(n = 5)

# correct missings from -9 to NAs
teds_mh <- teds_mh |> 
  mutate(PSYPROB = na_if(PSYPROB, -9),
         DSMCRIT = na_if(DSMCRIT, -9))

# among all PSYPROB, characteristics of DSMCRIT
teds_mh |> 
  filter(!is.na(PSYPROB), !is.na(DSMCRIT)) |> 
  count(PSYPROB, DSMCRIT) |> 
  group_by(PSYPROB) |> 
  mutate(pct = n / sum(n) * 100) |> 
  ungroup() |> 
  arrange(PSYPROB, DSMCRIT) |> 
  print(n = 38)

lab_dsmcrit_full <- c(
  "1" = "Alcohol-induced disorder",
  "2" = "Substance-induced disorder",
  "3" = "Alcohol intoxication",
  "4" = "Alcohol dependence",
  "5" = "Opioid dependence",
  "6" = "Cocaine dependence",
  "7" = "Cannabis dependence",
  "8" = "Other drug dependence",
  "9" = "Alcohol abuse",
  "10" = "Cannabis abuse",
  "11" = "Other substance abuse",
  "12" = "Opioid abuse",
  "13" = "Other drug abuse",
  "14" = "Anxiety disorders",
  "15" = "Depressive disorders",
  "16" = "Schizophrenia/other psychotic disorders",
  "17" = "Bipolar disorders",
  "18" = "Attention deficit/disruptive behavior disorders",
  "19" = "Other mental health condition"
)

dsmcrit_wide <- teds_mh |> 
  filter(!is.na(PSYPROB), !is.na(DSMCRIT)) |> 
  count(PSYPROB, DSMCRIT) |> 
  group_by(PSYPROB) |> 
  mutate(pct = round(n / sum(n) * 100, 1)) |> 
  ungroup() |>
  mutate(DSMCRIT_lab = lab_dsmcrit_full[as.character(DSMCRIT)]) |>
  select(DSMCRIT, DSMCRIT_lab, PSYPROB, pct) |>
  pivot_wider(names_from = PSYPROB, values_from = pct, names_prefix = "PSYPROB_") |>
  rename(`Co-occurring MH/SUD (%)` = PSYPROB_1, `No Co-occurring MH/SUD (%)` = PSYPROB_2) |>
  arrange(DSMCRIT)

dsmcrit_wide

#########################################################################################
# sample:

# Participants reporting “opioid dependence or opioid abuse” (DSMCRIT) at admission among
# patients receiving SERVICES 3-7 (exclude 1,2,8) among AGE 18+.
#########################################################################################

# apply selection criteria

teds_clean <- teds_all |> 
  filter(
    SERVICES %in% c(3, 4, 5, 6, 7),
    AGE >= 3,
    DSMCRIT %in% c(5,12))

# check that it worked
nrow(teds_clean)  # n = 2332627
teds_clean |> count(SERVICES) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
teds_clean |> count(AGE) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
teds_clean |> count(DSMCRIT) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)

# US states (census state FIPS code)
teds_clean |> 
  group_by(STFIPS) |> 
  summarise(n = n()) |> 
  print(n = 60)

###############
# exposures
###############

# primary:
# marijuana/hashish reported in SUB1, SUB2, SUB3 (yes vs. no)

teds_clean <- teds_clean |> 
  mutate(
    cannabis = case_when(
      SUB1 == 4 | SUB2 == 4 | SUB3 == 4 ~ 1,
      TRUE ~ 0
    )
  )

teds_clean |> count(cannabis) |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(SUB1)
teds_clean |> count(SUB2)
teds_clean |> count(SUB3)
teds_clean |> count(cannabis)

# secondary:
# categorize into frequency groups based on FREQ1, FREQ2, and FREQ3

# from those who reported cannabis in SUB1-3, pull FREQ1-3
teds_clean <- teds_clean |> 
  mutate(
    cannabis_freq_raw = pmax(
      if_else(SUB1 == 4, FREQ1, NA_real_),
      if_else(SUB2 == 4, FREQ2, NA_real_),
      if_else(SUB3 == 4, FREQ3, NA_real_),
      na.rm = TRUE
    ),
  )

# check (should be 1, 2, 3, and NA - no -9)
teds_clean |> count(cannabis_freq_raw)

# final category
teds_clean <- teds_clean |> 
  mutate(
    cannabis_freq = case_when(
      cannabis == 0                            ~ "No cannabis use",
      # cannabis == 1 & is.na(cannabis_freq_raw) ~ "No use past month", # removed to keep NA FREQs as NAs
      cannabis_freq_raw == 1                   ~ "No use past month",
      cannabis_freq_raw == 2                   ~ "Some use past month",
      cannabis_freq_raw == 3                   ~ "Daily use past month",
      TRUE ~ NA_character_
    ),
    cannabis_freq = factor(
      cannabis_freq,
      levels = c(
        "No cannabis use",
        "No use past month",
        "Some use past month",
        "Daily use past month")
    )
  )

# check totals
teds_clean |> count(cannabis_freq) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)

# check NAs - this should be 7400
teds_clean |> filter(is.na(cannabis_freq)) |> count(cannabis)

###############
# outcomes
###############

# primary: REASON

teds_clean <- teds_clean |> 
  mutate(
    reason_2 = case_when( # primary analysis 1 vs 2-7
      REASON == 1     ~ 1,
      REASON %in% 2:7 ~ 0,
      TRUE ~ NA_real_
    ),
    reason_2 = factor(
      reason_2,
      levels = c(0,1),
      labels = c("Other",
                 "Completed")
    ),
    reason_3 = case_when( # secondary analysis 1 vs 2,3
      REASON == 1     ~ 1,
      REASON %in% 2:3 ~ 0,
      TRUE ~ NA_real_
    ),
    reason_3 = factor(
      reason_3,
      levels = c(0,1),
      labels = c("Dropped out/terminated",
                 "Completed")
    ),
    reason_4 = case_when( # sensitivity analysis: 1 & 4 vs 2,3,5,6, & 7. 
      REASON %in% c(1,4)       ~ 1,
      REASON %in% c(2,3,5,6,7) ~ 0,
      TRUE ~ NA_real_
    ),
    reason_4 = factor(
      reason_4,
      levels = c(0,1),
      labels = c("Other",
                 "Completed/Transferred")
    ),
  )

teds_clean |> count(REASON) |>  mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
teds_clean |> count(REASON, reason_2, reason_3, reason_4)
teds_clean |> count(reason_2) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
teds_clean |> count(reason_3) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
teds_clean |> count(reason_4) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)

# secondary: LOS

teds_clean <- teds_clean |> 
  mutate(
    los_91 = case_when(
      LOS %in% 1:33  ~ "=< 90 days",
      LOS %in% 34:37 ~ "> 90 days",
      TRUE ~ NA_character_
    ),
    los_91 = factor(
      los_91, levels = c("=< 90 days", "> 90 days")
    )
  )

teds_clean |> count(LOS) |> print(n = 38)
teds_clean |> count(los_91) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
teds_clean |> count(LOS, los_91) |> print(n = 38)

# LOS > 180 days

teds_clean <- teds_clean |> 
  mutate(
    los_181 = case_when(
      LOS %in% 1:35  ~ "=< 180 days",
      LOS %in% 36:37 ~ "> 180 days",
      TRUE ~ NA_character_
    ),
    los_181 = factor(
      los_181, levels = c("=< 180 days", "> 180 days")
    )
  )

teds_clean |> count(los_181) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
teds_clean |> count(LOS, los_181) |> print(n = 38)

# tertiary: METHUSE aka medication-assisted opioid therapy (y/n)

teds_clean <- teds_clean |>
  mutate(
    methuse_2 = case_when(
      METHUSE %in% 2 ~ 0,
      METHUSE %in% 1 ~ 1,
      TRUE ~ NA_real_
    ),
    methuse_2 = factor(
      methuse_2,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )
  
teds_clean |> count(METHUSE, methuse_2)
teds_clean |> count(methuse_2) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)

#############
# covariates
#############

# descriptives # 

covars <- c("SEX", "RACE", "ETHNIC", "EMPLOY", "MARSTAT",
            "LIVARAG", "HLTHINS", "PRIMPAY", "PSYPROB", 
            "ALCFLG", "COKEFLG", "PCPFLG", "HALLFLG",
            "MTHAMFLG", "STIMFLG", "BENZFLG", "SEDHPFLG", "OTHERFLG",
            "NOPRIOR", "PSOURCE")

covar_table <- covars |> 
  set_names() |> 
  map(\(v) {
    teds_clean |> 
      count(.data[[v]]) |> 
      mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)
  })
covar_table 

####################
# operationalization
####################

# AGE by decade
teds_clean <- teds_clean |> 
  mutate(
    age_dec = case_when(
      AGE %in% 3:6   ~ "18-34", # not 18-35
      AGE %in% 7:9   ~ "35-49", # not 36-50
      AGE %in% 10:11 ~ "50-64", # not 51-64
      AGE %in% 12    ~ "65+",
      TRUE ~ NA_character_
    ),
    age_dec = factor(age_dec,
                     levels = c("18-34", "35-49", "50-64", "65+"))
  )

teds_clean |> count(age_dec)
teds_clean |> count(AGE, age_dec)
teds_clean |> count(age_dec) |> mutate(pct = n / sum(n) * 100, flag_small_cell = n < 11)

# sex
teds_clean <- teds_clean |> 
  mutate(
    sex_2 = factor(
      SEX,
      levels = c(1,2),
      labels = c("Male",
                 "Female"))
  )

teds_clean |> count(SEX)
teds_clean |> count(SEX, sex_2) # check Male is ref category
levels(teds_clean$sex_2)

# race
teds_clean <- teds_clean |> 
  mutate(
    race_2 = case_when(
      RACE == 5 ~ "White",
      RACE == 4 ~ "Black/African American",
      RACE %in% c(1,2,3,6,7,8,9) ~ "Other",
      TRUE ~ NA_character_
    ),
    race_2 = factor(
      race_2,
      levels = c(
        "White",
        "Black/African American",
        "Other"))
  )

teds_clean |> count(RACE, race_2)
teds_clean |> count(race_2) |> mutate(pct = n / sum(n) * 100)

# ethnic
teds_clean <- teds_clean |> 
  mutate(
    ethnic_2 = case_when(
      ETHNIC == 4 ~ "Non-Hispanic",
      ETHNIC %in% c(1,2,3,5) ~ "Hispanic",
      TRUE ~ NA_character_
    ),
    ethnic_2 = factor(
      ethnic_2,
      levels = c(
        "Non-Hispanic",
        "Hispanic"))
  )

teds_clean |> count(ETHNIC, ethnic_2)
teds_clean |> count(ethnic_2) |> mutate(pct = n / sum(n) * 100)

# employ
teds_clean <- teds_clean |> 
  mutate(
    employ_2 = case_when(
      EMPLOY %in% c(1,2) ~ "Employed",
      EMPLOY %in% c(3,4) ~ "Not employed",
      TRUE ~ NA_character_
    ),
    employ_2 = factor(
      employ_2,
      levels = c(
        "Employed",
        "Not employed"))
  )

teds_clean |> count(EMPLOY, employ_2)
teds_clean |> count(employ_2) |> mutate(pct = n / sum(n) * 100)

# marstat
teds_clean <- teds_clean |> 
  mutate(
    marstat_2 = case_when(
      MARSTAT == 2 ~ "Married",
      MARSTAT %in% c(1,3,4) ~ "Not Married",
      TRUE ~ NA_character_
    ),
    marstat_2 = factor(
      marstat_2,
      levels = c(
        "Married",
        "Not Married"))
  )

teds_clean |> count(MARSTAT, marstat_2)
teds_clean |> count(marstat_2) |> mutate(pct = n / sum(n) * 100)

# livarag
teds_clean <- teds_clean |> 
  mutate(
    livarag_2 = case_when(
      LIVARAG %in% c(2,3) ~ "Housed",
      LIVARAG == 1 ~ "Homeless",
      TRUE ~ NA_character_
    ),
    livarag_2 = factor(
      livarag_2,
      levels = c(
        "Housed",
        "Homeless"))
  )

teds_clean |> count(LIVARAG, livarag_2)
teds_clean |> count(livarag_2) |> mutate(pct = n / sum(n) * 100)

# psyprob
teds_clean <- teds_clean |> 
  mutate(
    psyprob_2 = case_when(
      PSYPROB == 2 ~ 0,
      PSYPROB == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    psyprob_2 = factor(
      psyprob_2,
      levels = c(0,1),
      labels = c("No co-occurring MH & SUD",
                 "Co-occurring MH & SUD"))
  )

teds_clean |> count(PSYPROB, psyprob_2)
teds_clean |> count(psyprob_2) |> mutate(pct = n / sum(n) * 100)



##########################
##########################
# BOOKMARK
##########################
##########################


# flags
flag_vars <- c("ALCFLG", "COKEFLG", "PCPFLG", "HALLFLG",
               "MTHAMFLG", "STIMFLG", "BENZFLG", "SEDHPFLG", "OTHERFLG")

EXCLUDED_flag_vars <- (c("MARFLG", "HERFLG", "METHFLG", "OPSYNFLG",
                         "AMPHFLG", "TRNQFLG", "BARBFLG", "INHFLG", "OCTFLG"))

teds_clean |> count(ALCFLG)

teds_clean <- teds_clean |> 
  mutate(
    across(
      all_of(flag_vars),
      ~ factor(.x,
               levels = c(0,1),
               labels = c("No", "Yes")),
      .names = "{.col}_2"
    )
  ) |> 
  set_variable_labels(
    ALCFLG_2   = "Alcohol",
    COKEFLG_2  = "Cocaine/crack",
    PCPFLG_2   = "PCP",
    HALLFLG_2  = "Hallucinogens",
    MTHAMFLG_2 = "Methamphetamine/speed",
    STIMFLG_2  = "Other stimulants",
    BENZFLG_2  = "Benzodiazepines",
    SEDHPFLG_2 = "Other non-benzodiazepine sedatives/hypnotics",
    OTHERFLG_2 = "Other drugs"
  )

teds_clean |> count(ALCFLG, ALCFLG_2)


teds_clean |> count(ALCFLG)   |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(COKEFLG, COKEFLF_2)  |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(PCPFLG)   |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(HALLFLG)  |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(MTHAMFLG) |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(STIMFLG)  |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(BENZFLG)  |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(SEDHPFLG) |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(OTHERFLG) |> mutate(pct = n / sum(n) * 100)

####################
# health insurance
####################

# HLTHINS investigation
teds_clean |> count(HLTHINS) 

# HLTHINS x cannabis
teds_clean |> count(cannabis) 
teds_clean |> filter(!is.na(cannabis), !is.na(HLTHINS)) |> nrow()

teds_clean |>
  count(cannabis, HLTHINS) |>
  group_by(cannabis) |>
  mutate(pct = n / sum(n) * 100)

chi_can_hlth <- teds_clean |> 
  filter(!is.na(HLTHINS)) |>
  count(cannabis, HLTHINS) |> 
  pivot_wider(names_from = HLTHINS, values_from = n, values_fill = 0) |> 
  column_to_rownames(var = "cannabis") |> 
  as.matrix()

chisq.test(chi_can_hlth)
chisq.test(chi_can_hlth)$expected

# HLTHINS x REASON
teds_clean |> count(REASON)
teds_clean |> filter(!is.na(cannabis), !is.na(HLTHINS)) |> nrow()

teds_clean |>
  count(REASON, HLTHINS) |>
  group_by(REASON) |>
  mutate(pct = n / sum(n) * 100)

chi_reason_hlth <- teds_clean |> 
  filter(!is.na(REASON), !is.na(HLTHINS)) |>
  count(REASON, HLTHINS) |> 
  pivot_wider(names_from = HLTHINS, values_from = n, values_fill = 0) |> 
  column_to_rownames(var = "REASON") |> 
  as.matrix()

chisq.test(chi_reason_hlth)
chisq.test(chi_reason_hlth)$expected

# HLTHINS x PRIMPAY
teds_clean |> count(PRIMPAY)
teds_clean |> filter(!is.na(PRIMPAY), !is.na(HLTHINS)) |> nrow()

teds_clean |> 
  count(PRIMPAY, HLTHINS) |> 
  group_by(PRIMPAY) |> 
  mutate(pct = n / sum(n) * 100)

chi_prim_hlth <- teds_clean |> 
  filter(!is.na(PRIMPAY), !is.na(HLTHINS)) |>
  count(PRIMPAY, HLTHINS) |> 
  pivot_wider(names_from = HLTHINS, values_from = n, values_fill = 0) |> 
  column_to_rownames(var = "PRIMPAY") |> 
  as.matrix()

chisq.test(chi_prim_hlth)
chisq.test(chi_prim_hlth)$expected

# effect size (Cramers V) - cannabis 
effect_size <- function(chisq_stat, n, tab) {
  k <- min(dim(tab))
  sqrt(chisq_stat / (n * (k-1)))
}

n_can <- sum(chi_can_hlth)
effect_size(8262.9, n_can, chi_can_hlth)

# effect size (Cramers V) - reason 
n_reason <- sum(chi_reason_hlth)
effect_size(49464, n_reason, chi_reason_hlth)

# effect size (Cramers V) - PRIMPAY
n_PRIMPAY <- sum(chi_prim_hlth)
effect_size(1719214, n_PRIMPAY, chi_prim_hlth)

# PRIMPAY in HLTHINS?
teds_clean |> 
  filter(PRIMPAY %in% c(5, 6, 7)) |> 
  count(PRIMPAY, HLTHINS) |> 
  group_by(PRIMPAY) |> 
  mutate(pct = n / sum(n) * 100) |> 
  ungroup()

teds_clean |> 
  filter(PRIMPAY %in% c(5,6,7), is.na(HLTHINS)) |> 
  count(STFIPS, PRIMPAY, HLTHINS, sort = TRUE)

# new combined variable
teds_clean <- teds_clean |> 
  mutate(
    pay_ins = case_when(
      PRIMPAY == 1 ~ 1, # self pay
      PRIMPAY == 2 ~ 2, # private insurance 
      PRIMPAY == 3 ~ 3, # medicare
      PRIMPAY == 4 ~ 4, # medicaid
      HLTHINS == 1 ~ 2, # private insurance 
      HLTHINS == 3 ~ 3, # medicare
      HLTHINS == 2 ~ 4, # medicaid
      PRIMPAY %in% c(5,6,7) ~ 5, # other/none
      HLTHINS == 4 ~ 5, # other/none
      TRUE ~ NA_real_,
    ), 
    pay_ins = factor(pay_ins,
                     levels = 1:5,
                     labels = c("Other/none",
                                "Self-pay",
                                "Private insurance",
                                "Medicare",
                                "Medicaid"
                     ))
  )

teds_clean |> count(pay_ins) |> mutate(pct = n / sum(n) * 100)
teds_clean |> count(PRIMPAY, HLTHINS, pay_ins) |> print(n = 50)

# pay_ins source (PRIMPAY vs HLTHINS)
teds_clean <- teds_clean |> 
  mutate(
    pay_ins_src = case_when(
      PRIMPAY %in% 1:7 ~ "PRIMPAY",
      HLTHINS %in% 1:4 ~ "HLTHINS",
      !is.na(pay_ins) ~ "Other/none",
      TRUE ~ NA_character_
    )
  )

teds_clean |> count(pay_ins, pay_ins_src)

# 2nd HLTHINS variable
teds_clean <- teds_clean |> 
  mutate(
    hlthins_2 = case_when(
      HLTHINS == 1 ~ "Private",
      HLTHINS %in% c(2,3) ~ "Public",
      HLTHINS == 4 ~ "None",
      TRUE ~ NA_character_
    ),
    hlthins_2 = factor(
      hlthins_2,
      levels = c(
        "Private",
        "Public",
        "None"))
  )

teds_clean |> count(hlthins_2) |> mutate(pct = n / sum(n) * 100)

# overall check
teds_clean |> count(HLTHINS, hlthins_2)
teds_clean |> count(HLTHINS)
teds_clean |> count(PRIMPAY)


# HLTHINS dummy vars 

teds_clean |> count(HLTHINS)

teds_clean <- teds_clean |> 
  mutate(
    hlthins_3 = factor(
      HLTHINS,
      levels = c(1,2,3,4),
      labels = c("Private",
                 "Medicaid",
                 "Medicare/other",
                 "None"))
  )

teds_clean |> count(hlthins_3)
teds_clean |> count(HLTHINS, hlthins_3)
levels(teds_clean$hlthins_3) # check private is ref


# HLTHINS missingness

teds_clean |> 
  group_by(STFIPS) |> 
  summarise(n = n(), pct_missing = round(mean(is.na(HLTHINS)) * 100, 1)) |> 
  arrange(desc(pct_missing)) |> 
  print(n = 60)

teds_clean |> 
  group_by(STFIPS) |> 
  summarise(n = n(), pct_missing = round(mean(is.na(PRIMPAY)) * 100, 1)) |> 
  arrange(desc(pct_missing)) |> 
  print(n = 60)

teds_clean |> 
  filter(STFIPS %in% c(6, 26, 27, 36, 37, 51, 53, 55)) |> 
  summarise(n = n(),
            all_missing_both = all(is.na(HLTHINS) & is.na(PRIMPAY)))

teds_clean |> 
  filter(STFIPS %in% c(6, 26, 27, 36, 37, 51, 53, 55)) |> 
  group_by(STFIPS) |> 
  summarise(
    n            = n(),
    hlthins_obs  = sum(!is.na(HLTHINS)),
    primpay_obs  = sum(!is.na(PRIMPAY)),
    missing_both = sum(is.na(HLTHINS) & is.na(PRIMPAY))
  ) |> 
  janitor::adorn_totals()

teds_clean |> 
  filter(STFIPS == 37, !is.na(HLTHINS)) |> 
  count(source_year, HLTHINS)

# in case of filtering by missing states:
nonreporting_states <- c(6, 26, 27, 36, 51, 53, 55)

teds_hlthins <- teds_clean |> 
  filter(!STFIPS %in% nonreporting_states)

nrow(teds_hlthins) # n = 2006710

#############
# OR function
#############

# profile likelihood intervals
or_ci <- function(model, digits = 2) {
  out <- exp(cbind(
    OR = coef(model),
    confint(model)
  ))
  return(out)
}

# Wald intervals (estimate +- 1.96 * SE)
or_ci_wald <- function(model) {
  exp(cbind(OR = coef(model), confint.default(model)))
}

or_ci_wald_tbl <- function(model) {
  or_ci_wald(model) |>
    as.data.frame() |>
    rownames_to_column("term") |>
    as_tibble() |>
    rename(estimate = OR, conf.low = `2.5 %`, conf.high = `97.5 %`)
}

#################################################################
# Models

# Exposures:
#   primary:   'cannabis'
#   secondary: 'cannabis_freq'

# Primary Outcome: REASON
#   a) REASON (1) vs (2-7) 'reason_2'
#   b) REASON (1) vs (2,3) 'reason_3'

# Secondary Outcome: LOS
#   a) LOS > 180 days 'los_181'
#   b) continuous (if this is possible...?)

# b0: crude
# b1: + sociodemos
# b2: + hlthins
# b3: + substance flags
# b4: + psyprob
#################################################################

# confirm levels
map(teds_clean[c("age_dec",
                 "sex_2",
                 "ethnic_2",
                 "employ_2",
                 "marstat_2",
                 "livarag_2",
                 "psyprob_2",
                 "hlthins_3",
                 "reason_2",
                 "reason_3",
                 "cannabis_freq",
                 "cannabis")],
    levels)

nobs(m_r2_can_b0_og)  # n = 232627
nobs(m_r2_freq_b0_og) # n = 232627
nobs(m_r3_can_b0_og)  # n = 1341510

#######
# b0
#######

m_r2_can_b0_og <- glm(reason_2 ~ cannabis, data = teds_clean, family = binomial)

summary(m_r2_can_b0_og)
or_ci_wald(m_r2_can_b0_og)

# reporting - m_r2_can_b0_og
teds_clean |> 
  filter(!is.na(reason_2), !is.na(cannabis)) |> 
  group_by(cannabis) |> 
  summarise(N = n(),
            completed = sum(reason_2 == "Completed"),
            .groups = "drop") |> 
  mutate(completed_cell = sprintf("%s (%.1f)",
                                  format(completed, big.mark = ","),
                                  completed / N * 100),
         N = format(N, big.mark = ","))

est <- exp(coef(m_r2_can_b0_og)["cannabis"])
ci  <- exp(confint.default(m_r2_can_b0_og)["cannabis", ])

sprintf("%.2f (%.2f–%.2f)", est, ci[1], ci[2])

# -------------------------------------
# primary exposure: cannabis frequency 
# -------------------------------------

m_r2_freq_b0_og <- glm(reason_2 ~ cannabis_freq, data = teds_clean, family = binomial)

summary(m_r2_freq_b0_og)
or_ci_wald(m_r2_freq_b0_og)

# reporting - EXP 2 & primary (1) vs (2-7)
teds_clean |> 
  filter(!is.na(reason_2), !is.na(cannabis_freq)) |> 
  group_by(cannabis_freq) |> 
  summarise(N = n(),
            completed = sum(reason_2 == "Completed"),
            .groups = "drop") |> 
  mutate(completed_cell = sprintf("%s (%.1f)",
                                  format(completed, big.mark = ","),
                                  completed / N * 100),
         N = format(N, big.mark = ","))

est <- exp(coef(m_r2_freq_b0_og)["cannabis_freq"])
ci <- exp(confint.default(m_r2_freq_b0_og)["cannabis_freq", ])

sprintf("%.2f (%.2f–%.2f)", est, ci[1], ci[2])


# outcome: REASON, secondary: treatment completion (1) vs (2,3)

m_r3_can_b0_og <- glm(reason_3 ~ cannabis, data = teds_clean, family = binomial)

summary(m_r3_can_b0_og)
or_ci_wald(m_r3_can_b0_og)

m_r3_freq_b0_og <- glm(reason_3 ~ cannabis_freq, data = teds_clean, family = binomial)

summary(m_r3_freq_b0_og)
or_ci_wald(m_r3_freq_b0_og)

# reporting - EXP 1 & primary (1) vs (2,3)

teds_clean |> 
  filter(!is.na(reason_3), !is.na(cannabis)) |> 
  group_by(cannabis) |> 
  summarise(N = n(),
            completed = sum(reason_3 == "Completed"),
            .groups = "drop") |> 
  mutate(completed_cell = sprintf("%s (%.1f)",
                                  format(completed, big.mark = ","),
                                  completed / N * 100),
         N = format(N, big.mark = ","))

est <- exp(coef(m_r3_can_b0_og)["cannabis"])
ci  <- exp(confint.default(m_r3_can_b0_og)["cannabis", ])

sprintf("%.2f (%.2f–%.2f)", est, ci[1], ci[2])

# reporting - EXP 2 & primary (1) vs (2,3)

teds_clean |> 
  filter(!is.na(reason_3), !is.na(cannabis_freq)) |> 
  group_by(cannabis_freq) |> 
  summarise(N = n(),
            completed = sum(reason_3 == "Completed"),
            .groups = "drop") |> 
  mutate(completed_cell = sprintf("%s (%.1f)",
                                  format(completed, big.mark = ","),
                                  completed / N * 100),
         N = format(N, big.mark = ","))

est <- exp(coef(m_r3_freq_b0_og))[-1]
ci  <- exp(confint.default(m_r3_freq_b0_og))[-1, ]

tibble(term = names(est),
       cell = sprintf("%.2f (%.2f–%.2f)", est, ci[, 1], ci[, 2]))


# reporting - primary (1) vs (2,3) N and %s

sec_counts <- function(exposure) {
  teds_clean |> 
    filter(!is.na(reason_3), !is.na({{ exposure }})) |> 
    group_by({{ exposure }}) |> 
    summarise(N = n(),
              completed = sum(reason_3 == "Completed"),
              .groups = "drop") |> 
    mutate(completed_cell = sprintf("%s (%.1f)",
                                    format(completed, big.mark = ","),
                                    completed / N * 100),
           N = format(N, big.mark = ","))
}

sec_counts(cannabis)        # Any cannabis use: No / Yes rows
sec_counts(cannabis_freq)   



#######################
# MODEL BUILDING

# block 1 - sociodemos
# block 2 - hlthins
# block 3 - sub flags
# block 4 - psyprob

#######################

exposures <- c("cannabis", "cannabis_freq")

outcomes <- tribble(
  ~outcome,   ~label,              ~family,
  "reason_2", "REASON 1 vs 2-7",   "binomial",
  "reason_3", "REASON 1 vs 2-3",   "binomial",
  "los_181",  "LOS > 180 days",    "binomial"
  # "los_days", "LOS (continuous)", "gaussian"
)

# ------- samples per model ------- #

all_covars <- c("age_dec", "sex_2", "race_2", "ethnic_2", "employ_2", "marstat_2", "livarag_2",
                "HLTHINS",
                "ALCFLG", "COKEFLG", "PCPFLG", "HALLFLG", "MTHAMFLG", "STIMFLG", "BENZFLG", "SEDHPFLG", "OTHERFLG",
                "PSYPROB")

data_r2 <- teds_clean |> drop_na(reason_2, cannabis, cannabis_freq, all_of(all_covars))
data_r3 <- teds_clean |> drop_na(reason_3, cannabis, cannabis_freq, all_of(all_covars))
data_los <- teds_clean |> drop_na(los_181, cannabis, cannabis_freq, all_of(all_covars))

nrow(data_r2)
nrow(data_r3)
nrow(data_los)
nrow(teds_clean)

# ------- primary outcome: REASON 1 vs 2-7 'reason_2' ------- #

## reason_2 ~ cannabis

m_r2_can_b0 <- glm(reason_2 ~ cannabis, data = data_r2, family = binomial)
m_r2_can_b1 <- update(m_r2_can_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m_r2_can_b2 <- update(m_r2_can_b1, . ~ . + HLTHINS)
m_r2_can_b3 <- update(m_r2_can_b2, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m_r2_can_b4 <- update(m_r2_can_b3, . ~ . + PSYPROB)

summary(m_r2_can_b0)
summary(m_r2_can_b1)
summary(m_r2_can_b2)
summary(m_r2_can_b3)
summary(m_r2_can_b4)

or_ci_wald(m_r2_can_b0)
or_ci_wald(m_r2_can_b1)
or_ci_wald(m_r2_can_b2)
or_ci_wald(m_r2_can_b3)
or_ci_wald(m_r2_can_b4)

## reason_2 ~ cannabis_freq

m_r2_freq_b0 <- glm(reason_2 ~ cannabis_freq, data = data_r2, family = binomial)
m_r2_freq_b1 <- update(m_r2_freq_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m_r2_freq_b2 <- update(m_r2_freq_b1, . ~ . + HLTHINS)
m_r2_freq_b3 <- update(m_r2_freq_b2, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m_r2_freq_b4 <- update(m_r2_freq_b3, . ~ . + PSYPROB)

summary(m_r2_freq_b0)
summary(m_r2_freq_b1)
summary(m_r2_freq_b2)
summary(m_r2_freq_b3)
summary(m_r2_freq_b4)

or_ci_wald(m_r2_freq_b0)
or_ci_wald(m_r2_freq_b1)
or_ci_wald(m_r2_freq_b2)
or_ci_wald(m_r2_freq_b3)
or_ci_wald(m_r2_freq_b4)


# ------- outcome: REASON 1 vs 2,3 'reason_3' ------- #

## reason_3 ~ cannabis

m_r3_can_b0 <- glm(reason_3 ~ cannabis, data = data_r3, family = binomial)
m_r3_can_b1 <- update(m_r3_can_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m_r3_can_b2 <- update(m_r3_can_b1, . ~ . + HLTHINS)
m_r3_can_b3 <- update(m_r3_can_b2, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m_r3_can_b4 <- update(m_r3_can_b3, . ~ . + PSYPROB)

summary(m_r3_can_b0)
summary(m_r3_can_b1)
summary(m_r3_can_b2)
summary(m_r3_can_b3)
summary(m_r3_can_b4)

or_ci_wald(m_r3_can_b0)
or_ci_wald(m_r3_can_b1)
or_ci_wald(m_r3_can_b2)
or_ci_wald(m_r3_can_b3)
or_ci_wald(m_r3_can_b4)

## reason_3 ~ cannabis_freq

m_r3_freq_b0 <- glm(reason_3 ~ cannabis_freq, data = data_r3, family = binomial)
m_r3_freq_b1 <- update(m_r3_freq_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m_r3_freq_b2 <- update(m_r3_freq_b1, . ~ . + HLTHINS)
m_r3_freq_b3 <- update(m_r3_freq_b2, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m_r3_freq_b4 <- update(m_r3_freq_b3, . ~ . + PSYPROB)

summary(m_r3_freq_b0)
summary(m_r3_freq_b1)
summary(m_r3_freq_b2)
summary(m_r3_freq_b3)
summary(m_r3_freq_b4)

or_ci_wald(m_r3_freq_b0)
or_ci_wald(m_r3_freq_b1)
or_ci_wald(m_r3_freq_b2)
or_ci_wald(m_r3_freq_b3)
or_ci_wald(m_r3_freq_b4)

# ------- outcome: LOS > 180 days 'los_181' ------- #

## los_181 ~ cannabis

m_los_can_b0 <- glm(los_181 ~ cannabis, data = data_los, family = binomial)
m_los_can_b1 <- update(m_los_can_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m_los_can_b2 <- update(m_los_can_b1, . ~ . + HLTHINS)
m_los_can_b3 <- update(m_los_can_b2, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m_los_can_b4 <- update(m_los_can_b3, . ~ . + PSYPROB)

summary(m_los_can_b0)
summary(m_los_can_b1)
summary(m_los_can_b2)
summary(m_los_can_b3)
summary(m_los_can_b4)

or_ci_wald(m_los_can_b0)
or_ci_wald(m_los_can_b1)
or_ci_wald(m_los_can_b2)
or_ci_wald(m_los_can_b3)
or_ci_wald(m_los_can_b4)

## los_181 ~ cannabis_freq  

m_los_freq_b0 <- glm(los_181 ~ cannabis_freq, data = data_los, family = binomial)
m_los_freq_b1 <- update(m_los_freq_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m_los_freq_b2 <- update(m_los_freq_b1, . ~ . + HLTHINS)
m_los_freq_b3 <- update(m_los_freq_b2, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m_los_freq_b4 <- update(m_los_freq_b3, . ~ . + PSYPROB)

summary(m_los_freq_b0)
summary(m_los_freq_b1)
summary(m_los_freq_b2)
summary(m_los_freq_b3)
summary(m_los_freq_b4)

or_ci_wald(m_los_freq_b0)
or_ci_wald(m_los_freq_b1)
or_ci_wald(m_los_freq_b2)
or_ci_wald(m_los_freq_b3)
or_ci_wald(m_los_freq_b4)


# ------- ORs ------- #

## collect all models
model_list <- list(
  m_r2_can_b0 = m_r2_can_b0,
  m_r2_can_b1 = m_r2_can_b1,
  m_r2_can_b2 = m_r2_can_b2,
  m_r2_can_b3 = m_r2_can_b3,
  m_r2_can_b4 = m_r2_can_b4,
  m_r2_freq_b0 = m_r2_freq_b0,
  m_r2_freq_b1 = m_r2_freq_b1,
  m_r2_freq_b2 = m_r2_freq_b2,
  m_r2_freq_b3 = m_r2_freq_b3,
  m_r2_freq_b4 = m_r2_freq_b4,
  m_r3_can_b0 = m_r3_can_b0,
  m_r3_can_b1 = m_r3_can_b1,
  m_r3_can_b2 = m_r3_can_b2,
  m_r3_can_b3 = m_r3_can_b3,
  m_r3_can_b4 = m_r3_can_b4,
  m_r3_freq_b0 = m_r3_freq_b0,
  m_r3_freq_b1 = m_r3_freq_b1,
  m_r3_freq_b2 = m_r3_freq_b2,
  m_r3_freq_b3 = m_r3_freq_b3,
  m_r3_freq_b4 = m_r3_freq_b4,
  m_los_can_b0 = m_los_can_b0,
  m_los_can_b1 = m_los_can_b1,
  m_los_can_b2 = m_los_can_b2,
  m_los_can_b3 = m_los_can_b3,
  m_los_can_b4 = m_los_can_b4,
  m_los_freq_b0 = m_los_freq_b0,
  m_los_freq_b1 = m_los_freq_b1,
  m_los_freq_b2 = m_los_freq_b2,
  m_los_freq_b3 = m_los_freq_b3,
  m_los_freq_b4 = m_los_freq_b4
)

## extract ORs 
results <- imap(model_list, \(m, nm) {
  or_ci_wald_tbl(m) |>
    filter(str_starts(term, "cannabis")) |>
    mutate(model_name = nm, n = nobs(m))
}) |>
  list_rbind() |>
  separate_wider_delim(model_name, delim = "_",
                       names = c(NA, "outcome", "exposure", "model"))

## n's across models
results |> distinct(outcome, exposure, model, n) |> print(n = 30)

results_table <- results |>
  mutate(est = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) |>
  select(outcome, exposure, term, model, est) |>
  pivot_wider(names_from = model, values_from = est)
results_table

dir.create("output", showWarnings = FALSE)
saveRDS(model_list, "output/model_list.rds")
write_csv(results, "output/results_long.csv")
write_csv(results_table, "output/results_table.csv")



#################################################################
# MODEL BUILDING - NO HLTHINS, data = teds_clean

# Exposures:
#   primary:   'cannabis'
#   secondary: 'cannabis_freq'

# Primary Outcome: REASON
#   a) REASON (1) vs (2-7) 'reason_2'
#   b) REASON (1) vs (2,3) 'reason_3'

# Secondary Outcome: LOS
#   a) LOS > 180 days 'los_181'
#   b) continuous (if this is possible...?)

# b0: crude
# b1: + sociodemos
# b2: + substance flags
# b3: + psyprob
#################################################################

# ------- primary outcome: REASON 1 vs 2-7 'reason_2' ------- #

## reason_2 ~ cannabis

m2_r2_can_b0 <- glm(reason_2 ~ cannabis, data = teds_clean, family = binomial)
m2_r2_can_b1 <- update(m2_r2_can_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m2_r2_can_b2 <- update(m2_r2_can_b1, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m2_r2_can_b3 <- update(m2_r2_can_b2, . ~ . + PSYPROB)

summary(m2_r2_can_b0)
summary(m2_r2_can_b1)
summary(m2_r2_can_b2)
summary(m2_r2_can_b3)

or_ci_wald(m2_r2_can_b0)
or_ci_wald(m2_r2_can_b1)
or_ci_wald(m2_r2_can_b2)
or_ci_wald(m2_r2_can_b3)

nobs(m2_r2_can_b3)


## reason_2 ~ cannabis_freq

m2_r2_freq_b0 <- glm(reason_2 ~ cannabis_freq, data = teds_clean, family = binomial)
m2_r2_freq_b1 <- update(m2_r2_freq_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m2_r2_freq_b2 <- update(m2_r2_freq_b1, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m2_r2_freq_b3 <- update(m2_r2_freq_b2, . ~ . + PSYPROB)

summary(m2_r2_freq_b0)
summary(m2_r2_freq_b1)
summary(m2_r2_freq_b2)
summary(m2_r2_freq_b3)

or_ci_wald(m2_r2_freq_b0)
or_ci_wald(m2_r2_freq_b1)
or_ci_wald(m2_r2_freq_b2)
or_ci_wald(m2_r2_freq_b3)

nobs(m2_r2_freq_b3)

# ------- outcome: REASON 1 vs 2,3 'reason_3' ------- #

## reason_3 ~ cannabis

m2_r3_can_b0 <- glm(reason_3 ~ cannabis, data = teds_clean, family = binomial)
m2_r3_can_b1 <- update(m2_r3_can_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m2_r3_can_b2 <- update(m2_r3_can_b1, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m2_r3_can_b3 <- update(m2_r3_can_b2, . ~ . + PSYPROB)

summary(m2_r3_can_b0)
summary(m2_r3_can_b1)
summary(m2_r3_can_b2)
summary(m2_r3_can_b3)

or_ci_wald(m2_r3_can_b0)
or_ci_wald(m2_r3_can_b1)
or_ci_wald(m2_r3_can_b2)
or_ci_wald(m2_r3_can_b3)

## reason_3 ~ cannabis_freq

m2_r3_freq_b0 <- glm(reason_3 ~ cannabis_freq, data = teds_clean, family = binomial)
m2_r3_freq_b1 <- update(m2_r3_freq_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m2_r3_freq_b2 <- update(m2_r3_freq_b1, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m2_r3_freq_b3 <- update(m2_r3_freq_b2, . ~ . + PSYPROB)

summary(m2_r3_freq_b0)
summary(m2_r3_freq_b1)
summary(m2_r3_freq_b2)
summary(m2_r3_freq_b3)

or_ci_wald(m2_r3_freq_b0)
or_ci_wald(m2_r3_freq_b1)
or_ci_wald(m2_r3_freq_b2)
or_ci_wald(m2_r3_freq_b3)

# ------- outcome: LOS > 180 days 'los_181' ------- #

## los_181 ~ cannabis

m2_los_can_b0 <- glm(los_181 ~ cannabis, data = teds_clean, family = binomial)
m2_los_can_b1 <- update(m2_los_can_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m2_los_can_b2 <- update(m2_los_can_b1, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m2_los_can_b3 <- update(m2_los_can_b2, . ~ . + PSYPROB)

summary(m2_los_can_b0)
summary(m2_los_can_b1)
summary(m2_los_can_b2)
summary(m2_los_can_b3)

or_ci_wald(m2_los_can_b0)
or_ci_wald(m2_los_can_b1)
or_ci_wald(m2_los_can_b2)
or_ci_wald(m2_los_can_b3)

## los_181 ~ cannabis_freq  

m2_los_freq_b0 <- glm(los_181 ~ cannabis_freq, data = teds_clean, family = binomial)
m2_los_freq_b1 <- update(m2_los_freq_b0, . ~ . + age_dec + sex_2 + race_2 + ethnic_2 + employ_2 + marstat_2 + livarag_2)
m2_los_freq_b2 <- update(m2_los_freq_b1, . ~ . + ALCFLG + COKEFLG + PCPFLG + HALLFLG + MTHAMFLG + STIMFLG + BENZFLG + SEDHPFLG + OTHERFLG)
m2_los_freq_b3 <- update(m2_los_freq_b2, . ~ . + PSYPROB)

summary(m2_los_freq_b0)
summary(m2_los_freq_b1)
summary(m2_los_freq_b2)
summary(m2_los_freq_b3)

or_ci_wald(m2_los_freq_b0)
or_ci_wald(m2_los_freq_b1)
or_ci_wald(m2_los_freq_b2)
or_ci_wald(m2_los_freq_b3)





#################################################################
# SENSITIVITY ANALYSES WITH W/O STATES THAT DON'T COLLECT HLTHINS
#################################################################

















