library(dplyr)
library(tidyr)
library(ipumsr)
library(ggplot2)
library(dagitty)

# Load main data
setwd("~/SASS_598/MEPSproj/data/main")
ddi <- read_ipums_ddi("meps_00001.xml")
data <- read_ipums_micro(ddi)

# Load supplemental data
setwd("~/SASS_598/MEPSproj/data/supplemental")
supp_ddi <- read_ipums_ddi("meps_00001.xml")
supp_data <- read_ipums_micro(supp_ddi)

# Back to working directory
setwd("~/SASS_598/MEPSproj/eda_rmarkdown")

# Join main data with supplemental data
data <- data %>% left_join(
  supp_data %>% select(MEPSID, SDOHELIG, FSNOTLAST12M, DSCRMDR),
  by = join_by(MEPSID))

# Prepare to relabel data
# RACEA variable
race_dict <- setNames(c("White", "Black/African-American", "Aleut, Alaskan Native, or American Indian", "Alaskan Native or American Indian", "Alaskan Native/Eskimo", "Aleut", "American Indian", "American Indian or Alaskan Native and any other group", "Asian or Pacific Islander", "Asian", "Chinese", "Filipino", "Korean", "Vietnamese", "Japanese", "Asian Indian", "Pacific Islander", "Hawaiian", "Samoan", "Guamanian", "Other Asian or Pacific Islander", "Other Asian or Pacific Islander (1992-1995)", "Other Asian or Pacific Islander (1996)", 
                        "Other Asian or Pacific Islander (1997-1998)", "Other Asian (1999 forward)", "Other Race", "Other Race (1963-1977)", "Other Race (1978)", "Other Race (1979-1991)", "Other Race (1992-1995)", "Other Race (1996)", "Other Race (1997-1998)", "Other Race (1999-2002)", "Primary Race not releasable", "Multiple Race, No Primary Race Selected", "Multiple Race, including Asian, excluding Black and White", "Multiple Race, including Asian and Black, excluding White", "Multiple Race, including Asian and White, excluding Black", 
                        "Multiple Race, including Black, excluding Asian and White", "Multiple Race, including Black and White, excluding Asian", "Multiple Race, including White, excluding Asian and Black", "Multiple Race, including Asian, White, and Black", "Multiple Race, excluding Asian, White, and Black", NA, NA, NA, NA),
                      c(100, 200, 300, 310, 320, 330, 340, 350, 400, 410, 411, 412, 413, 414, 415, 416, 420, 421, 422, 423, 430, 431, 432, 433, 434, 500, 510, 520, 530, 540, 550, 560, 570, 580, 600, 610, 611, 612, 613, 614, 615, 616, 617, 900, 970, 980, 990))
race_formula_list <- paste0(names(race_dict), "~", "\"", race_dict, "\"") |> lapply(as.formula)

# INSULIN, DIAPILLS, DIACLASS, DIALRNPCP, DIALRNNPC, DIALRNPHHP, RIALRNINT, DCSDIABDX, TRANSBAR variables
# survey_answers <- setNames(c("NIU", "No", "Yes", "Unknown-refused", "Unknown-not ascertained", "Unknown-don't know"), c(0, 1, 2, 7, 8, 9))
survey_answers <- setNames(c("NA", "No", "Yes", "Unknown", "Unknown", "Unknown"), c(0, 1, 2, 7, 8, 9))
survey_formula_list <- paste0(names(survey_answers), "~", "\"", survey_answers, "\"") |> lapply(as.formula)

# DCSDIABDX variable
# has_diabetes_answers <- setNames(c("NA", "No", "Yes", "Unknown", "Unknown", "Unknown"), c(0, 1, 2, 7, 8, 9))
# has_diabetes_formula_list <- paste0(names(has_diabetes_answers), "~", "\"", has_diabetes_answers, "\"") |> lapply(as.formula)

# DIETFOLLOW variable
diet_answers <- setNames(c("NA", "No", "Yes", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown"), c(0, 10, 20, 90, 96, 97, 98, 99))
diet_formula_list <- paste0(names(diet_answers), "~", "\"", diet_answers, "\"") |> lapply(as.formula)

# DIACONF variable
confidence_answers <- setNames(c("NA", "Not confident at all", "Somewhat confident", "Confident", "Very confident", "Unknown", "Unknown", "Unknown"), c(0, 1, 2, 3, 4, 7, 8, 9))
confidence_formula_list <- paste0(names(confidence_answers), "~", "\"", confidence_answers, "\"") |> lapply(as.formula)

# RATEMEDCARE, RATEHLTHFD variables
sdoh_answers <- setNames(c("NA", "Excellent", "Excellent or very good", "Very good", "Very good or good", "Good", "Good or fair", "Fair", "Fair or poor", "Poor", "Unknown"), c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 98))
sdoh_formula_list <- paste0(names(sdoh_answers), "~", "\"", sdoh_answers, "\"") |> lapply(as.formula)

# HARDPAYBASIC variable
sdoh_hardpay_answers <- setNames(c("NA", "Very hard", "Very or somewhat hard", "Somewhat hard", "Somewhat or not at all hard", "Not at all hard", "Unknown"), c(0, 1, 2, 3, 4, 5, 8))
sdoh_hardpay_formula_list <- paste0(names(sdoh_hardpay_answers), "~", "\"", sdoh_hardpay_answers, "\"") |> lapply(as.formula)

# Relabel data
data <- data %>%
  mutate(INSULIN_text = case_match(INSULIN, !!!survey_formula_list), # Recode from numbers to answers
         DIAPILLS_text = case_match(DIAPILLS, !!!survey_formula_list),
         DIACLASS_text = case_match(DIACLASS, !!!survey_formula_list),
         DIALRNPCP_text = case_match(DIALRNPCP, !!!survey_formula_list),
         DIALRNNPC_text = case_match(DIALRNNPC, !!!survey_formula_list),
         DIALRNPHHP_text = case_match(DIALRNPHHP, !!!survey_formula_list),
         DIALRNINT_text = case_match(DIALRNINT, !!!survey_formula_list),
         DCSDIABDX_text = case_match(DCSDIABDX, !!!survey_formula_list),
         DIETFOLLOW_text = case_match(DIETFOLLOW, !!!diet_formula_list),
         DIACONF_text = case_match(DIACONF, !!!confidence_formula_list),
         TRANSBAR_text = case_match(TRANSBAR, !!!survey_formula_list),
         RATEMEDCARE_text = case_match(RATEMEDCARE, !!!sdoh_formula_list),
         RATEHLTHFD_text = case_match(RATEHLTHFD, !!!sdoh_formula_list),
         HARDPAYBASIC_text = case_match(HARDPAYBASIC, !!!sdoh_hardpay_formula_list),
         race_str = case_match(RACEA, !!!race_formula_list),
         AGE = replace(AGE, AGE == 996, NA),
         INCWELFR = replace(INCWELFR, INCWELFR %in% c(999999.96, 999999.97, 999999.98, 999999.99), NA)) %>%
  mutate(INSULIN_text = na_if(INSULIN_text, "NA"), # Change string NA to NA type
         DIAPILLS_text = na_if(DIAPILLS_text, "NA"),
         DIACLASS_text = na_if(DIACLASS_text, "NA"),
         DIALRNPCP_text = na_if(DIALRNPCP_text, "NA"),
         DIALRNNPC_text = na_if(DIALRNNPC_text, "NA"),
         DIALRNPHHP_text = na_if(DIALRNPHHP_text, "NA"),
         DIALRNINT_text = na_if(DIALRNINT_text, "NA"),
         DIACONF_text = na_if(DIACONF_text, "NA"),
         DIETFOLLOW_text = na_if(DIETFOLLOW_text, "NA"),
         TRANSBAR_text = na_if(TRANSBAR_text, "NA"),
         RATEMEDCARE_text = na_if(RATEMEDCARE_text, "NA"),
         RATEHLTHFD_text = na_if(RATEHLTHFD_text, "NA"),
         HARDPAYBASIC_text = na_if(HARDPAYBASIC_text, "NA"))

# Split data by year
data_2020 <- data %>% filter(YEAR == 2020)
data_2021 <- data %>% filter(YEAR == 2021)

# Join data across years
full_data <- data_2020 %>%
  inner_join(
    data_2021,
    by = "MEPSID"
  )

# Data where person completed the DCS in 2020 and 2021
diabetes_data <- full_data %>% filter(DCSDIABDX.x == 2 & DCSDIABDX.y == 2)

# Data where person completed the DCS in 2020 and 2021 and completed the SDOH supplement in 2021
diabetes_sdoh_data <- diabetes_data %>% filter(SDOHELIG.y == 2)