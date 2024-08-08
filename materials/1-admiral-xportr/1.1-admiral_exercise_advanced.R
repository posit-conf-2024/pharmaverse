# Name: ADSL
#
# Label: Subject Level Analysis Dataset
#
# Input: dm, ex, ds
library(admiral)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

data("dm")
data("ds")
data("ex")
data("ae")
data("lb")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint

#### Convert to NAs ####

# User defined functions ----

# Here are some examples of how you can create your own functions that
#  operates on vectors, which can be used in `mutate`.

# Grouping
format_racegr1 <- function(x) {
  case_when(
    x == "WHITE" ~ "White",
    x != "WHITE" ~ "Non-white",
    TRUE ~ "Missing"
  )
}

format_agegr1 <- function(x) {
  case_when(
    # 
  )
}

format_region1 <- function(x) {
  case_when(
    x %in% c("CAN", "USA") ~ "NA",
    !is.na(x) ~ "RoW",
    TRUE ~ "Missing"
  )
}

format_lddthgr1 <- function(x) {
  case_when(
    x <= 30 ~ "<= 30",
    x > 30 ~ "> 30",
    TRUE ~ NA_character_
  )
}

# EOSSTT mapping
format_eosstt <- function(x) {
  case_when(
    x %in% c("COMPLETED") ~ "COMPLETED",
    x %in% c("SCREEN FAILURE") ~ NA_character_,
    !is.na(x) ~ "DISCONTINUED",
    TRUE ~ "ONGOING"
  )
}


#### Derive EXSTDTC and EXENDTC ####
# Derivations ----
# impute start and end time of exposure to first and last respectively, do not impute date






# Source objects ----

# Death cause sources
src_ae <- dthcaus_source(
  dataset_name = "ae",
  filter = AEOUT == "FATAL",
  date = convert_dtc_to_dtm(AESTDTC, highest_imputation = "M"),
  mode = "first",
  dthcaus = AEDECOD,
  set_values_to = exprs(DTHDOM = "AE", DTHSEQ = AESEQ)
)

src_ds <- dthcaus_source(
  # dataset_name = ,
  # filter = ,
  # date = ,
  # mode = ,
  # dthcaus = ,
  # set_values_to = exprs()
)

adsl <- dm %>%
  ## derive treatment variables (TRT01P, TRT01A) ----
# See also the "Visit and Period Variables" vignette
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html#treatment_adsl)
mutate(TRT01P = ARM, TRT01A = ACTARM) %>%
  ## derive treatment start date (TRTSDTM) ----
derive_vars_merged(
  dataset_add = ex_ext,
  filter_add = (EXDOSE > 0 |
                  (EXDOSE == 0 &
                     str_detect(EXTRT, "PLACEBO"))) &
    !is.na(EXSTDTM),
  new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
  order = exprs(EXSTDTM, EXSEQ),
  mode = "first",
  by_vars = exprs(STUDYID, USUBJID)
) %>%
  ## derive treatment end date (TRTEDTM) ----
derive_vars_merged(
  dataset_add = ex_ext,
  filter_add = (EXDOSE > 0 |
                  (EXDOSE == 0 &
                     str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
  new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
  order = exprs(EXENDTM, EXSEQ),
  mode = "last",
  by_vars = exprs(STUDYID, USUBJID)
) %>%
  ## Derive treatment end/start date TRTSDT/TRTEDT ----
derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%
  ## derive treatment duration (TRTDURD) ----
derive_var_trtdurd()

## Disposition dates, status ----
# convert character date to numeric date without imputation
ds_ext <- derive_vars_dt(
  # ,
  # dtc = ,
  # new_vars_prefix = 
)

# Screen fail date
adsl <- adsl %>%
  #### SCRFDT var add ####
  derive_vars_merged(
    
  ) %>%
  #EOSDT var add
  derive_vars_merged(
    # dataset_add = ,
    # by_vars = exprs(),
    # new_vars = exprs(),
    # filter_add = 
  ) %>%
  # EOS status
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = DSCAT == "DISPOSITION EVENT",
    new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)),
    missing_values = exprs(EOSSTT = "ONGOING")
  ) %>%
  # Last retrieval date
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(FRVDT = DSSTDT),
    filter_add = DSCAT == "OTHER EVENT" & DSDECOD == "FINAL RETRIEVAL VISIT"
  ) %>%
  # Derive Randomization Date
  # derive_vars_merged(
  #   dataset_add = ,
  #   filter_add = ,
  #   by_vars = exprs(),
  #   new_vars = exprs()
  # ) %>%
  # Death date - impute partial date to first day/month
  derive_vars_dt(
    # new_vars_prefix = ,
    # dtc = ,
    # highest_imputation = ,
    # date_imputation = 
  ) %>%
  # Relative Day of Death
  # derive_vars_duration(
  #   new_var = ,
  #   start_date = ,
  #   end_date = 
  # ) %>%
  # Elapsed Days from Last Dose to Death
  derive_vars_duration(
    new_var = LDDTHELD,
    start_date = TRTEDT,
    end_date = DTHDT,
    add_one = FALSE
  ) %>%
  # Cause of Death and Traceability Variables
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      #GET FATAL AES
      # event(
      #   dataset_name = ,
      #   condition = ,
      #   set_values_to = exprs(),
      # ),
      event(
        dataset_name = "ds",
        condition = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
        set_values_to = exprs(DTHCAUS = DSTERM, DTHDOM = DOMAIN),
      )
    ),
    source_datasets = list(ae = ae, ds = ds),
    tmp_event_nr_var = event_nr,
    order = exprs(event_nr),
    mode = "first",
    new_vars = exprs(DTHCAUS = DTHCAUS, DTHDOM = DTHDOM)
  ) %>%
  # Death Cause Category
  #### DTHCGR1 var creation ####
  mutate(
  )

## Last known alive date ----
## DTC variables are converted to numeric dates imputing missing day and month
## to the first

adsl <- adsl %>%
  #### extreme event LSTALVDT var creation ####
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "ae",
        # order = ,
        # condition = ,
        # set_values_to = ,
        #   seq = ,
      ),
      event(
        dataset_name = "ae",
        # order = ,
        # condition = ,
        # set_values_to = ,
        #   seq = ,
      ),
      event(
        dataset_name = "lb",
        # order = ,
        # condition =,
        # set_values_to = ,
        #   seq = ,
      ),
      event(
        dataset_name = "adsl",
        # condition = ,
        # set_values_to = ,
      )
    ),
    source_datasets = list(ae = ae, lb = lb, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTALVDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTALVDT)
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
  ) %>%
  ## Groupings and others variables ----
####use functions to create groupings/other vars ie. DTH30FL, DTHA30FL, DTHB30FL, DOMAIN ####
mutate(
  RACEGR1 = format_racegr1(RACE),
  # AGEGR1 = ,
)

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiral_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adsl, file = file.path(dir, "adsl.rda"), compress = "bzip2")
