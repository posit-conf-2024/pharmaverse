# Name: ADAE
#
# Label: Adverse Event Analysis Dataset
#
# Input: ae, adsl, ex_single
library(admiral)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

data("ae")
data("admiral_adsl")
data("ex_single")
data("suppae")

adsl <- admiral_adsl

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint


###convert_blanks_to_na()###
# ae <- 
# ex <- 


# Derivations ----

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(TRTSDT, TRTEDT, DTHDT, EOSDT)

adae <- ae %>%
  # join adsl to ae
  derive_vars_merged(
    # dataset_add = ,
    # new_vars = ,
    # by = 
  ) %>%
  ## Derive analysis start time ---- AESTDTC
derive_vars_dtm(
  # dtc = ,
  # new_vars_prefix = ,
  # highest_imputation = ,
  # min_dates = 
) %>%
  ## Derive analysis end time ---- AEENDTC
derive_vars_dtm(
  # dtc = ,
  # new_vars_prefix = ,
  # highest_imputation = ,
  # date_imputation = ,
  # time_imputation = ,
  # max_dates = 
) %>%
  ## Derive analysis end/start date ----
derive_vars_dtm_to_dt() %>%
  ## Derive analysis start relative day and  analysis end relative day ---- ref date = TRTSDT
derive_vars_dy(
  # reference_date = ,
  # source_vars = 
) %>%
  ## Derive analysis duration (value and unit) ---- ADURN
derive_vars_duration(
  # new_var = ,
  # new_var_unit = ,
  # start_date = ,
  # end_date = ,
  # in_unit = ,
  # out_unit = ,
  # add_one = ,
  # trunc_out = 
)

ex_ext <- derive_vars_dtm(
  # ex,
  # dtc = ,
  # new_vars_prefix = ,
  # flag_imputation = "none"
)

adae <- adae %>%
  ## Derive last dose date/time ----
derive_vars_joined(
  # dataset_add = ex_ext,
  # by_vars = ,
  # new_vars = ,
  # join_vars = ,
  # join_type = ,
  # order = ,
  # filter_add = ,
  # filter_join = ,
  # mode = "last"
) %>%
  ## Derive severity / causality / ... ----
mutate(
  ASEV = AESEV,
  AREL = AEREL
) %>%
  ## Derive treatment emergent flag ---- use TRTSDT and TRTEDT
derive_var_trtemfl(
  # trt_start_date = ,
  # trt_end_date = ,
  # end_window = 
) %>%
  ## Derive occurrence flags: first occurrence of most severe AE ----
# create numeric value ASEVN for severity
mutate(
  ASEVN = as.integer(factor(ASEV, levels = c("MILD", "MODERATE", "SEVERE", "DEATH THREATENING")))
) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID),
      order = exprs(desc(ASEVN), ASTDTM, AESEQ),
      new_var = AOCCIFL,
      mode = "first"
    ),
    filter = TRTEMFL == "Y"
  )

# Join all ADSL with AE
adae <- adae %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiral_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adae, file = file.path(dir, "adae.rda"), compress = "bzip2")
