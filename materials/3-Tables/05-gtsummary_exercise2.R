# Table Exercise: Demographic summary table using {gtsummary}

# Create a Demography table split by treatment

# Setup
## Load necessary packages
library(gtsummary)
library(cards)
library(tidyverse)

## Import data
adsl <- pharmaverseadam::adsl |>
  filter(SAFFL == "Y") |>
  left_join(
    pharmaverseadam::advs |>
      filter(PARAMCD %in% c("BMI", "HEIGHT", "WEIGHT"), !is.na(AVAL)) |>
      arrange(ADY) |>
      slice(1, .by = c(USUBJID, PARAMCD)) |>
      pivot_wider(id_cols = USUBJID, names_from = PARAMCD, values_from = AVAL),
    by = "USUBJID"
  ) |>
  select(USUBJID, TRT01A, AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT) |>
  labelled::set_variable_labels(
    BMI = "BMI",
    HEIGHT = "Height, cm",
    WEIGHT = "Weight, kg"
  )

# 1. Use cards::ard_stack() then gtsummary::tbl_ard_summary() to summarize AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT by TRT01A
# 2. For all continuous variables, present the following stats: c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}")
# 3. Ensure the AGEGR1 levels are reported in the correct order
# 4. Report the standard deviation of BMI to 2 decimal place
# BONUS!
# 5. Add the header "**Active Treatment**" over the 'Xanomeline' treatments.

ard <-
  adsl |>
  # ensure the age groups print in the correct order
  mutate(AGEGR1 = ) |>
  labelled::set_variable_labels(AGEGR1 = "Age Group") |>
  ard_stack(
    .by = ,


    .missing = TRUE,
    .attributes = TRUE
  )

tbl <-
  ard |>
  tbl_ard_summary(
    by = ,
    include = , # this orders the variables in the table
    type = , # all continuous variables should be summarized as multi-row
    statistic = , # specify the statistics for all continuous variables
  ) |>
  # add a header above the 'Xanomeline' treatments. We used `show_header_names()` to know the column names
  modify_spanning_header()
tbl
