# Table Exercise: Demographic summary table using {gtsummary}

# Create a Demography table split by treatment

# Setup
## Load necessary packages
library(gtsummary)
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

# 1. Use tbl_summary() to summarize AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT by TRT01A
# 2. For all continuous variables, present the following stats: c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}")
# 3. Ensure the AGEGR1 levels are reported in the correct order
# 4. Report the standard deviation of BMI to 2 decimal place
# 5. View the ARD saved in the gtsummary table: e.g. `tbl$cards`
# BONUS!
# 6. Add the header "**Active Treatment**" over the 'Xanomeline' treatments.

tbl <-
  adsl |>
  mutate(AGEGR1 = ) |>
  tbl_summary(
    by = ,
    include = ,
    type = ,
    statistic = ,
    label = , # add a label for AGEGR1
    digits = # change the number of decimal places for the BMI SD statistic
  ) |>
  # add a header above the 'Xanomeline' treatments. HINT: Use `show_header_names()` to know the column names
  modify_spanning_header()
tbl

# extract the ARD from the table

