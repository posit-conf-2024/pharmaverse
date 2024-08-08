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
# 6. Remove the "n (%)" footnote, since it's pretty clear what the statistics are

ard <-
  adsl |>
  # ensure the age groups print in the correct order
  mutate(AGEGR1 = factor(AGEGR1, levels = c("18-64", ">64"))) |>
  labelled::set_variable_labels(AGEGR1 = "Age Group") |>
  ard_stack(
    .by = TRT01A,
    ard_continuous(variables = c(AGE, BMI, HEIGHT, WEIGHT), fmt_fn = BMI ~ list(sd = 2L)),
    ard_categorical(variables = c(AGEGR1, SEX, RACE, ETHNIC)),
    .missing = TRUE,
    .attributes = TRUE
  )

tbl <-
  ard |>
  tbl_ard_summary(
    by = TRT01A,
    include = c(AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT), # this orders the variables in the table
    type = all_continuous() ~ "continuous2", # all continuous variables should be summarized as multi-row
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"), # change the statistics for all continuous variables
  ) |>
  # add a header above the 'Xanomeline' treatments. We used `show_header_names()` to know the column names
  modify_spanning_header(c(stat_2, stat_3) ~ "**Active Treatment**") |>
  modify_footnote(all_stat_cols() ~ NA) # removing the footnote, as it's pretty obvious
tbl
