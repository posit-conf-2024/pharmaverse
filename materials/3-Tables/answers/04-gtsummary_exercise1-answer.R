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
# 7. Remove the "n (%)" footnote, since it's pretty clear what the statistics are


tbl <-
  adsl |>
  # ensure the age groups print in the correct order
  mutate(AGEGR1 = factor(AGEGR1, levels = c("18-64", ">64"))) |>
  tbl_summary(
    by = TRT01A,
    include = -USUBJID, # no need to include ID in the table
    type = all_continuous() ~ "continuous2", # all continuous variables should be summarized as multi-row
    statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"), # change the statistics for all continuous variables
    label = list(AGEGR1 = "Age Group"), # add a label for AGEGR1
    # change the number of decimal places for the SD statistic
    digits = BMI ~ list(sd = 2L)
  ) |>
  # add a header above the 'Xanomeline' treatments. We used `show_header_names()` to know the column names
  modify_spanning_header(c(stat_2, stat_3) ~ "**Active Treatment**") |>
  modify_footnote(all_stat_cols() ~ NA) # removing the footnote, as it's pretty obvious
tbl

# extract the ARD from the table
tbl$cards[[1]] |>
  cards::tidy_ard_row_order() # this puts the results in a nicer order for viewing
