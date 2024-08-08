# ARD Exercise: Demographic summaries using {cards}

# For this exercise, compute all the necessary summaries for a demographics table that includes age, age group, sex, race, ethnicity, bmi, height, and weight. We want to compute these summaries within each treatment group (High Dose, Low Dose, Placebo) and overall.

# Setup
## Load necessary packages
library(cards)
library(dplyr)
library(tidyr)

## Import data
adsl <- pharmaverseadam::adsl |>
  filter(SAFFL == "Y") |>
  left_join(
    pharmaverseadam::advs |>
      filter(PARAMCD %in% c("BMI", "HEIGHT", "WEIGHT"), !is.na(AVAL)) |>
      group_by(USUBJID, PARAMCD) |>
      arrange(ADY) |>
      slice(1) |>
      ungroup() |>
      select(USUBJID, PARAMCD, AVAL) |>
      pivot_wider(names_from = PARAMCD, values_from = AVAL),
    by = "USUBJID"
  )

##  A. First, compute the continuous summaries for AGE, BMI, HEIGHT, WEIGHT by TRT01A

ard_continuous(
  data = adsl,
  by = ,
  variables =
)


##  B. Next, compute the categorical summaries for AGEGR1, SEX, RACE, ETHNIC by TRT01A

ard_categorical(
  data = adsl,
  by = ,
  variables =
)

##  C. Since SEX is actually a dichotomous variable, let's switch to `ard_dichotomous` to compute values for just one of its levels ("F")
# (hint: see
# `https://insightsengineering.github.io/cards/latest-tag/articles/getting-started.html#dichotomous-summaries`
#     for an example)

ard_dichotomous(
  data = adsl,
  by = ,
  variables = ,
  value = ~ ""  # complete with the desired value
)

## D. By default, `ard_continuous` computes "N","mean","sd","median","p25","p75","min","max". Suppose we only want to compute a subset of these ("mean", "sd", "median", "p25", "p75"). Modify your code from part A to compute just these summaries.
# (hint: modify the `statistic` argument. see `https://insightsengineering.github.io/cards/latest-tag/reference/summary_functions.html#ref-examples` for an example)

ard_continuous(
  data = adsl,
  by = ,
  variables = ,
  statistic = ~ continuous_summary_fns(  ) # complete
)


## D. Perform all of the summaries in a single ard_stack call, including:
#   - summaries by TRT01A as performed above
#      - continuous summaries from part D for AGE, BMI, HEIGHT, and WEIGHT
#      - categorical summaries from part B for AGEGR1, RACE, ETHNIC
#      - dichotomous summaries from part C for SEX
#   - overall summaries for all of the variables
#   - total N
ard_stack(
  data = adsl,
  .by = ,

  # add ard_* calls here

  .overall = ,
  .total_n =
)
