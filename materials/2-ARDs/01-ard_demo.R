# ARD Demo: Demographic summaries using {cards}

# In this demo, we will some demographic summaries by treatment group (TRT01A)


# A. Set up
## Load necessary packages
library(cards)

## Load ADaM ADSL data
adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")


# B. Compute overall continuous summaries

ard_age_overall <- ard_continuous(
  data = adsl,
  variables = AGE
)

## modifying the statistics

### default
ard_continuous(
  data = adsl,
  variables = AGE,
  statistic = everything() ~ continuous_summary_fns(c("N", "mean", "sd", "median", "p25", "p75", "min", "max"))
)

### subset
ard_continuous(
  data = adsl,
  variables = AGE,
  statistic = ~ continuous_summary_fns(c("mean","median"))
)

### custom
tidy_ttest <- function(x){
  t.test(x) |> broom::tidy()
}
tidy_ttest(adsl$AGE)

ard_continuous(
  data = adsl,
  variables = AGE,
  statistic = ~ list(tidy_ttest = tidy_ttest)
)
ard_continuous(
  data = adsl |> dplyr::mutate(AGE2 = AGE),
  variables = c(AGE, AGE2),
  statistic = list(AGE = list(tidy_ttest = tidy_ttest),
                   AGE2 = continuous_summary_fns())
)

### when things go wrong
error_fun <- function(x){
  stop("an error!")
}

ard_continuous(
  data = adsl |> dplyr::mutate(AGE2 = AGE),
  variables = c(AGE, AGE2),
  statistic = ~continuous_summary_fns(other_stats = list(my_stat = error_fun))
) |>
  print_ard_conditions()


# C. Compute overall categorical summaries

ard_agegr1_overall <- ard_categorical(
  data = adsl,
  variables = AGEGR1
)

## modify the statistics

### default
ard_categorical(
  data = adsl,
  variables = AGE,
  statistic = everything() ~ c("n", "p", "N")
)

### subset (**no custom**)
ard_categorical(
  data = adsl,
  variables = AGE,
  statistic = ~ c("n", "p")
)



# D. Calculate summaries by TRT01A
ard_age <- ard_continuous(
  data = adsl,
  by = TRT01A,
  variables = AGE
)

ard_agegr1 <- ard_categorical(
  data = adsl,
  by = TRT01A,
  variables = AGEGR1
)


# E. Combine results using `bind_ard()`
bind_ard(
  ard_age,
  ard_agegr1,
  ard_age_overall,
  ard_agegr1_overall
)

# F. Use the `ard_stack()` helper to calculate combined results in 1 step

##  Note the summaries for the `by` variable are also included
ard_full1 <- ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous(variables = AGE),
  ard_categorical(variables = AGEGR1)
)

## Add overall calculations (i.e. no `by` variable)
ard_full2 <- ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous(variables = AGE),
  ard_categorical(variables = AGEGR1),
  .overall = TRUE
)
ard_full2 |> dplyr::filter(is.na(group1))


## Add big N (also possible with `ard_total_n`)
ard_full3 <- ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous(variables = AGE),
  ard_categorical(variables = AGEGR1),
  .overall = TRUE,
  .total_n = TRUE
)
ard_full3 |> tail()
