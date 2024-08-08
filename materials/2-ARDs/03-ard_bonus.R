# ARD Bonus Exercise: Adverse Events summaries using {cards}

# Load necessary packages
library(cards)
library(cardx)

# Import data
adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")
adae <- pharmaverseadam::adae |> dplyr::mutate(any_ae = TRUE)

### BONUS 1 ###

## A. Calculate the number and percentage of *unique* subjects with any AE, by treatment group (TRT01A) and severity (AESEV). (Hint: use `any_ae` for calculations)

adae_subj1 <- adae |>
  dplyr::select(   ) |> # specify variables to keep for the calculations
  unique()

# complete the code below
ard_any <-  ard_dichotomous(
  data = ,
  by = ,
  variables = ,
  denominator =
)


## B. Calculate the number and percentage of *unique* subjects with at least one AE by System organ class (AESOC), within every combination of treatment group (TRT01A) and severity (AESEV)

adae_subj2 <- adae |>
  dplyr::select(   ) |> # specify variables to keep for the calculations
  unique()

# complete the code below
ard_soc <- ard_hierarchical(
  data = ,
  by = ,
  variables = ,
  denominator =
)

## C. Calculate the number and percentage of *unique* subjects with at least one AE by preferred terms (AETERM) nested within AESOC, within every combination of treatment group (TRT01A) and severity (AESEV) (hint: use `ard_hierarchical` to calculate preferred terms nested in system organ class)


adae_subj3 <- adae |>
  dplyr::select( ) |> # specify variables to keep for the calculations
  unique()

# complete the code below
ard_pt <- ard_hierarchical(
  data = ,
  by = ,
  variables = ,
  denominator =
)

## D. Calculate population Ns for each treatment group

# complete the code below
ard_bigN <- ard_categorical(
  data = ,
  variables = ,
  statistic = ~ "n"
)

## E. Combine results into a single cards object, keeping the original order

# complete the code below
bind_ard(

  # add the ard objects from above

  .order =  # complete
)


### BONUS 2 ###

# A. Use {cardx} to test the difference in AGE between High Dose and Placebo groups

# complete the code below
adsl |>
  dplyr::filter(    )|> # filter to just the groups we care about
  ard_stats_t_test(
    by = ,
    variables =
  )

# B. Use {cardx} to test the difference in SEX between treatment groups

adsl |>
  ard_stats_chisq_test(
    by = ,
    variables =

  )
