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
  dplyr::select(USUBJID, TRT01A, AESEV, any_ae) |>
  unique()

ard_any <-  ard_dichotomous(
  data = adae_subj1,
  by = c(TRT01A, AESEV),
  variables = any_ae,
  denominator = adsl
)

## B. Calculate the number and percentage of *unique* subjects with at least one AE by System organ class (AESOC), within every combination of treatment group (TRT01A) and severity (AESEV)

adae_subj2 <- adae |>
  dplyr::select(USUBJID, TRT01A, AESEV, AESOC) |>
  unique()

ard_soc <- ard_hierarchical(
  data = adae_subj2,
  by = c(TRT01A, AESEV),
  variables = AESOC,
  denominator = adsl
)

## C. Calculate the number and percentage of *unique* subjects with at least one AE by preferred terms (AETERM) nested within AESOC, within every combination of treatment group (TRT01A) and severity (AESEV) (hint: use `ard_hierarchical` to calculate preferred terms nested in system organ class)

adae_subj3 <- adae |>
  dplyr::select(USUBJID, TRT01A, AESEV, AESOC, AETERM) |>
  unique()

ard_pt <- ard_hierarchical(
  data = adae_subj3,
  by = c(TRT01A, AESEV),
  variables = c(AESOC, AETERM),
  denominator = adsl
)

## D. Calculate population Ns for each treatment group

ard_bigN <- ard_categorical(
  data = adsl,
  variables = TRT01A,
  statistic = ~ "n"
)

## E. Combine results into a single cards object, keeping the original order

bind_ard(
  ard_bigN,
  ard_any,
  ard_soc,
  ard_pt,
  .order = TRUE
)



### BONUS 2 ###

# A. Use {cardx} to test the difference in AGE between High Dose and Placebo groups

adsl |>
  dplyr::filter(TRT01A %in% c("Xanomeline High Dose","Placebo"))|>
  ard_stats_t_test(
    by = TRT01A,
    variables = AGE

  )

# B. Use {cardx} to test the difference in SEX between treatment groups

adsl |>
  ard_stats_chisq_test(
    by = TRT01A,
    variables = SEX

  )
