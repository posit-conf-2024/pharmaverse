# Tables Bonus Exercise: AE table and Mock shell using {tfrmt}

# In these exercises, we will continue to practice formatting for additional tables. We will also explore the ability to create a mock.

# Setup begins -----------------------------------------------------------------

## 1. Load necessary packages
library(tfrmt)
library(cards)
library(dplyr)

## 2. Load AE ARD
ard_ae <- readRDS("materials/3-Tables/data/tfrmt_ard_ae.rds")

## 3. Performs manipulations to prep for display
#  - back-fill the Any AE category so it is another level of SOC
#  - Perform shuffle to consolidate columns
#  - adjust the label for ANY AE so matches the SOC value (expectation of all top-level summary rows)
#  - scale percentages
#  - filter to just the needed stats (no SOC/PT level denominators)
#  - add order columns
ard_ae <- ard_ae |>
  mutate(group3 = ifelse(variable=="any_ae", "AESOC", group3),
         group3_level = ifelse(variable=="any_ae", "ANY ADVERSE EVENT", group3_level))|>
  shuffle_ard() |>
  mutate(label = ifelse(variable=="any_ae", "ANY ADVERSE EVENT", label),
         stat_name = ifelse(variable=="TRT01A" & stat_name=="n", "bigN", stat_name),
         stat = ifelse(stat_name=="p", stat*100, stat)) |>
  filter(!(variable!="TRT01A" & stat_name=="N")) |>
  mutate(ord1 = ifelse(variable=="any_ae", 1, ifelse(variable=="AESOC", 2, 3)),
         ord2 = as.numeric(as.factor(AESOC)),
         ord3 = as.numeric(as.factor(label)))
# Setup ends -------------------------------------------------------------------


# Exercises:

## Bonus 1: AE Table
# Create a tfrmt for the AE ARD previously created with the following specs (see mock display `data/tfrmt_mock_ae.html`):
#    - SOC (top-level group) and preferred term (nested group) in the rows
#    - treatment spanning over severity in the columns
#    - big Ns in the treatment spanners
# (hints:
#   - use `tfrmt_n_pct()` template to get a quick-start)

tfrmt_n_pct(n = "n", pct = "p") |>
  tfrmt(group = AESOC,
        label = label,
        param = stat_name,
        value = stat,
        column = c(TRT01A, AESEV),
        sorting_cols = c(ord1, ord2, ord3),
        col_plan = col_plan(
          -context, -variable, -starts_with("ord")
        ),
        big_n = big_n_structure(param_val = "bigN")) |>
  print_to_gt(ard_ae)


## Bonus 2: Mock shell
# Create the code to reproduce the mock shell from `data/tfrmt_mock_vs.html`
#  using the sample ARD `mock_data` below
# (hint: use `print_mock_gt` to print a table with no values)

mock_data <- readRDS("materials/3-Tables/data/tfrmt_mock_vs.rds")

tfrmt(group = AVISIT,
      label = PARAM,
      column = TRTA,
      param = stat_name,
      sorting_cols = ord,
      body_plan = body_plan(
        frmt_structure(group_val = ".default", label_val = ".default",
                       frmt_combine("{mean} ({sd})",
                                    mean = frmt("x.x"),
                                    sd = frmt("x.xx")))
      ),
      big_n = big_n_structure(param_val = "N"),
      col_plan = col_plan(
        "Xanomeline High Dose",
        "Xanomeline Low Dose",
        "Placebo",
        -ord
      )
      ) |>
  print_mock_gt(mock_data)
