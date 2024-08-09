# Tables Demo: Demographic table using {tfrmt}

# Load necessary packages
library(tfrmt)
library(cards)
library(dplyr)
library(tidyr)

## Load demog ARD
ard_demog <- readRDS("materials/3-Tables/data/tfrmt_ard_demog.rds")

# Manipulations to prep for display
#  - Perform shuffle to consolidate columns
#  - Fill in any overall summaries with TRT01A = "Overall"
#  - Manipulate label for dichotomous SEX so it includes the level and appears on one line
#  - Add columns to order on
#  - keep only treatment group big N's (not variable-level Ns)
ard_demog <- ard_demog |>
  shuffle_ard() |>
  replace_na(list(TRT01A = "Overall")) |>
  mutate(stat_name = ifelse(variable=="TRT01A" & stat_name=="n" |
                              variable=="..ard_total_n..", "bigN", stat_name),
         variable=ifelse(variable=="SEX", "SEX (F)", variable),
         label = ifelse(variable=="SEX (F)", variable, label),
         ord1 = as.numeric(factor(variable, levels = c("AGE", "AGEGR1","SEX (F)", "RACE", "ETHNIC","BMI","HEIGHT","WEIGHT"))),
         ord2 = as.numeric(factor(label, levels = c("18-64",">64"))))|>
  filter(stat_name=="bigN" |
           (! variable %in% c("TRT01A","..ard_total_n..") & ! stat_name=="N"))


## Initialize tfrmt
tfrmt_demog <- tfrmt(
  group = variable, ## What is the grouping variable
  label = label, ## What is the row label variable
  column = TRT01A, ## what is the column variable
  param = stat_name, ## what is the param variable
  value = stat, ## what is the value variable,
  sorting_cols = c(ord1, ord2) ## what column(s) to sort on
)

print_to_gt(tfrmt = tfrmt_demog, .data = ard_demog)


## Define a basic body plan
tfrmt_demog <- tfrmt_demog |>
  tfrmt(
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("x.x"))
    )
  )

print_to_gt(tfrmt = tfrmt_demog, .data = ard_demog)



## format the n and % in a cell together as "n (%)"
tfrmt_demog <- tfrmt_demog |>
  tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default", label_val = ".default",
        frmt_combine("{n} ({p}%)",
                     n = frmt("xx"),
                     p = frmt("xx")))
      )
    )

print_to_gt(tfrmt = tfrmt_demog, .data = ard_demog)

## adjust the n (%) formatting so it is:
### - scaled by 100
### - displayed as "<1%" if less than 1%
tfrmt_demog <- tfrmt_demog |>
  tfrmt(
    body_plan = body_plan(
      frmt_structure(
        group_val = ".default", label_val = ".default",
        frmt_combine("{n} ({p}%)",
                     n = frmt("xx"),
                     p = frmt_when(
                       "<0.01" ~ "<1",
                       TRUE ~ frmt("xx", transform = ~.*100)))
    )
  )
  )

print_to_gt(tfrmt = tfrmt_demog, .data = ard_demog)


## apply different rounding for mean/sd and median/q1/q3
tfrmt_demog <- tfrmt_demog |>
  tfrmt(
    body_plan = body_plan(

      # you can do this based on the param name
      frmt_structure(
        group_val = ".default", label_val = ".default", mean = frmt("xx.xx")
      ) ,
      frmt_structure(
        group_val = ".default", label_val = ".default", sd = frmt("xx.xx")
      ) ,

      # or you can do this based on the label value!
      frmt_structure(
        group_val = ".default", label_val = c("Median","Q1","q3"), frmt("xx.x")
      )
    )
  )

print_to_gt(tfrmt = tfrmt_demog, .data = ard_demog)

## big N's
tfrmt_demog <- tfrmt_demog |>
  tfrmt(
    big_n = big_n_structure(param_val = "bigN")
  )

print_to_gt(tfrmt = tfrmt_demog, .data = ard_demog)

## Remove the "context" and order columns
tfrmt_demog <- tfrmt_demog |>
  tfrmt(
    col_plan = col_plan(
      - context,
      - starts_with("ord")
    )
  )

print_to_gt(tfrmt = tfrmt_demog, .data = ard_demog)

## Add an empty row after each grouping variable
tfrmt_demog <- tfrmt_demog |>
  tfrmt(
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block(post_space = "  "))
    )
  )

print_to_gt(tfrmt = tfrmt_demog, .data = ard_demog)




