# Table Exercise: Demographic summary table using {tfrmt}

# For this exercise, we will utilize the demographics ARD created in the previous section to create a demographics table with custom formatting.

# Setup begins -----------------------------------------------------------------

## 1. Load necessary packages
library(tfrmt)
library(cards)
library(dplyr)
library(tidyr)
library(tfrmtbuilder)

## 2. Load demog ARD
ard_demog <- readRDS("materials/3-Tables/data/tfrmt_ard_demog.rds")

## 3. Perform manipulations to prep for display
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
# Setup ends -------------------------------------------------------------------


# Exercise:

## A. Complete the tfrmt from the demo

# a) Put mean/sd on the same line, each rounded to 2 decimal places:
#    1. modify the `frmt_structure` in the `body_plan`
#
# b) Put median/q1/q3  on the same line, each rounded to 1 decimal place:
#    1. modify the `label` value in the data so it is the same for each
#    2. modify the `frmt_structure` in the `body_plan`
#
# c) Add a footnote corresponding to the "Placebo" column

# data manipulation step
ard_demog2 <- ard_demog |>
  mutate(label = case_when(
    stat_name %in% c("mean","sd") ~ "Mean (SD)", # stat_name "mean","sd" to have the same label

    # b1) stat_name "median", "p25", and "p75" to have the same label "Median (Q1, Q3)"
    #   < add code here > ,

    TRUE ~ label # otherwise keep as-is!
  ))

# create tfrmt
tfrmt_demog <- tfrmt(
  group = variable,
  label = label,
  column = TRT01A,
  param = stat_name,
  value = stat,
  sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine(expression = "{n} ({p}%)",
                   n = frmt("xx"),
                   p = frmt_when(
                     "<0.01" ~ "<1",
                     TRUE ~ frmt("xx", transform = ~.*100)))
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine(
        expression = "", # a1) complete expression to display as `mean (sd)`
        mean = frmt(""), # a1) complete `frmt` so values are rounded to 2 dec places
        sd = frmt("")    # a1) complete `frmt` so values are rounded to 2 dec places
      )
    ),
    frmt_structure(
      group_val = ".default", label_val = ".default",
      frmt_combine(
        expression = "",   # a2) complete expression to display as `median (p25, p75)`
        median = frmt(""), # a2) complete `frmt` so values are rounded to 1 dec place
        p25 = frmt(""),    # a2) complete `frmt` so values are rounded to 1 dec place
        p75 = frmt("")     # a2) complete `frmt` so values are rounded to 1 dec place
      )
    )
  ) ,
  big_n = big_n_structure(param_val = "bigN"),
  col_plan = col_plan(
    - context,
    - starts_with("ord")
  ),
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = "  "))
  ),
  footnote_plan = footnote_plan(
    footnote_structure("Footnote text", column_val = ) # c) complete the column val
  )
)

# view table with data
print_to_gt(tfrmt_demog, ard_demog2)


## B. Save the tfrmt as JSON (hint: use `tfrmt_to_json`)
# export both the ARD and JSON files to your desktop
tfrmt_to_json(tfrmt_demog, "materials/3-Tables/export/tfrmt_demog.json")
saveRDS(ard_demog2, "materials/3-Tables/export/ard_demog2.rds")


## C. Use the tfrmtbuilder app to:
#     - Toggle *off* mock mode
#     - Upload the ARD and JSON files from your desktop
#     - On the `Titles` tab, add a title to the table
#     - On the `Column Plan` tab, rearrange the columns as High Dose, Low Dose, Placebo, Overall
#     - On the `Page Plan` tab, split the table after group_val="ETHNIC" so it spans 2 pages
#              (hint: add a new Page Structure)
tfrmtbuilder()

