library(cards)
library(tfrmt)

# demog data
adsl <- pharmaverseadam::adsl |>
  dplyr::filter(SAFFL == "Y") |>
  dplyr::left_join(
    pharmaverseadam::advs |>
      dplyr::filter(PARAMCD %in% c("BMI", "HEIGHT", "WEIGHT"), !is.na(AVAL)) |>
      dplyr::group_by(USUBJID, PARAMCD) |>
      dplyr::arrange(ADY) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(USUBJID, PARAMCD, AVAL) |>
      tidyr::pivot_wider(names_from = PARAMCD, values_from = AVAL),
    by = "USUBJID"
  )
myard <- ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous( variables = c(AGE, BMI, HEIGHT, WEIGHT),
                  statistic = ~ continuous_summary_fns(c("mean", "sd", "median", "p25", "p75"))),
  ard_categorical(variables = c(AGEGR1, RACE, ETHNIC)),
  ard_dichotomous(variables = SEX, value = ~ "F"),
  .overall = TRUE,
  .total_n = TRUE
)

saveRDS(myard, "materials/3-Tables/data/tfrmt_ard_demog.rds")


# AE data

adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")
adae <- pharmaverseadam::adae |> dplyr::mutate(any_ae = TRUE)

adae_subj1 <- adae |>
  dplyr::select(USUBJID, TRT01A, AESEV, any_ae) |>
  unique()
ard_any <-  ard_dichotomous(
  data = adae_subj1,
  by = c(TRT01A, AESEV),
  variables = any_ae,
  denominator = adsl
)
adae_subj2 <- adae |>
  dplyr::select(USUBJID, TRT01A, AESEV, AESOC) |>
  unique()
ard_soc <- ard_hierarchical(
  data = adae_subj2,
  by = c(TRT01A, AESEV),
  variables = AESOC,
  denominator = adsl
)

adae_subj3 <- adae |>
  dplyr::select(USUBJID, TRT01A, AESEV, AESOC, AETERM) |>
  unique()
ard_pt <- ard_hierarchical(
  data = adae_subj3,
  by = c(TRT01A, AESEV),
  variables = c(AESOC, AETERM),
  denominator = adsl
)

ard_bigN <- ard_categorical(
  data = adsl,
  variables = TRT01A,
  statistic = ~ "n"
)

bind_ard(
  ard_bigN,
  ard_any,
  ard_soc,
  ard_pt,
  .order = TRUE
)|>
  saveRDS("materials/3-Tables/data/tfrmt_ard_ae.rds")

# mock table
ard_ae <- readRDS("materials/3-Tables/data/tfrmt_ard_ae.rds")

ard_ae <- ard_ae |>
  dplyr::mutate(group3 = ifelse(variable=="any_ae", "AESOC", group3),
         group3_level = ifelse(variable=="any_ae", "ANY ADVERSE EVENT", group3_level))|>
  shuffle_ard() |>
  dplyr::mutate(label = ifelse(variable=="any_ae", "ANY ADVERSE EVENT", label),
         stat_name = ifelse(variable=="TRT01A" & stat_name=="n", "bigN", stat_name),
         stat = ifelse(stat_name=="p", stat*100, stat)) |>
  dplyr::filter(!(variable!="TRT01A" & stat_name=="N")) |>
  dplyr::mutate(ord1 = ifelse(variable=="any_ae", 1, ifelse(variable=="AESOC", 2, 3)),
         ord2 = as.numeric(as.factor(AESOC)),
         ord3 = as.numeric(as.factor(label)))

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
  print_mock_gt(ard_ae) |>
  gt::gtsave("materials/3-Tables/data/tfrmt_mock_ae.html")



# mock data for VS


vs <- pharmaverseadam::advs |>
  dplyr::filter(AVISIT %in% c("Baseline", "Week 2", "Week 4", "Week 6"),
                PARAMCD %in% c("DIABP", "SYSBP", "PULSE", "TEMP")) |>
  dplyr::arrange(AVISITN) |>
  dplyr::select(TRTA, AVISIT, PARAM) |>
  unique()
vs |>
  tidyr::crossing(
    dplyr::tibble(stat_name=c("mean","sd"))
  ) |>
  dplyr::bind_rows(
    dplyr::tibble(TRTA = unique(vs$TRTA),
                  stat_name = "N")
  ) |>
  dplyr::group_by(AVISIT, PARAM) |>
  dplyr::mutate(ord = dplyr::cur_group_id()) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(TRTA), dplyr::desc(AVISIT), PARAM)|>
  saveRDS("materials/3-Tables/data/tfrmt_mock_vs.rds")



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
  print_mock_gt(mock_data) |>
  gt::gtsave("materials/3-Tables/data/tfrmt_mock_vs.html")

