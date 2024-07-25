# This script is used to provide images of various otuput formats 

library(pharmaverseadam)
library(gtsummary)
library(tidyverse)

tbl <-
  pharmaverseadam::adsl |> 
  filter(!ARM %in% "Screen Failure") |>   
  tbl_summary(
    by = ARM,
    include = c(AGE, RACE),
    type = AGE ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})",
                                     "{median} ({p25} \u2014 {p75})",
                                     "{min} \u2014 {max}"),
    digits = AGE ~ list(sd = 1)
  ) |> 
  modify_spanning_header(
    c(stat_2, stat_3) ~ "**Active Treatment**"
  ) |> 
  modify_footnote(
    all_stat_cols() ~ NA,
    c(stat_2, stat_3) ~ "Capsule BID"
  )
tbl
saveRDS(tbl, file = here::here("materials", "table-overview", "img", "gsummary-tbl.rds"))

# save an HTML gt image
as_gt(tbl) |> 
  gt::gtsave(here::here("materials", "table-overview", "img", "gt-html.png"))

# save a docx flextable
as_flex_table(tbl) |> 
  flextable::save_as_docx(
    path = here::here("materials", "table-overview", "img", "flextable-word.docx"),
    pr_section = 
      officer::prop_section(
        page_size = officer::page_size(
          orient = "landscape",
          width = 8.3, height = 11.7
        ),
        type = "continuous",
        page_margins = officer::page_mar()
      )
  )

# save a PDF huxtable
as_hux_table(tbl) |> 
  huxtable::quick_html()
  huxtable::quick_html(here::here("materials", "table-overview", "img", "huxtable-pdf.pdf"))
