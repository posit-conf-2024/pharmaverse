library(pharmaverseadam)

adsl_keepers <- c(
  "USUBJID",
  "AGE",
  "TRT01A",
  "TRTSDT",
  "TRTEDT"
)
adsl <- adsl[, adsl_keepers]

for (col in colnames(adsl)) {
  attr(adsl[[col]], "label") <- NULL
}

adae_keepers <- c(
  "USUBJID",
  "AETERM",
  "AESEV",
  "AESTDY",
  "AEENDY"
)
adae <- adae[, adae_keepers]

for (col in colnames(adae)) {
  attr(adae[[col]], "label") <- NULL
}


saveRDS(adsl, "materials/1-admiral-xportr/data/adsl.rds")
saveRDS(adae, "materials/1-admiral-xportr/data/adae.rds")
