library(xportr)
library(readxl)

# ---- metadata ----
meta_dataset <- read_xlsx("materials/1-admiral-xportr/data/xportr-metadata.xlsx", sheet = "datasets")
meta_variables <- read_xlsx("materials/1-admiral-xportr/data/xportr-metadata.xlsx", sheet = "variables")

# ----- ADSL ----
# pretend we're making adsl with admiral instead of reading it in :)
adsl <- readRDS("materials/1-admiral-xportr/data/adsl.rds")

## ---- step-by-step so we can see intermediate results ----

### ---- attach metadata ----
adsl <- xportr_metadata(adsl, metadata = meta_variables, domain = "ADSL")
attributes(adsl)

### ---- column order ----
# AGE and TRT01A are in the wrong spot
colnames(adsl)
adsl <- xportr_order(adsl)
colnames(adsl)

### ---- variable types ----
adsl <- xportr_type(adsl)

# Q: Why did {xportr} say we have type mismatches?
# hint: xportr_type(adsl, verbose = "message")

### ---- variable labels
# columns don't have labels
lapply(adsl, \(x) attr(x, 'label'))
adsl <- xportr_label(adsl)
lapply(adsl, \(x) attr(x, 'label'))


### ---- variable length ----
# we didn't specify lengths in the metadata
adsl <- xportr_length(adsl)
lapply(adsl, \(x) attr(x, "width"))

### ---- variable format ----
adsl <- xportr_format(adsl)
lapply(adsl, \(x) attr(x, 'format.sas'))

### ------ write the file ----
xportr_write(adsl, path = "materials/1-admiral-xportr/data/adsl.xpt")

## ---- or we can do it all in one go ----
xportr(
  adsl,
  var_metadata = meta_variables,
  df_metadata = meta_dataset,
  domain = "ADSL",
  path = "materials/1-admiral-xportr/data/adsl.xpt"
)

# ---- ADAE ----
# You're turn!
# 1. Update the metadata in xportr/xportr-metadata.xlsx.
# 2. Attach the metadata to ADAE.
# 3. Run the checks.
# 4. Write the result to xportr/adae.xpt.


adae <- readRDS("materials/1-admiral-xportr/data/adae.rds")

## ---- step by step ----
adae <- xportr_metadata(adae, metadata = meta_variables, domain = "ADAE")

adae <- xportr_order(adae)
adae <- xportr_type(adae)
adae <- xportr_length(adae)
adae <- xportr_format(adae)

xportr_write(adae, path = "materials/1-admiral-xportr/data/adae.xpt")

## ---- all at once ----
xportr(
  adae,
  var_metadata = meta_variables,
  df_metadata = meta_dataset,
  domain = "ADAE",
  path = "materials/1-admiral-xportr/data/adae.xpt"
)
