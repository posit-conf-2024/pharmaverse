install.packages("devtools")
devtools::install_version("gt", version = "0.10.1")
pak::pak("insightsengineering/cards")
pak::pak("ddsjoberg/gtsummary")
install.packages(c("tidyverse",
                   "pharmaversesdtm",
                   "pharmaverseadam",
                   "admiral",
                   "xportr",
                   "cardx",
                   "tfrmt",
                   "tfrmtbuilder",
                   "labelled"))