# data-raw/genes_data.R
# code to prepare `genes_data` dataset goes here

library(dplyr)
library(readxl)

input <- "//vetinst.no/dfs-felles/StasjonK/FAG/Provedata/Rapportering/NormVetUtvikling/Input_data/"
path <- paste0(input, "gene_codes.xlsx")

gene_data <- read_xlsx(
  path
)

usethis::use_data(gene_data, overwrite = TRUE)
