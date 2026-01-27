# data-raw/species_codes.R
# code to prepare `species_codes` dataset goes here

library(readr)

input <- "//vetinst.no/dfs-felles/StasjonK/FAG/Provedata/Rapportering/NormVetUtvikling/Input_data/"
path <- paste0(input, "data_species.txt")

species_codes <- read_delim(
  path,
  delim = "\t",
  col_names = TRUE
)

usethis::use_data(species_codes, overwrite = TRUE)
