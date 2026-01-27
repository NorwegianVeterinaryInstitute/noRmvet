# data-raw/panel_ranges.R
# code to prepare `panel_ranges` dataset goes here

library(readxl)

input <- "//vetinst.no/dfs-felles/StasjonK/FAG/Provedata/Rapportering/NormVetUtvikling/Input_data/"
path <- paste0(input, "panels_and_ranges.xlsx")

panel_ranges <- read_xlsx(
  path
) %>%
  mutate(
    range_min = as.numeric(range_min),
    range_max = as.numeric(range_max)
  ) %>%
  rename("substans" = substance) %>%
  mutate(report_year = as.character(report_year))

usethis::use_data(panel_ranges, overwrite = TRUE)
