# data-raw/cutoff_data.R
# code to prepare `cutoff_data` dataset goes here

library(dplyr)
library(readxl)
library(purrr)

input <- "//vetinst.no/dfs-felles/StasjonK/FAG/Provedata/Rapportering/NormVetUtvikling/Input_data/"
path <- paste0(input, "Cutoff_med_class.xlsx")

cutoff_data <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_dfr(read_excel, path = path, col_types = "text", .id = "SheetName") %>%
  left_join(noRmvet::am_groups, relationship = "many-to-many") %>%
  select(-c(Substance, analyttkode_gruppe)) %>%
  rename("cut_off_gruppe" = SheetName,
         "dato" = DATO,
         "kilde" = Kilde,
         "mo" = MO,
         "cut_off" = cutoff) %>%
  mutate(cut_off = ifelse(
    cut_off == "0.06", "0.064", cut_off
  ))

usethis::use_data(cutoff_data, overwrite = TRUE)
