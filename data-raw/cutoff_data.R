# data-raw/cutoff_data.R
# code to prepare `cutoff_data` dataset goes here

library(dplyr)
library(readxl)
library(purrr)

input <- "//vetinst.no/dfs-felles/StasjonK/FAG/Provedata/Rapportering/NormVetUtvikling/Input_data/"
path <- paste0(input, "Cutoff_med_class.xlsx")

cutoffs <- path %>%
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

cutoff_data <- cutoffs %>%
  filter(analyttkode_sens == "080115") %>%
  mutate(analyttkode_sens = "08011501") %>%
  bind_rows(cutoffs)

usethis::use_data(cutoff_data, overwrite = TRUE)
