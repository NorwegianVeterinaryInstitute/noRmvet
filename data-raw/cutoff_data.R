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
  mutate(cut_off = case_when(
    cut_off == "1.6E-2" ~ "0.016",
    cut_off == "1.4999999999999999E-2" ~ "0.016",
    cut_off == "0.064" ~ "0.06",
    cut_off == "0.12" ~ "0.125",
    cut_off == "0.015" ~ "0.016",
    TRUE ~ cut_off
  ))

cutoff_data <- cutoffs %>%
  filter(analyttkode_sens == "080115") %>%
  mutate(analyttkode_sens = "08011501") %>%
  bind_rows(cutoffs)

usethis::use_data(cutoff_data, overwrite = TRUE)
