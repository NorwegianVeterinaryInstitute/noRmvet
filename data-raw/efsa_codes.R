# data-raw/efsa_codes.R
# code to prepare `efsa_codes` dataset goes here

library(readxl)

input <- "//vetinst.no/dfs-felles/StasjonK/FAG/Provedata/Rapportering/NormVetUtvikling/Input_data/"

sheet_names <- excel_sheets(
  paste0(
    input,
    "/EFSA_codes.xlsx"
  )
)

efsa_codes <- lapply(
  sheet_names,
  function(x) {
    read_xlsx(
      paste0(
        input,
        "/EFSA_codes.xlsx"
      ),
      sheet = x)
  })

names(efsa_codes) <- sheet_names

usethis::use_data(efsa_codes, overwrite = TRUE)
