# data-raw/am_groups.R
# code to prepare `am_groups` dataset goes here

library(getPass)
library(odbc)
library(dplyr)
library(stringr)

pw <- getPass()

# Connect to database
con <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "sqlpjstest01",
  Database = "normvet",
  UID = "normvet",
  PWD = pw
)

# Get group data for substances
am_groups <- as_tibble(tbl(con, "analytt_sens_group")) %>%
  mutate_all(~str_squish(.)) %>%
  filter(!is.na(substans))

usethis::use_data(am_groups, overwrite = TRUE)
