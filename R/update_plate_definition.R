#' Update plate definitions
#'
#' This function updates the "plate_definitions" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
#'
#' @param server Name of the server to connect to
#' @param database Name of the database to fetch data from
#' @param user Username for the database connection
#' @param update Logical, whether or not the data will be sent to the database. Check differences within the data before updating the database by using `update = FALSE`
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @import tidyr
#' @importFrom DBI dbWriteTable
#' @importFrom getPass getPass
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom stringr str_squish
#'
update_plate_definition <- function(server,
                                    database,
                                    user,
                                    update = FALSE) {
  # Fetch password
  pw <- getPass()

  # Connect to database
  con <- dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = server,
    Database = database,
    UID = user,
    PWD = pw
  )

  old_table <- as_tibble(tbl(con, "plate_definitions"))

  table_list = c("prove",
                 "delprove",
                 "undersokelse",
                 "resultat",
                 "sens_undersokelse_korrigert",
                 "sens_resultat",
                 "art_gruppe",
                 "materiale_gruppe",
                 "bakterie_gruppe",
                 "bakterie_kategori",
                 "analytt_sens_group",
                 "analytt_sens_cutoff",
                 "report_sampling_year")

  # Extract initial table
  init_table <- as_tibble(tbl(con, "innsendelse")) %>%
    mutate_all(~str_squish(as.character(.))) %>%
    mutate(aar = as.character(aar))

  tables <- lapply(table_list, function(x) as_tibble(tbl(con, x)) %>%
                     mutate_all(~str_squish(.)))

  names(tables) <- table_list

  tables$sens_resultat <- rename(tables$sens_resultat,
                                 "analyttkode_sens" = analyttkode) %>%
    select(-kjennelsekode)

  # Merge tables
  RESULT_plate_def <- init_table %>%
    left_join(tables$prove) %>%
    left_join(tables$delprove) %>%
    left_join(tables$undersokelse) %>%
    left_join(tables$resultat) %>%
    left_join(tables$sens_undersokelse_korrigert) %>%
    left_join(tables$sens_resultat) %>%
    left_join(tables$art_gruppe) %>%
    left_join(tables$materiale_gruppe) %>%
    left_join(tables$bakterie_gruppe) %>%
    left_join(tables$bakterie_kategori) %>%
    left_join(tables$analytt_sens_group) %>%
    left_join(tables$analytt_sens_cutoff) %>%
    left_join(tables$report_sampling_year) %>%
    mutate(
      plate_def = case_when(
        # 2001 - 2013
        bakterie_kategori == "Indikator" &
          report_year %in% c(
            "2001",
            "2002",
            "2003",
            "2004",
            "2005",
            "2006",
            "2007",
            "2008",
            "2009",
            "2010",
            "2011",
            "2012",
            "2013"
          ) &
          bakterie_gruppe %in% c(
            "Escherichia coli",
            "Enterococcus faecium",
            "Enterococcus faecalis",
            "CNS",
            "Staphylococcus aureus",
            "Staphylococcus pseudintermedius"
          ) &
          metodekode_sens == "020075" ~ "plate1",
        # 2014
        bakterie_kategori == "Indikator" &
          report_year == "2014" &
          bakterie_gruppe %in% c(
            "Escherichia coli",
            "Enterococcus faecium",
            "Enterococcus faecalis"
          ) &
          metodekode_sens == "020075" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2014" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020185" ~ "plate2",
        # 2015
        bakterie_kategori == "Indikator" &
          report_year == "2015" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens %in% c("020075","020148") ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2015" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020185" ~ "plate2",
        # 2016
        bakterie_kategori == "Indikator" &
          report_year == "2016" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens %in% c("020168","020148") ~ "plate1",
        # 2017
        bakterie_kategori == "Indikator" &
          report_year == "2017" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens %in% c("020168","020148") ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2017" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020185" ~ "plate2",
        # 2018
        bakterie_kategori == "Indikator" &
          report_year == "2018" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020148" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2018" &
          bakterie_gruppe %in% c(
            "Enterococcus faecium",
            "Enterococcus faecalis"
          ) &
          metodekode_sens == "020148" &
          sens_undersokelsesnummer == "1" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2018" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020185" ~ "plate2",
        # 2019
        bakterie_kategori == "Indikator" &
          report_year == "2019" &
          bakterie_gruppe %in% c(
            "Escherichia coli",
            "Enterococcus faecium",
            "Enterococcus faecalis",
            "MRSA",
            "MRSP",
            "Staphylococcus pseudintermedius"
          ) &
          metodekode_sens == "020148" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2019" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020185" ~ "plate2",
        # 2020
        bakterie_kategori == "Indikator" &
          report_year == "2020" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020148" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2020" &
          bakterie_gruppe %in% c(
            "Enterococcus faecium",
            "Enterococcus faecalis"
          ) &
          metodekode_sens == "020148" &
          sens_undersokelsesnummer == "1" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2020" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020185" ~ "plate2",
        # 2021
        bakterie_kategori == "Indikator" &
          report_year == "2021" &
          bakterie_gruppe %in% c(
            "Escherichia coli",
            "Enterococcus faecium",
            "Enterococcus faecalis",
            "MRSA"
          ) &
          metodekode_sens == "020148" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2021" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020185" ~ "plate2",
        # 2022
        bakterie_kategori == "Indikator" &
          report_year == "2022" &
          bakterie_gruppe %in% c(
            "Escherichia coli",
            "Staphylococcus aureus",
            "Staphylococcus felis",
            "Staphylococcus pseudintermedius",
            "CNS"
          ) &
          metodekode_sens == "020148" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2022" &
          bakterie_gruppe %in% c(
            "Enterococcus faecium",
            "Enterococcus faecalis"
          ) &
          metodekode_sens == "020148" &
          sens_undersokelsesnummer == "1" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2022" &
          bakterie_gruppe == "Escherichia coli" &
          metodekode_sens == "020185" ~ "plate2",
        # 2023
        bakterie_kategori == "Indikator" &
          report_year == "2023" &
          bakterie_gruppe %in% c(
            "Escherichia coli",
            "Staphylococcus aureus",
            "Staphylococcus pseudintermedius",
            "CNS",
            "MRSP",
            "Karba"
          ) &
          metodekode_sens == "020148" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2023" &
          bakterie_gruppe %in% c(
            "Enterococcus faecium",
            "Enterococcus faecalis"
          ) &
          metodekode_sens %in% c("020148","020168") &
          sens_undersokelsesnummer == "1" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2023" &
          bakterie_gruppe %in% c("Escherichia coli", "Karba") &
          metodekode_sens == "020185" ~ "plate2",
        # 2024
        bakterie_kategori == "Indikator" &
          report_year == "2024" &
          bakterie_gruppe %in% c(
            "Escherichia coli",
            "Staphylococcus aureus",
            "Staphylococcus pseudintermedius",
            "MRSA",
            "Karba",
            "Enterococcus faecium",
            "Enterococcus faecalis"
          ) &
          metodekode_sens == "020148" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2024" &
          bakterie_gruppe %in% c("Escherichia coli", "Karba") &
          metodekode_sens == "020185" ~ "plate2",
        # 2025
        bakterie_kategori == "Indikator" &
          report_year == "2025" &
          bakterie_gruppe %in% c(
            "Escherichia coli",
            "Staphylococcus aureus",
            "Staphylococcus felis",
            "MRSA",
            "Karba",
            "Enterococcus faecium",
            "Enterococcus faecalis"
          ) &
          metodekode_sens == "020148" ~ "plate1",
        bakterie_kategori == "Indikator" &
          report_year == "2025" &
          bakterie_gruppe %in% c("Escherichia coli", "Karba") &
          metodekode_sens == "020185" ~ "plate2",
        TRUE ~ NA_character_
      )
    ) %>%
    select(
      report_year,
      bakterie_kategori,
      bakterie_gruppe,
      metodekode_sens,
      sens_undersokelsesnummer,
      plate_def
    ) %>%
    filter(!is.na(plate_def)) %>%
    distinct()


  comp <- old_table %>%
    left_join(
      RESULT_plate_def,
      by = c(
        "report_year",
        "bakterie_kategori",
        "bakterie_gruppe",
        "metodekode_sens",
        "sens_undersokelsesnummer"
      )
    ) %>%
    filter(plate_def.x != plate_def.y)


  if (update == FALSE) {
    if (nrow(comp) == 0 & nrow(old_table) == nrow(RESULT_plate_def)) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = RESULT_plate_def,
        "diff" = comp
      )
    }
  } else {
    print("Updating table in database.")
    DBI::dbWriteTable(conn = con,
                      name = "plate_definitions",
                      RESULT_plate_def,
                      overwrite = TRUE)
  }


}
