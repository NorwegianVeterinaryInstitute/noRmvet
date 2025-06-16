#' Update plate grouping
#'
#' This function updates the "sens_undersokelse_korrigert" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
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
#' @importFrom purrr reduce
#' @importFrom stringr str_trim
#'
update_plate_grouping <- function(server,
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

  old_table <- as_tibble(tbl(con, "sens_undersokelse_korrigert"))

  table_list <- c("prove",
                  "delprove",
                  "undersokelse",
                  "resultat",
                  "sens_undersokelse",
                  "sens_resultat",
                  "art_gruppe",
                  "materiale_gruppe",
                  "bakteriegruppe",
                  "analytt_sens_cutoff",
                  "analytt_sens_group")

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


  tables$sens_undersokelse <- rename(tables$sens_undersokelse,
                                     "metodekode_sens" = metodekode)


  # Merge tables
  RESULT_plate_info <- init_table %>%
    left_join(tables$prove) %>%
    left_join(tables$delprove) %>%
    left_join(tables$undersokelse) %>%
    left_join(tables$resultat) %>%
    left_join(tables$sens_undersokelse) %>%
    left_join(tables$sens_resultat) %>%
    left_join(tables$art_gruppe) %>%
    left_join(tables$materiale_gruppe) %>%
    left_join(tables$bakteriegruppe) %>%
    left_join(tables$analytt_sens_cutoff) %>%
    left_join(tables$analytt_sens_group) %>%
    mutate(
      saksnr = paste(
        aar,
        ansvarlig_seksjon,
        innsendelsesnummer,
        provenummer,
        delprovenummer,
        undersokelsesnummer,
        resultatnummer,
        sens_undersokelsesnummer,
        sep = "-"
      )
    )

  # plate IDs
  substances_per_plate <- RESULT_plate_info %>%
    filter(substr(analyttkode_sens, 1,2) == "08") %>%
    group_by(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      provenummer,
      delprovenummer,
      undersokelsesnummer,
      resultatnummer,
      sens_undersokelsesnummer
    ) %>%
    summarise(substances = toString(substans))

  recoded_data <- RESULT_plate_info %>%
    filter(substr(analyttkode_sens, 1,2) == "08") %>%
    left_join(substances_per_plate) %>%
    filter(metodekode_sens %in% c("020148","020075"),
           grepl("Cefepim|Temosilin", substances),
           grepl("klavulansyre", substances)) %>%
    mutate(metodekode_sens_new = "020185") %>%
    select(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      provenummer,
      delprovenummer,
      undersokelsesnummer,
      resultatnummer,
      sens_undersokelsesnummer,
      metodekode_sens_new
    )

  sens_undersokelse <- tables$sens_undersokelse %>%
    left_join(recoded_data) %>%
    mutate(metodekode_sens = ifelse(
      is.na(metodekode_sens_new),
      metodekode_sens,
      metodekode_sens_new
    )
    ) %>%
    select(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      provenummer,
      delprovenummer,
      undersokelsesnummer,
      resultatnummer,
      sens_undersokelsesnummer,
      metodekode_sens
    ) %>%
    group_by(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      provenummer,
      delprovenummer,
      undersokelsesnummer,
      resultatnummer,
      sens_undersokelsesnummer
    ) %>%
    summarise_all(list(func_paste)) %>%
    ungroup()

  comp <- old_table %>%
    left_join(
      sens_undersokelse,
      by = c(
        "aar",
        "ansvarlig_seksjon",
        "innsendelsesnummer",
        "provenummer",
        "delprovenummer",
        "undersokelsesnummer",
        "resultatnummer"
      )
    ) %>%
    filter(sens_undersokelsesnummer.x != sens_undersokelsesnummer.y)


  if (update == FALSE) {
    if (nrow(comp) == 0) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = sens_undersokelse,
        "diff" = comp
      )
    }
  } else {
    print("Updating table in database.")
    DBI::dbWriteTable(conn = con,
                      name = "sens_undersokelse_korrigert",
                      sens_undersokelse,
                      overwrite = TRUE)
  }
}
