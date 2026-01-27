#' Update species groups
#'
#' This function updates the "art_gruppe" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
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
update_species_groups <- function(server,
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

  old_table <- as_tibble(tbl(con, "art_gruppe"))

  table_list <- c("prove",
                  "art_gruppe")

  innsendelse_db <- as_tibble(tbl(con, "innsendelse")) %>%
    mutate_at(
      vars(c("aar","ansvarlig_seksjon","innsendelsesnummer")),
      ~as.character(.)
    )

  tables <- lapply(table_list, function(x) as_tibble(tbl(con, x)))

  tables <- lapply(tables, function(x) {
    x %>%
      mutate_at(
        vars(
          c(
            "aar",
            "ansvarlig_seksjon",
            "innsendelsesnummer"
          )
        ),
        ~as.character(.)
      )
  })


  # Combine tables
  RESULT_artkode_gruppe <- innsendelse_db %>%
    reduce(tables, left_join, .init = .) %>%
    dplyr::mutate(artkode = stringr::str_trim(artkode)) %>%
    left_join(species_codes) %>%
    dplyr::mutate(
      art_gruppe = case_when(
        artkode == "03100101001007" ~ "Villsvin",
        hensiktkode == "0200160" ~ "Slaktekylling eldre enn 50 døgn",
        substr(driftsformkode, 1, 1) == "1" ~ art,
        hensiktkode == "0200105001" ~ art,
        artkode == "05090101005" ~ "Villfugl",
        substr(artkode, 1, 4) %in% c("0507",
                                     "0508",
                                     "0510",
                                     "0511",
                                     "0512",
                                     "0514",
                                     "0519") ~ "Villfugl",
        hensiktkode == "0200301" &
          substr(artkode, 1, 3) == "120" ~ "Skjell",
        artkode == "03070101003" ~ "Rev",
        aar == "2021" &
          innsendelsesnummer %in% c("1233","817") &
          ansvarlig_seksjon == "01" ~ "Sau",
        # Exclude specific samples from salmonella counting
        # denoted with "EX"
        aar == "2021" &
          innsendelsesnummer %in% c("281","2418") &
          ansvarlig_seksjon == "01" ~ "EX",
        is.na(art_gruppe) &
          grepl("lymfeknute", ignore.case = T, merknad) &
          grepl("svin|gris", ignore.case = T, merknad) ~ "Svin",
        is.na(art_gruppe) &
          grepl("lymfeknute", ignore.case = T, merknad) &
          grepl("storfe", ignore.case = T, merknad) ~ "Storfe",
        TRUE ~ art
      )) %>%
    select(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      artkode,
      hensiktkode,
      driftsformkode,
      art_gruppe
    ) %>%
    mutate(
      saksnr = paste(aar,
                     ansvarlig_seksjon,
                     innsendelsesnummer,
                     art_gruppe,
                     sep = "-"),
      dupl = duplicated(saksnr)
    ) %>%
    filter(dupl == FALSE) %>%
    select(-c(saksnr, dupl))

  comp <- old_table %>%
    left_join(
      RESULT_artkode_gruppe,
      by = c(
        "aar",
        "ansvarlig_seksjon",
        "innsendelsesnummer",
        "artkode",
        "hensiktkode",
        "driftsformkode"
      )
    ) %>%
    filter(art_gruppe.x != art_gruppe.y)

  if (update == FALSE) {
    if (nrow(comp) == 0) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = RESULT_artkode_gruppe,
        "diff" = comp
      )
    }
  } else {
    print("Updating table in database.")
    DBI::dbWriteTable(conn = con,
                      name = "art_gruppe",
                      RESULT_artkode_gruppe,
                      overwrite = TRUE)
  }
}
