#' Update report year values
#'
#' This function updates the "report_sampling_year" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
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
#'
update_report_year <- function(server,
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

  old_table <- as_tibble(tbl(con, "report_sampling_year")) %>%
    mutate(aar = as.character(aar),
           report_year = as.character(report_year))

  # Define which tables to import from
  tables <- c("prove",
              "delprove",
              "undersokelse",
              "resultat")

  innsendelse_db <- as_tibble(tbl(con, "innsendelse"))

  db_tables <- lapply(tables, function(x) as_tibble(tbl(con, x)))
  names(db_tables) <- tables

  db_tables$resultat <- db_tables$resultat %>%
    rename(analyttkode_funn = "analyttkode")

  # Create data frame
  RESULT_report_year <- innsendelse_db %>%
    left_join(db_tables$prove) %>%
    left_join(db_tables$delprove) %>%
    left_join(db_tables$undersokelse) %>%
    left_join(db_tables$resultat) %>%
    rename("pjs_year" = aar,
           "analyttkode" = analyttkode_funn) %>%
    mutate_at(vars(c(analyttkode, hensiktkode, artkode)),
              ~str_squish(.)) %>%
    mutate(pjs_year = as.character(pjs_year)) %>%
    # Use the rules above to generate the correct
    # combinations of pjs_year, hensiktkode, and
    # analyttkode for each report_year
    mutate(report_year = case_when(
      # E.coli klinisk? /svin/
      hensiktkode == "0200301002" &
        substr(analyttkode, 1,10) == "0406010105" &
        substr(artkode, 1,11) == "03100101001" &
        pjs_year == "2002" ~ "2002",
      # endring av en spesifik sak fra 2003
      pjs_year == "2003" &
        ansvarlig_seksjon == "01" &
        innsendelsesnummer == "26" ~ "2002",
      # E.coli klinisk? /svin/
      hensiktkode == "0200301002" &
        substr(analyttkode, 1,10) == "0406010105" &
        substr(artkode, 1,11) == "03100101001" &
        pjs_year == "2003" ~ "2002",
      # E.coli klinisk? /kylling/
      hensiktkode == "0200301002" &
        substr(analyttkode, 1,10) == "0406010105" &
        substr(artkode, 1,11) == "05090102004" &
        pjs_year %in% c("2002","2003") ~ "2002",
      # E.coli klinisk? /kylling/
      hensiktkode == "0200301002" &
        substr(analyttkode, 1,10) == "0406010105" &
        substr(artkode, 1,11) == "05090102004" &
        pjs_year %in% c("2004") ~ "2004",
      # E.coli klinisk? /svin/
      hensiktkode == "0200301002" &
        substr(analyttkode, 1,10) == "0406010105" &
        substr(artkode, 1,11) == "03100101001" &
        pjs_year == "2004" ~ "2004",
      # E.coli klinisk? /kylling/, ok
      hensiktkode == "0200301002" &
        substr(analyttkode, 1,10) == "0406010105" &
        substr(artkode, 1,11) == "05090102004" &
        pjs_year %in% c("2015","2016","2017") ~ "2018",
      # E.coli klinisk? /kalkun/  ok
      hensiktkode == "0200301002" &
        substr(analyttkode, 1,10) == "0406010105" &
        substr(artkode, 1,11) == "05090102005" &
        pjs_year %in% c("2015","2016","2017") ~ "2018",
      # E.coli klinisk? /hund,rev , ok
      hensiktkode == "0200301002"  &
        analyttkode == "0406010105" &
        artkode %in% c("03070101002","03070101003") &
        pjs_year %in% c("2017","2018") ~ "2019",
      # E.coli klinisk? /hund,rev, ok
      substr(hensiktkode,1,5) == "01001"&
        analyttkode == "0406010105" &
        artkode %in% c("03070101002","03070101003") &
        pjs_year %in% c("2017","2018") ~ "2019",
      # Klebsiella, alle arter,ok
      hensiktkode !="0200301"  &
        substr(analyttkode, 1,10) == "0406010504"
      & pjs_year %in% c("2017","2018","2019") ~ "2020",
      substr(hensiktkode,1,5) == "01001" &
        substr(analyttkode, 1,10) == "0406010504"
      & pjs_year %in% c("2017","2018","2019") ~ "2020",
      # Actinobacillus , ok
      substr(hensiktkode, 1,2) != "02" &
        substr(analyttkode, 1, 10) == "0406030406" &
        pjs_year %in% c("2004","2005","2011",
                        "2012","2013","2014",
                        "2015","2016","2017",
                        "2018","2019") ~ "2020",
      # Streptococcus canis, ok
      pjs_year %in% c("2020","2021","2022","2023") &
        analyttkode == "0415030202" &
        hensiktkode %in% c("0100107014",
                           "1000101",
                           "1000102") &
        substr(artkode, 1, 11) == "03070101002" ~ "2024",
      pjs_year == "2018" &
        analyttkode == "0415030202" ~ "2019",
      # Staphylococcus aureus, storfe, klinisk, ok
      pjs_year == "2021" &
        analyttkode == "0415010302" &
        hensiktkode == "0200301002" &
        artkode == "03100202001" ~ "2022",
      # Streptococcus uberis 	#"0415030220" og dysgalactiae, storfe,
      # klinisk, -mangler- helt ok samme år
      pjs_year == "2021" &
        analyttkode %in% c("0415030220","0415030203") &
        hensiktkode == "0200301002"  &
        artkode == "03100202001" ~ "2021",
      # Staphylococcus pseudintermedius , ok
      substr(hensiktkode, 1,2) != "02" &
        substr(analyttkode, 1,10) == "0415010332" &
        pjs_year %in% c("2017","2018") ~ "2019",
      #  Pseudomonas aeruginosa, ok
      pjs_year %in% c("2022","2021","2020","2019","2018") &
        analyttkode == "0405010101" ~ "2022",
      # Staphylococcus pseudintermedius og MRSP, - mangler   "0100107"
      substr(hensiktkode,1,2) != "02" &
        (substr(analyttkode, 1,10) == "0415010332" |
           analyttkode=="04150103320101") &
        pjs_year %in% c("2019", "2020","2021", '2022', '2023') ~ "2023",
      # Pasteurella, ok
      pjs_year %in% c("2017","2018","2019","2020","2021","2022" ) &
        substr(analyttkode,1,8) == "04060302" ~ "2023",
      # E.coli
      # Kliniske Høns og kalkun 2020 - 2023
      hensiktkode %in% c("06111","0100107015","0100107014","0100101") &
        substr(artkode, 1, 10) == "0509010200" &
        substr(analyttkode,1,10) == "040601010" &
        pjs_year %in% c("2020","2021","2022","2023") ~ "2024",
      # ? Høns
      !hensiktkode %in% c("0200301", "0200161")  &
        !substr(artkode, 1, 10) == "0509010200" &
        substr(analyttkode,1,10) == "0406010105"
      & pjs_year %in% c("2020","2021","2022") ~ "2023",
      substr(hensiktkode,1,5) == "01001" &
        !substr(artkode, 1, 10) == "0509010200" &
        substr(analyttkode,1,10) == "0406010105"
      & pjs_year %in% c("2020","2021","2022") ~ "2023",
      pjs_year %in% c("2020","2021","2022","2023","2024") &
        hensiktkode %in% c("06111","0100107015","0100107014","0100101") &
        analyttkode == "0406010105" &
        substr(artkode, 1, 11) == "05090102004" ~ "2024",
      pjs_year %in% c("2020","2021","2022","2023","2024") &
        hensiktkode %in% c("06111","0100107015","0100107014","0100101") &
        analyttkode == "0406010105" &
        substr(artkode, 1, 11) == "05090102005" ~ "2024",
      pjs_year %in% c("2017","2018","2019","2020","2021","2022","2023","2024") &
        analyttkode == "0403010208" ~ "2024",
      pjs_year == "2005" &
        artkode == "03070101002" &
        hensiktkode == "0200301001" ~ "2004",
      TRUE ~ pjs_year
    )
    ) %>%
    rename("aar" = pjs_year) %>%
    select(aar, analyttkode, hensiktkode, artkode, report_year) %>%
    distinct(aar, analyttkode, hensiktkode, artkode, report_year) %>%
    mutate(aar = as.character(aar),
           report_year = as.character(report_year))

  comp <- old_table %>%
    left_join(
      RESULT_report_year,
      by = c(
        "aar",
        "analyttkode",
        "hensiktkode",
        "artkode"
      )
    ) %>%
    filter(report_year.x != report_year.y)

  if (update == FALSE) {
    if (nrow(comp) == 0) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = RESULT_report_year,
        "diff" = comp
      )
    }
  } else {
    print("Updating table in database.")
    DBI::dbWriteTable(conn = con,
                    name = "report_sampling_year",
                    RESULT_report_year,
                    overwrite = TRUE)
  }
}
