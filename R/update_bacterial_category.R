#' Update bacterial categories
#'
#' This function updates the "bakterie_kategori" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
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
#'
update_bacterial_category <- function(server,
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

  old_table <- as_tibble(tbl(con, "bakterie_kategori")) %>%
    mutate(aar = as.character(aar))

  tables <- c("prove",
              "delprove",
              "undersokelse",
              "resultat",
              "sens_undersokelse",
              "sens_resultat",
              "art_gruppe",
              "materiale_gruppe",
              "bakterie_gruppe",
              "bakterie_kategori",
              "report_sampling_year")

  innsendelse_db <- as_tibble(tbl(con, "innsendelse")) %>%
    mutate_all(~str_squish(.))

  db_tables <- lapply(tables, function(x) as_tibble(tbl(con, x)) %>%
                        mutate_all(~str_squish(.)))

  names(db_tables) <- tables

  RESULT_bacterial_category <- innsendelse_db %>%
    reduce(db_tables, left_join, .init = .) %>%
    dplyr::mutate(
      bakterie_kategori = case_when(
        substr(hensiktkode,1,2) == "07" ~ "Oppdrag",
        hensiktkode =="0100105048" ~ "Viktige",
        metodekode %in% c("020147","020161","020146","020145","020174",
                          "020177","020192","020197","020166","020175",
                          "020172") ~ "Viktige",
        analyttkode=="14130301"  ~ "Viktige",
        substr(analyttkode, 1, 14) %in% c(
          "04060101050103",
          "04150303060101",
          "04150303070101") ~ "Viktige",
        substr(analyttkode, 1, 14) %in% c("04060113020102") ~ "Zoonotisk",
        substr(analyttkode, 1, 10) %in% c("0403010201",
                                          "0403010204") ~ "Zoonotisk",
        substr(analyttkode, 1, 8) %in% c("04060103",
                                         "04030102") ~ "Zoonotisk",
        hensiktkode == "0200301" & analyttkode %in% c(
          "0415010309",
          "0415010332",
          "0415010312",
          "0415010323",
          "0415010302",
          "0406010105"
        ) ~ "Indikator",
        hensiktkode =="0200301002" ~ "Klinisk",
        analyttkode=="04150103320101" & hensiktkode == "08" ~ "Klinisk",
        analyttkode=="0415010312"~ "Indikator",
        report_year == "2009" &
          artkode == "03100202004" &
          substr(analyttkode, 1, 10) == "0406010105"  ~ "Klinisk",
        artkode %in% c("03100202001","03070101002") &
          analyttkode %in% c("0415010323","0415010302","0415010302") ~ "Klinisk",
        as.numeric(report_year) < 2022  & analyttkode == "0415010302"   ~ "Klinisk",
        substr(hensiktkode, 1, 2) != "02" & analyttkode %in% c(
          "0415010332",
          "415010317"
        ) ~ "Klinisk",
        # report_year < "2022"  & analyttkode=="0415010332"   ~ "Klinisk",
        #  report_year == "2022"  & substr(analyttkode, 1, 8) =="04150103" ~ "Indikator",
        # report_year < "2023"  & substr(analyttkode, 1, 8) =="04150103" ~ "Klinisk",
        hensiktkode =="0200127"  & analyttkode=="0406030213"   ~ "Klinisk",
        analyttkode=="406020203" ~ "Klinisk",
        aar == "2011" &
          ansvarlig_seksjon == "01" &
          innsendelsesnummer== "5255" ~ "Klinisk",
        aar == "2023" &
          ansvarlig_seksjon == "01" &
          innsendelsesnummer == "2537" ~ "Klinisk",
        substr(hensiktkode, 1, 2) %in% c("01") ~ "Klinisk",
        substr(analyttkode, 1, 14) %in% c(
          "04060101050103",
          "04150303060101",
          "04150303070101") ~ "Viktige",
        substr(hensiktkode, 1, 5) == "10001" ~ "Klinisk",
        substr(hensiktkode, 1, 2) == "01" ~ "Klinisk",
        (hensiktkode != "0200301002") |
          !(substr(hensiktkode,1,2) %in% c("01","04","06","08")) &
          substr(analyttkode, 1, 10) %in% c(
            "0406010105",
            "0406010610",
            "0415030306",
            "0415030307",
            "0415010332",
            "0415010302"
          ) ~ "Indikator",
        analyttkode %in% c(
          "0415030309",
          "04150303",
          "0415010302020101",
          "0406020203",
          "0406020206"
        ) ~ "Klinisk",
        substr(analyttkode, 1, 8) %in% c("04150103",
                                         "04060105",
                                         "04060117",
                                         "04060302") ~ "Klinisk",
        substr(analyttkode, 1, 10) %in% c("0406020202",
                                          "0415030201",
                                          "0406010501") ~ "Klinisk",
        substr(analyttkode, 1, 10) %in% c("0415030202",
                                          "0406010504",
                                          "0406011101",
                                          "0406030406",
                                          "0415010332") ~ "Klinisk",
        substr(analyttkode, 1, 12) == "04150103020201" ~ "Klinisk",
        analyttkode == "040601060401" ~ "Klinisk",
        TRUE ~ "Viktige"
      )
    ) %>%
    mutate(bakterie_kategori = case_when(
      aar == "2013" &
        bakterie_gruppe == "Staphylococcus pseudintermedius" &
        art_gruppe == "Hund" ~ "Klinisk",
      aar %in% c("2021","2022") &
        bakterie_gruppe == "Escherichia coli" &
        art_gruppe == "Høns" &
        mat_gruppe %in% c("Lever","Milt") ~ "Klinisk",
      # Endre gener til viktige
      analyttnavn %in% c(
        "ampC (gen), kromosomalt, oppregulert (360 bp)",
        "ESBL-gen",
        "ampC (gen), plasmidbårent",
        "mecA (gen for meticillinresistens)",
        "nuc (St. aureus nuklease-gen)",
        "qnr, kinolonresistensgener",
        "ampC (gen)"
      ) ~ "Viktige",
      analyttkode == "020147" ~ "Indikator",
      substr(analyttkode, 1,2) == "07" ~ "Viktige",
      analyttkode == "0403010208" ~ "Klinisk",
      TRUE ~ bakterie_kategori
    )) %>%
    select(aar, analyttkode, hensiktkode, metodekode, artkode, bakterie_kategori) %>%
    distinct(aar, analyttkode, hensiktkode, metodekode, artkode, bakterie_kategori) %>%
    mutate(aar = as.character(aar))

  comp <- old_table %>%
    left_join(
      RESULT_bacterial_category,
      by = c(
        "aar", "analyttkode", "hensiktkode", "metodekode", "artkode"
      )
    ) %>%
    filter(bakterie_kategori.x != bakterie_kategori.y)

  if (update == FALSE) {
    if (nrow(comp) == 0) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = RESULT_bacterial_category,
        "diff" = comp
      )
    }
  } else {
    print("Updating table in database.")
    DBI::dbWriteTable(conn = con,
                      name = "bakterie_kategori",
                      RESULT_bacterial_category,
                      overwrite = TRUE)
  }
}
