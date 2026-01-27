#' Update material groups
#'
#' This function updates the "materiale_gruppe" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
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
update_material_group <- function(server,
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

  old_table <- as_tibble(tbl(con, "materiale_gruppe"))

  tables <- c("prove",
              "delprove",
              "undersokelse",
              "resultat",
              "sens_undersokelse",
              "sens_resultat",
              "art_gruppe",
              "materiale",
              "bakterie_gruppe",
              "bakterie_kategori",
              "analytt_sens_group",
              "analytt_sens_cutoff",
              "report_sampling_year")

  innsendelse_db <- as_tibble(tbl(con, "innsendelse")) %>%
    mutate_all(~str_squish(.))

  db_tables <- lapply(tables, function(x) as_tibble(tbl(con, x)) %>%
                        mutate_all(~str_squish(.)))

  names(db_tables) <- tables

  db_tables$sens_resultat <- rename(db_tables$sens_resultat,
                                    "analyttkode_sens" = analyttkode) %>%
    select(-kjennelsekode)

  db_tables$sens_undersokelse <- rename(db_tables$sens_undersokelse,
                                        "metodekode_sens" = metodekode)

  db_tables$materiale <- db_tables$materiale %>%
    rename("materialekode" = kode)

  RESULT_material_group <- innsendelse_db %>%
    reduce(db_tables, left_join, .init = .) %>%
    dplyr::mutate(kode = stringr::str_trim(materialekode)) %>%
    # Define material groups
    dplyr::mutate(mat_gruppe = case_when(
      substr(kode,1,5) %in% c("04001","05006") ~ "Luftveier",
      substr(kode,1,7) %in% c("0400515") ~ "Tarminnhold",
      substr(kode,1,5) %in% c("05013", "05001") ~ "Tarminnhold",
      substr(kode,1,5) == "05014" ~ "Urin",
      substr(kode,1,5) == "04007" | kode == "0400511" ~ "Hud og slimhinner",
      substr(kode,1,5) == "20001" ~ "Avføring og Støvprøve",
      substr(kode,1,2) %in% c("10", "13") ~ "Miljø",
      kode == "090080202" ~ as.character(NA),
      substr(kode,1,5) %in% c("04010", "09008", "02001") ~ "Kjøtt",
      substr(kode,1,5) == "05005" ~ "Melk",
      substr(kode,1,9) %in% c("090010601", "090010602") ~ "Ost",
      substr(kode,1,5) %in% c("09001") ~ "Meieriprodukter",
      substr(kode,1,7) %in% c("0900301") ~ "Meieriprodukter",
      kode %in% c("0900401010218") ~ "Bladsalat",
      kode %in% c("0901202") ~ "Krydderurter",
      kode %in% c("090090102") ~ "Skjell",
      kode %in% c("090090202","09009030103") ~ "Krepsdyr og bløtdyr",
      substr(kode,1,5) %in% c("11003", "11004") ~ "Dyrefôr",
      TRUE ~ navn
    )) %>%
    rename("materialenavn" = navn) %>%
    # Define salmonella material
    mutate(saksnr = paste(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      provenummer,
      delprovenummer,
      undersokelsesnummer,
      resultatnummer,
      sep = "-"
    )) %>%
    mutate(salmonella_materiale = case_when(
      saksnr %in% c(
        "2021-01-1186-1-1-1-1",
        "2021-01-1233-1-1-1-1",
        "2021-01-1312-1-1-1-1",
        "2021-01-1675-4-1-1-1",
        "2021-01-1873-1-1-1-1",
        "2021-01-2555-1-1-1-1",
        "2021-01-263-1-1-1-1",
        "2021-01-281-1-1-1-1",
        "2021-01-3482-1-1-1-1",
        "2021-01-3957-1-1-1-1",
        "2021-01-585-1-1-1-1",
        "2021-01-786-1-1-1-1",
        "2021-01-817-1-1-1-1",
        "2022-01-1613-1-1-1-1",
        "2022-01-385-1-1-1-1",
        "2022-22-231-1-1-1-1",
        "2022-22-231-2-1-1-1",
        "2023-01-4954-1-1-1-1",
        "2023-01-6519-1-1-1-1",
        "2023-01-7715-1-1-1-1",
        "2023-01-1714-1-1-1-1",
        "2023-01-4793-1-1-1-1",
        "2023-01-5267-1-1-1-1",
        "2023-01-5757-1-1-1-1",
        "2023-01-4954-1-1-1-1",
        "2023-01-6519-1-1-1-1",
        "2023-01-7715-1-1-1-1",
        "2023-01-1714-1-1-1-1",
        "2023-01-4793-1-1-1-1",
        "2023-01-5267-1-1-1-1",
        "2023-01-5757-1-1-1-1",
        "2023-22-259-1-1-1-2",
        "2024-01-6438-4-1-1-2",
        "2024-01-9468-1-1-1-1",
        "2024-01-11448-1-1-1-3",
        "2024-01-11745-1-1-1-3",
        "2024-01-11746-1-1-1-1",
        "2024-01-11747-1-1-1-1",
        "2024-01-12453-1-1-1-1",
        "2024-22-1436-1-1-1-2",
        "2024-22-1437-1-1-1-2",
        "2024-22-1439-3-1-1-1",
        "2024-22-1445-1-1-1-2",
        "2024-22-1445-3-1-1-2",
        "2024-22-1469-2-1-1-1"
      ) ~ "Mat",
      saksnr %in% c(
        "2023-40-9-1-1-1-1"
      ) ~ "Dyrefôr",
      saksnr %in% c(
        "2023-01-4956-1-1-1-1",
        "2024-22-1516-1-1-1-2"
      ) ~ "EX",
      TRUE ~ "Dyr"
    )) %>%
    mutate(mat_gruppe = case_when(
      saksnr %in% c("2006-01-1877-1-1-1-2",
                    "2006-01-2183-1-1-1-1",
                    "2006-01-2197-1-1-1-2") ~ "Tarminnhold",
      report_year == "2007" &
        bakterie_gruppe == "Escherichia coli" &
        art_gruppe == "Kalkun" &
        materialenavn == "Miljøprøver" ~ "Tarminnhold",
      report_year %in% c("2009","2011") &
        art_gruppe == "Høns" &
        bakterie_kategori == "Indikator" &
        materialenavn == "Miljøprøver" ~ "Tarminnhold",
      aar == "2006" &
        ansvarlig_seksjon == "01" &
        innsendelsesnummer == "2197" &
        bakterie_gruppe == "Enterococcus faecalis" ~ "Tarminnhold",
      TRUE ~ mat_gruppe
    )) %>%
    mutate(
      mat_gruppe = case_when(
        mat_gruppe == "Kjøtt" &
          bakterie_kategori == "Indikator" &
          art_gruppe == "Høns" ~ "Kyllingkjøtt",
        mat_gruppe == "Kjøtt" &
          bakterie_kategori == "Indikator" &
          art_gruppe == "Svin" ~ "Svinekjøtt",
        mat_gruppe == "Kjøtt" &
          bakterie_kategori == "Indikator" &
          art_gruppe == "Storfe" ~ "Storfekjøtt",
        mat_gruppe == "Kjøtt" &
          bakterie_kategori == "Indikator" &
          art_gruppe == "Sau" ~ "Sauekjøtt",
        mat_gruppe == "Kjøtt" &
          bakterie_kategori == "Indikator" &
          art_gruppe == "Kalkun" ~ "Kalkunkjøtt",
        TRUE ~ mat_gruppe
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
      materialekode,
      materialenavn,
      mat_gruppe,
      salmonella_materiale
    ) %>%
    group_by_all() %>%
    summarise_all(list(func_paste)) %>%
    ungroup()

  comp <- old_table %>%
    left_join(
      RESULT_material_group,
      by = c(
        "aar",
        "ansvarlig_seksjon",
        "innsendelsesnummer",
        "provenummer",
        "delprovenummer",
        "undersokelsesnummer",
        "resultatnummer",
        "materialekode",
        "materialenavn"
      )
    ) %>%
    filter(mat_gruppe.x != mat_gruppe.y |
             salmonella_materiale.x != salmonella_materiale.y)

  if (update == FALSE) {
    if (nrow(comp) == 0) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = RESULT_material_group,
        "diff" = comp
      )
    }
  } else {
    print("Updating table in database.")
    DBI::dbWriteTable(conn = con,
                      name = "materiale_gruppe",
                      RESULT_material_group,
                      overwrite = TRUE)
  }
}
