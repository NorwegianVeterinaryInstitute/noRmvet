#' Update bacterial groups
#'
#' This function updates the "bakterie_gruppe" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
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
update_bacterial_group <- function(server,
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

  old_table <- as_tibble(tbl(con, "bakterie_gruppe"))

  RESULT_bacterial_group <-
    as_tibble(tbl(con, "analytt_funn")) %>%
    select(kode, navn) %>%
    as.data.frame() %>%
    dplyr::mutate(kode = stringr::str_trim(kode)) %>%
    dplyr::mutate(
      gruppe = case_when(
        grepl("ESBL|AmpC", navn, ignore.case = TRUE) &
          substr(kode, 1, 10) %in% c("0406010105") |
          kode == "05010409" ~ "ESBL",
        grepl("Vankomycinresistent Enterococcus faecalis", navn, ignore.case = TRUE) ~ "VRE - faecalis",
        kode == "0415010302020101" ~ "MRSA",
        kode == "04150103320101" |
          kode == "04150103170101" ~ "MRSP",
        kode == "041503031901" ~ "LZ",
        grepl("Vankomycinresistent Enterococcus faecium", navn, ignore.case = TRUE) ~ "VRE - faecium",
        grepl("Enterococcus faecalis", navn, ignore.case = TRUE) ~ "Enterococcus faecalis",
        grepl("Enterococcus faecium", navn, ignore.case = TRUE) ~ "Enterococcus faecium",
        substr(kode, 1, 8) %in% c("04060103") ~ "Salmonella",
        grepl("Campylobacter coli" , navn, ignore.case = TRUE) ~ "Campylobacter coli",
        grepl("Campylobacter jejuni" , navn, ignore.case = TRUE) ~ "Campylobacter jejuni",
        grepl("Staphylococcus pseudintermedius" , navn, ignore.case = TRUE) |
          grepl("Staphylococcus intermedius" , navn, ignore.case = TRUE) ~ "Staphylococcus pseudintermedius",
        grepl("Staphylococcus aureus" , navn, ignore.case = TRUE) ~ "Staphylococcus aureus",
        grepl("Staphylococcus felis" , navn, ignore.case = TRUE) ~ "Staphylococcus felis",
        #substr(kode, 1, 10) %in% c("0415010317") ~  "Staphylococcus intermedius",
        substr(kode, 1, 8) %in% c("04150103") ~  "CNS",
        grepl("karba", navn, ignore.case = TRUE) ~ "Karba",
        grepl("Escherichia coli - kinolonresistent", navn, ignore.case = TRUE) ~ "QREC",
        grepl("Escherichia coli - kolistinresistent", navn, ignore.case = TRUE) ~ "Escherichia coli - Kolistinresistent",
        grepl("Escherichia coli" , navn, ignore.case = TRUE) ~ "Escherichia coli",
        substr(kode, 1, 10) %in% c("0405010101") ~  "Pseudomonas aeruginosa",
        substr(kode, 1, 10) %in% c("0415030203") ~  "Streptococcus dysgalactia",
        substr(kode, 1, 10) %in% c("0415030220") ~  "Streptococcus uberis",
        substr(kode, 1, 10) %in% c("0415030202") ~  "Streptococcus canis",
        substr(kode, 1, 8) %in% c("04150302") ~  "Streptococcus",
        substr(kode, 1, 8) %in% c("04060105") ~  "Klebsiella",
        substr(kode, 1, 8) %in% c("04060113") ~  "Yersinia",
        substr(kode, 1, 8) %in% c("04060202") ~  "Vibrio",
        substr(kode, 1, 8) %in% c("04060203") ~  "Aeromonas",
        substr(kode, 1, 8) %in% c("04060205") ~  "Moritella",
        substr(kode, 1, 8) %in% c("04060302") ~  "Pasteurella",
        substr(kode, 1, 8) %in% c("04060304") ~  "Actinobacillus",
        kode == "0403010208" ~ "Campylobacter upsaliensis",
        TRUE ~ as.character(NA)
      )
    ) %>%
    dplyr::mutate(
      cut_off_gruppe = case_when(
        grepl("ESBL|AmpC", gruppe, ignore.case = TRUE) ~ "Escherichia coli",
        grepl("Vankomycinresistent Enterococcus faecalis", gruppe, ignore.case = TRUE) ~ "Enterococcus faecalis",
        grepl("Vankomycinresistent Enterococcus faecium", gruppe, ignore.case = TRUE) ~ "Enterococcus faecium",
        grepl("Enterococcus faecalis", gruppe, ignore.case = TRUE) ~ "Enterococcus faecalis",
        grepl("Enterococcus faecium", gruppe, ignore.case = TRUE) ~ "Enterococcus faecium",
        grepl("Salmonella", gruppe, ignore.case = TRUE) ~ "Salmonella",
        grepl("Campylobacter coli" , gruppe, ignore.case = TRUE) ~ "Campylobacter coli",
        grepl("Campylobacter jejuni" , gruppe, ignore.case = TRUE) ~ "Campylobacter jejuni",
        grepl("Staphylococcus pseudintermedius" , gruppe, ignore.case = TRUE) ~ "Staphylococcus pseudintermedius",
        grepl("Staphylococcus aureus" , gruppe, ignore.case = TRUE) |
          grepl("Staphylococcus felis" , gruppe, ignore.case = TRUE) ~ "Staphylococcus aureus",
        grepl("Enterobacteriaceae - karbapenemasedannende", gruppe, ignore.case = TRUE) ~ "Escherichia coli",
        grepl("Escherichia coli - kinolonresistent", gruppe, ignore.case = TRUE) ~ "Escherichia coli",
        grepl("Escherichia coli - kolistinresistent", gruppe, ignore.case = TRUE) ~ "Escherichia coli",
        grepl("Escherichia coli" , gruppe, ignore.case = TRUE) ~ "Escherichia coli",
        grepl("Streptococcus dysgalactia", gruppe, ignore.case = T) ~ "Streptococcus dysgalactia",
        grepl("Streptococcus uberis", gruppe, ignore.case = T) ~ "Streptococcus uberis",
        grepl("Streptococcus canis", gruppe, ignore.case = T) ~ "Streptococcus canis",
        grepl("Klebsiella", gruppe, ignore.case = T) ~ "Klebsiella pneumoniae",
        grepl("Actinobacillus", gruppe, ignore.case = T) ~ "Actinobacillus pleuropneumoniae",
        grepl("QREC", gruppe, ignore.case = T) ~ "Escherichia coli",
        grepl("Escherichia coli - Kolistinresistent", gruppe, ignore.case = T) ~ "Escherichia coli",
        grepl("Karba", gruppe, ignore.case = T) ~ "Escherichia coli",
        #grepl("Staphylococcus intermedius", gruppe, ignore.case = T) ~ "Staphylococcus pseudintermedius",
        grepl("Yersinia", gruppe, ignore.case = T) ~ "Yersinia",
        grepl("Vibrio", gruppe, ignore.case = T) ~ "Vibrio",
        grepl("Aeromonas", gruppe, ignore.case = T) ~ "Aeromonas",
        grepl("Moritella", gruppe, ignore.case = T) ~ "Moritella",
        grepl("Pasteurella", gruppe, ignore.case = T) ~ "Pasteurella",
        grepl("Pseudomonas aeruginosa", gruppe, ignore.case = T) ~ "Pseudomonas aeruginosa",
        grepl("CNS", gruppe, ignore.case = T) ~ "CNS",
        grepl("VRE - faecalis", gruppe, ignore.case = T) ~ "Enterococcus faecalis",
        grepl("VRE - faecium", gruppe, ignore.case = T) ~ "Enterococcus faecium",
        gruppe == "Campylobacter upsaliensis" ~ "Campylobacter jejuni",
        TRUE ~ as.character(NA)
      )
    ) %>%
    rename(
      "analyttkode" = kode,
      "analyttnavn" = navn,
      "bakterie_gruppe" = gruppe
    )

  comp <- old_table %>%
    left_join(
      RESULT_bacterial_group,
      by = c(
        "analyttkode",
        "analyttnavn"
      )
    ) %>%
    filter(bakterie_gruppe.x != bakterie_gruppe.y |
             cut_off_gruppe.x != cut_off_gruppe.y)

  if (update == FALSE) {
    if (nrow(comp) == 0) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = RESULT_bacterial_group,
        "diff" = comp
      )
    }
  } else {
    print("Updating table in database.")
    DBI::dbWriteTable(conn = con,
                      name = "bakterie_gruppe",
                      RESULT_bacterial_group,
                      overwrite = TRUE)
  }
}
