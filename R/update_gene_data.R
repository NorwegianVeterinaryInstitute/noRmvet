#' Update gene names
#'
#' This function updates the "gene_data" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
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
update_gene_data <- function(server, database, user, update = FALSE) {
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

  old_table <- as_tibble(tbl(con, "gene_data"))

  # Define which tables to import
  table_list = c(
    "prove",
    "delprove",
    "undersokelse",
    "resultat",
    "sens_undersokelse_korrigert",
    "sens_resultat"
  )

  init_table <- as_tibble(tbl(con, "innsendelse")) %>%
    mutate_all(~ str_squish(.))

  tables <- lapply(table_list, function(x) {
    as_tibble(tbl(con, x)) %>%
      mutate_all(~ str_squish(.))
  })

  names(tables) <- table_list

  tables$sens_resultat <- rename(
    tables$sens_resultat,
    "analyttkode_sens" = analyttkode,
    "kjennelsekode_sens" = kjennelsekode
  )

  RESULT_gene_data <- init_table %>%
    reduce(tables, left_join, .init = .) %>%
    mutate(
      gene_desc = case_when(
        metodekode_sens == "070225" ~
          "ESBL (bredspektret betalaktamase)-gen, blaCMY-2 - påvisning med real-time PCR",
        metodekode_sens == "070268" |
          metodekode == "070268" ~
          "ESBL-gen: blaCTX-M, blaSHV og blaTEM - påvisning med PCR og sekvensering",
        metodekode_sens == "070264" ~
          "AmpC-gener, plasmidbårne - påvisning med multipleks PCR",
        metodekode_sens == "070282" ~
          "AmpC-gen, blaCMY-2 - påvisning med PCR",
        metodekode_sens == "070248" |
          metodekode == "070248" ~
          "ampC-gen, oppregulert, kromosomalt - påvisning med PCR",
        metodekode_sens == "070246" ~
          "ESBL (bredspektret betalaktamase)-gen, CTX-M, SHV og TEM - påvisning med PCR",
        metodekode_sens == "070278" ~
          "Bakterier, helgenomsekvensering (GATC Biotech AG, Køln, Tyskland)",
        metodekode_sens == "070304" |
          metodekode == "070304" ~
          "Bakterier, helgenomsekvensering (Illumina MiSeq)",
        metodekode_sens == "070218" ~
          "Staphylococcus aureus spa-type - bestemmelse med PCR (St. Olavs Hospital)",
        metodekode_sens == "070201" ~
          "Meticillinresistensgen mecA og mecC - påvisning med PCR",
        metodekode_sens == "070271" ~
          "MCR-gen, kolistinresistensgener - påvisning med dupleks PCR for mcr-1 og mcr-2",
        metodekode_sens == "070254" |
          metodekode == "070254" ~
          "Meticillinresistent Staphylococcus aureus og Staphylococcus pseudintermedius - påvisning av mecA-gen og nuc-gen med real-time PCR",
        TRUE ~ NA_character_
      ),
      gene_method = case_when(
        metodekode_sens %in%
          c(
            "070225",
            "070264",
            "070268",
            "070282",
            "070248",
            "070271",
            "070218",
            "070201",
            "070246",
            "070254"
          ) |
          metodekode %in%
            c(
              "070225",
              "070264",
              "070268",
              "070282",
              "070248",
              "070271",
              "070218",
              "070201",
              "070246",
              "070254"
            ) ~ "PCR",
        metodekode_sens %in%
          c("070278", "070304") |
          metodekode %in% c("070278", "070304") ~ "WGS",
        TRUE ~ NA_character_
      ),
      gene_code = case_when(
        substr(analyttkode, 1, 2) == "14" &
          substr(kjennelsekode, 1, 4) != "0201" &
          substr(analyttkode_sens, 1, 2) == "14" &
          substr(kjennelsekode_sens, 1, 4) != "0201" ~ analyttkode_sens,
        substr(analyttkode, 1, 2) == "14" &
          substr(kjennelsekode, 1, 4) == "0201" &
          substr(analyttkode_sens, 1, 2) == "14" &
          substr(kjennelsekode_sens, 1, 4) != "0201" ~ analyttkode_sens,
        substr(analyttkode, 1, 2) == "14" &
          substr(kjennelsekode, 1, 4) != "0201" &
          substr(analyttkode_sens, 1, 2) == "14" &
          substr(kjennelsekode_sens, 1, 4) == "0201" ~ analyttkode,
        substr(analyttkode, 1, 2) != "14" &
          substr(kjennelsekode, 1, 4) != "0201" &
          substr(analyttkode_sens, 1, 2) == "14" &
          substr(kjennelsekode_sens, 1, 4) != "0201" ~ analyttkode_sens,
        substr(analyttkode, 1, 2) == "14" &
          substr(kjennelsekode, 1, 4) != "0201" &
          substr(analyttkode_sens, 1, 2) != "14" &
          substr(kjennelsekode_sens, 1, 4) != "0201" ~ analyttkode,
        substr(analyttkode, 1, 2) == "14" &
          substr(kjennelsekode, 1, 4) != "0201" &
          is.na(analyttkode_sens) ~ analyttkode,
        TRUE ~ NA
      )
    ) %>%
    left_join(gene_data, by = c("gene_code" = "analyttkode")) %>%
    filter(!is.na(gene)) %>%
    select(
      metodekode,
      metodekode_sens,
      gene_desc,
      gene_method,
      analyttkode,
      analyttkode_sens,
      gene
    ) %>%
    distinct()

  only_in_old <- anti_join(old_table, RESULT_gene_data, by = names(old_table))
  only_in_new <- anti_join(
    RESULT_gene_data,
    old_table,
    by = names(RESULT_gene_data)
  )

  differences <- bind_rows(
    mutate(only_in_old, source = "only_in_old"),
    mutate(only_in_new, source = "only_in_new")
  )

  comp <- old_table %>%
    left_join(
      RESULT_gene_data,
      by = c(
        "analyttkode",
        "analyttkode_sens"
      )
    ) %>%
    filter(gene.x != gene.y)

  if (update == FALSE) {
    if (nrow(comp) == 0 & nrow(old_table) == nrow(RESULT_gene_data)) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = RESULT_gene_data,
        "diff" = differences
      )
    }
  } else {
    print("Updating table in database.")
    DBI::dbWriteTable(
      conn = con,
      name = "gene_data",
      RESULT_gene_data,
      overwrite = TRUE
    )
  }
}
