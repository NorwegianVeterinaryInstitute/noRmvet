#' Connect to NORM-VET database and fetch MIC data
#'
#' This function connects to the NORM-VET database and merges data in the correct format. Here, all data, including MIC values are imported. Users will have to supply their password to the database manually when running the function.
#'
#' @param server Name of the server to connect to
#' @param database Name of the database to fetch data from
#' @param user Username for the database connection
#' @param pw_method Specifies how the user supplies the database password, either "prompt" for an actual promt, or "local" to specify a text file holding the password
#' @param pw_file_path Path to text file holding the password, must be supplied when using "pw_method = local"
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom getPass getPass
#' @importFrom tidyr as_tibble
#' @importFrom stringr str_squish
#' @importFrom purrr reduce
#'
fetch_nv_mic_data <- function(server,
                              database,
                              user) {

  # Fetch password
  pw <- getPass()

  print("Connecting to database...")

  # Connect to database
  con <- dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = server,
    Database = database,
    UID = user,
    PWD = pw
  )

  # Define which tables to import
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

  print("Extracting tables...")

  # Import tables and correct
  init_table <- as_tibble(tbl(con, "innsendelse")) %>%
    mutate_all(~str_squish(.))

  tables <- lapply(table_list, function(x) as_tibble(tbl(con, x)) %>%
                     mutate_all(~str_squish(.)))

  names(tables) <- table_list

  tables$sens_resultat <- rename(tables$sens_resultat,
                                 "analyttkode_sens" = analyttkode) %>%
      select(-kjennelsekode)

  print("Merging tables...")

  # Merge tables
  suppressMessages(
    df <- init_table %>%
      reduce(tables, left_join, .init = .) %>%
      filter(substr(kjennelsekode, 1,4) %in% c("0202", "0201") |
               kjennelsekode == "02",
             ! hensiktkode %in% c("06097","0200303","0700109")) %>%
      mutate(resultat = case_when(
        substr(kjennelsekode, 1,4) == "0202" ~ "Påvist",
        kjennelsekode == "02" ~ "Undersøkt",
        TRUE ~ "Ikke påvist"))
  )

  print("Done!")

  return(df)
}
