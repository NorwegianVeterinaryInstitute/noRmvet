#' Connect to NORM-VET database and fetch data
#'
#' This function connects to the NORM-VET database and merges data in the correct format. Here, information on samples and results are imported, but not MIC values. Users will have to supply their password to the database manually when running the function.
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
fetch_nv_data <- function(server,
                          database,
                          user) {

  # Fetch password from user
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
                 "art_gruppe",
                 "materiale_gruppe",
                 "bakterie_gruppe",
                 "bakterie_kategori",
                 "report_sampling_year")

  print("Extracting tables...")

  # Import tables and correct
  init_table <- as_tibble(tbl(con, "innsendelse")) %>%
    mutate_all(~str_squish(.))

  tables <- lapply(table_list, function(x) as_tibble(tbl(con, x)) %>%
                     mutate_all(~str_squish(.)))

  names(tables) <- table_list

  print("Merging tables...")

  # Merge tables
  suppressMessages(
    df <- init_table %>%
      reduce(tables, left_join, .init = .) %>%
      filter(substr(kjennelsekode, 1,4) %in% c("0202", "0201"),
             ! hensiktkode %in% c("06097","0200303","0700109")) %>%
      mutate(resultat = ifelse(substr(kjennelsekode, 1,4) == "0202",
                               "Påvist", "Ikke påvist")) %>%
      select(
        aar,
        ansvarlig_seksjon,
        innsendelsesnummer,
        provenummer,
        delprovenummer,
        resultatnummer,
        sens_undersokelsesnummer,
        report_year,
        art_gruppe,
        materialenavn,
        mat_gruppe,
        salmonella_materiale,
        bakterie_kategori,
        bakterie_gruppe,
        resultat
        )
  )

  print("Done!")

  return(df)
}
