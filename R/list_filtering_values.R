#' List filtering values
#'
#' Function for listing filtering values from the database groups.
#'
#' @param server Name of the server to connect to
#' @param database Name of the database to fetch data from
#' @param user Username for the database connection
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
list_filtering_values <- function(server,
                                  database,
                                  user) {

  # Fetch password from user
  pw <- getPass()

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
                 "sens_undersokelse",
                 "art_gruppe",
                 "materiale_gruppe",
                 "bakterie_gruppe",
                 "bakterie_kategori",
                 "report_sampling_year")

  # Import tables and correct
  init_table <- as_tibble(tbl(con, "innsendelse")) %>%
    mutate_all(~str_squish(.))

  tables <- lapply(table_list, function(x) as_tibble(tbl(con, x)) %>%
                     mutate_all(~str_squish(.)))

  names(tables) <- table_list

  tables$sens_undersokelse <- rename(tables$sens_undersokelse,
                                     "metodekode_sens" = metodekode)

  # Merge tables
  init_table %>%
    reduce(tables, left_join, .init = .) %>%
    filter(substr(kjennelsekode, 1,4) %in% c("0202", "0201")) %>%
    select(
      report_year,
      art_gruppe,
      mat_gruppe,
      bakterie_kategori,
      bakterie_gruppe) %>%
    distinct() %>%
    filter(!is.na(bakterie_gruppe))

}
