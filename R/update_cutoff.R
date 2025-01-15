#' Update cutoff values for antimicrobials
#'
#' This function updates the "analytt_sens_cutoff" table in the NORM-VET Database.
#'
#' @param server Name of the server to connect to
#' @param database Name of the database to fetch data from
#' @param user Username for the database connection
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
update_cutoff <- function(server,
                          database,
                          user) {

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

  RESULT_cutoffs <- cutoff_data %>%
    group_by(cut_off_gruppe, analyttkode_sens) %>%
    slice_max(dato) %>%
    ungroup() %>%
    select(analyttkode_sens, cut_off_gruppe, cut_off)

  dbWriteTable(
    conn = con,
    name = "analytt_sens_cutoff",
    RESULT_cutoffs,
    overwrite = TRUE
    )
}
