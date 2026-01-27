#' Update cutoff values for antimicrobials
#'
#' This function updates the "analytt_sens_cutoff" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
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
update_cutoff <- function(server,
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

  old_table <- as_tibble(tbl(con, "analytt_sens_cutoff"))

  RESULT_cutoffs <- cutoff_data %>%
    group_by(cut_off_gruppe, analyttkode_sens) %>%
    slice_max(dato) %>%
    ungroup() %>%
    select(analyttkode_sens, cut_off_gruppe, cut_off)

  comp <- old_table %>%
    left_join(
      RESULT_cutoffs,
      by = c(
        "analyttkode_sens",
        "cut_off_gruppe"
      )
    ) %>%
    filter(cut_off.x != cut_off.y)

  if (update == FALSE) {
    if (nrow(comp) == 0) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      list(
        "old_data" = old_table,
        "new_data" = RESULT_cutoffs,
        "diff" = comp
      )
    }
  } else {
    print("Updating table in database.")
    dbWriteTable(
      conn = con,
      name = "analytt_sens_cutoff",
      RESULT_cutoffs,
      overwrite = TRUE
    )
  }
}
