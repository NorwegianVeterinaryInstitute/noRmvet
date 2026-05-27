#' Check for date errors in EFSA data
#'
#' Function for checking EFSA data for incorrect dates
#'
#' @param data Input data
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tibble tibble
#'
check_efsa_dates <- function(data) {

  date_required_cols <- c(
    "start_dato",
    "uttaksdato",
    "analysedato",
    "art_gruppe",
    "labIsolCode",
    "bakterie_gruppe",
    "sampY",
    "sampM",
    "sampD",
    "isolY",
    "isolM",
    "isolD",
    "analysisY",
    "analysisM",
    "analysisD"
  )

  missing_date_cols <- setdiff(
    c("start_dato", "uttaksdato", "analysedato"),
    names(data)
  )

  if (length(missing_date_cols) > 0) {
    warning(
      "Date check skipped because these columns are missing: ",
      paste(missing_date_cols, collapse = ", ")
    )

    date_issues <- tibble()

  } else {
    data_checked <- data %>%
      mutate(
        date_order_issue = case_when(
          is.na(start_dato) | is.na(uttaksdato) | is.na(analysedato) ~
            "Missing date needed for date order check",

          !(uttaksdato <= start_dato & start_dato <= analysedato) ~
            "Expected uttaksdato <= start_dato <= analysedato",

          TRUE ~ NA_character_
        )
      )

    date_issues <- data_checked %>%
      filter(!is.na(date_order_issue)) %>%
      mutate(
        issue_type = "date",
        issue = date_order_issue
      ) %>%
      select(
        row_id,
        issue_type,
        issue,
        any_of(date_required_cols)
      )
  }

  return(date_issues)
}
