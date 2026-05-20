#' Check for operator errors in EFSA data
#'
#' Function for checking EFSA data for incorrect operators
#'
#' @param data Input data
#'
#' @author Håkon Kaspersen, \email{hakon.pedersen.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tibble tibble
#'
check_efsa_operator <- function(data) {
  operator_required_cols <- c(
    "verdi_mengde",
    "range_min",
    "operator",
    "art_gruppe",
    "labIsolCode",
    "bakterie_gruppe",
    "plate_def",
    "substans"
  )

  missing_operator_cols <- setdiff(operator_required_cols, names(data))

  if (length(missing_operator_cols) > 0) {
    warning(
      "Operator check skipped because these columns are missing: ",
      paste(missing_operator_cols, collapse = ", ")
    )

    operator_issues <- tibble()

  } else {
    data_checked <- data %>%
      mutate(
        operator_issue = case_when(
          is.na(verdi_mengde) | is.na(range_min) | is.na(operator) ~
            "Missing value needed for operator check",

          verdi_mengde == range_min & operator != "<=" ~
            "MIC equals range_min, so operator should be '<='",

          verdi_mengde != range_min & operator == "<=" ~
            "MIC does not equal range_min, so operator should not be '<='",

          TRUE ~ NA_character_
        )
      )

    operator_issues <- data_checked %>%
      filter(!is.na(operator_issue)) %>%
      transmute(
        row_id,
        issue_type = "operator",
        issue = operator_issue,
        art_gruppe,
        labIsolCode,
        bakterie_gruppe,
        range_min,
        plate_def,
        substans,
        operator,
        verdi_mengde
      )
  }

  return(operator_issues)
}
