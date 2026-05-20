#' Check EFSA data for errors
#'
#' Function for checking prepared EFSA data for submission to the EFSA system.
#'
#' @param data Input data
#' @param empty_check_cols Vector with column names to check for empty values
#' @param check_operator If TRUE, will check for incorrect operators in the data
#' @param check_dates If TRUE, will check for incorrect dates in the data
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
check_efsa_data <- function(data,
                            empty_check_cols,
                            check_operator = TRUE,
                            check_dates = TRUE) {

  data_checked <- data %>%
    mutate(row_id = row_number())

  # -------------------------
  # Operator check
  # -------------------------
  if (check_operator) {
    operator_issues <- check_efsa_operator(data_checked)
  } else {
    operator_issues <- tibble()
  }

  # -------------------------
  # Date check
  # -------------------------
  if (check_dates) {
    date_issues <- check_efsa_dates(data_checked)
  } else {
    date_issues <- tibble()
  }

  # -------------------------
  # Empty-value check
  # -------------------------
  empty_issues <- check_efsa_empty_values(
    data_checked,
    empty_check_cols
    )

  # -------------------------
  # Combined output
  # -------------------------
  all_issues <- bind_rows(
    operator_issues,
    date_issues,
    empty_issues
  )

  summary <- all_issues %>%
    count(issue_type, issue, name = "n") %>%
    arrange(issue_type, desc(n))

  list(
    summary = summary,
    issues = all_issues,
    operator_issues = operator_issues,
    date_issues = date_issues,
    empty_issues = empty_issues
  )
}
