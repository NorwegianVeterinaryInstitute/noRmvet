#' Check for missing values in EFSA data
#'
#' Function for identifying missing values in EFSA data columns
#'
#' @param data Input data
#' @param empty_check_cols A vector of column names of interest
#'
#' @author Håkon Kaspersen, \email{hakon.pedersen.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
check_efsa_empty_values <- function(data, empty_check_cols) {
  missing_empty_check_cols <- setdiff(empty_check_cols, names(data))

  if (length(missing_empty_check_cols) > 0) {
    warning(
      "These empty-check columns are missing from the data and will be skipped: ",
      paste(missing_empty_check_cols, collapse = ", ")
    )
  }

  empty_check_cols <- intersect(empty_check_cols, names(data))

  empty_issues <- data %>%
    select(row_id, any_of(empty_check_cols)) %>%
    pivot_longer(
      cols = -row_id,
      names_to = "column",
      values_to = "value",
      values_transform = list(value = as.character)
    ) %>%
    mutate(
      value = trimws(value),
      is_empty = is.na(value) | value == ""
    ) %>%
    filter(is_empty) %>%
    transmute(
      row_id,
      issue_type = "empty_value",
      issue = paste0("Empty value in required column '", column, "'"),
      column,
      value
    )

  return(empty_issues)
}
