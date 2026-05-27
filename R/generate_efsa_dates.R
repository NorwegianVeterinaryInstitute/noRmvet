#' Generate EFSA-specific dates for reporting data
#'
#' Function for generating EFSA-specific sampling date, isolation date, and analysis date
#'
#' @param data Input data
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom lubridate as_date
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#'
generate_efsa_dates <- function(data) {
  data %>%
    mutate(
      uttaksdato = case_when(
        !is.na(uttaksdato) ~ as_date(ymd_hms(uttaksdato, truncated = 3)),
        is.na(uttaksdato) ~ as_date(ymd_hms(start_dato, truncated = 3) - 1)
      ),
      sampY = year(uttaksdato),
      sampM = month(uttaksdato),
      sampD = day(uttaksdato),
      start_dato = case_when(
        !is.na(start_dato) ~ as_date(ymd_hms(start_dato, truncated = 3)),
        is.na(start_dato) ~ as_date(ymd_hms(uttaksdato, truncated = 3) + 1)
      ),
      isolY = year(start_dato),
      isolM = month(start_dato),
      isolD = day(start_dato),
      analysedato = case_when(
        !is.na(start_dato) ~ as_date(ymd_hms(start_dato, truncated = 3) + 1),
        is.na(start_dato) ~ as_date(ymd_hms(uttaksdato, truncated = 3) + 2)
      ),
      analysisY = year(analysedato),
      analysisM = month(analysedato),
      analysisD = day(analysedato)
    )
}
