#' Correct to EFSA MICs and ranges
#'
#' Function for correcting MIC-values and ranges to EFSA standards
#'
#' @param data Input data
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
efsa_mics_and_ranges <- function(data) {
  data %>%
    left_join(panel_ranges) %>%
    mutate(
      range_min = as.character(range_min),
      range_min = sub("\\.0$", "", range_min),
      range_max = as.character(range_max),
      range_max = sub("\\.0$", "", range_max),
      range_min = case_when(
        range_min == "0.06" ~ "0.064",
        range_min == "0.12" ~ "0.125",
        TRUE ~ range_min
      ),
      verdi_mengde = sub(",", ".", verdi_mengde),
      verdi_mengde = case_when(
        verdi_mengde == "0.06" ~ "0.064",
        verdi_mengde == ".5" ~ "0.5",
        verdi_mengde == ".12" ~ "0.125",
        verdi_mengde == ".125" ~ "0.125",
        verdi_mengde == "0.12" ~ "0.125",
        verdi_mengde == "0.120" ~ "0.125",
        verdi_mengde == ".25" ~ "0.25",
        verdi_mengde == ".06" ~ "0.06",
        verdi_mengde == ".03" ~ "0.03",
        TRUE ~ verdi_mengde
      ),
      verdi_mengde = sub("\\.0$", "", verdi_mengde),
      operator = ifelse(operator == "=", "", operator),
      term = paste0(
        operator,
        verdi_mengde
      )
    ) %>%
    select(-MIC)
}
