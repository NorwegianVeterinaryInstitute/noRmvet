#' Calculate resistance occurrences
#'
#' Calculate resistance occurrence and confidence intervals on a substance level.
#'
#' @param data The data holding the MIC values and sample information
#' @param bacteria_category Bacterial category to filter on
#' @param material_group Material group to filter on
#' @param bacteria_group Bacteria group to filter on
#' @param species_group Animal species to filter on
#' @param reporting_year Report year to filter on
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble add_column
#'
calculate_res_occurrence <- function(data,
                                     bacteria_category = NULL,
                                     material_group = NULL,
                                     bacteria_group = NULL,
                                     species_group = NULL,
                                     reporting_year = NULL) {

  filtered_data <- filter_nv_data(
    data,
    bacteria_category = bacteria_category,
    material_group = material_group,
    bacteria_group = bacteria_group,
    species_group = species_group,
    reporting_year = reporting_year
  )

  if (is.null(material_group) & !is.null(species_group)) {
    res_calc(filtered_data, material_summary = "all")
  } else if (!is.null(material_group) & is.null(species_group)) {
    res_calc(filtered_data, material_summary = "food")
  } else if (!is.null(material_group) & !is.null(species_group)) {
    res_calc(filtered_data, material_summary = "single")
  }
}
