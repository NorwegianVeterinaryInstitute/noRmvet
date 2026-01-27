#' Calculate aggregated resistance occurrences
#'
#' Calculate resistance occurrences at an aggregated antimicrobial group level.
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
#' @import tidyr
#' @importFrom tidyr as_tibble
#' @importFrom stringr str_squish
#' @importFrom tibble add_column
#'
calculate_aggregated_res <- function(data,
                                     bacteria_category = NULL,
                                     material_group = NULL,
                                     bacteria_group = NULL,
                                     species_group = NULL,
                                     reporting_year = NULL) {


  filtered_data <- filter_nv_data_aggregated(
    data,
    bacteria_category = bacteria_category,
    material_group = material_group,
    bacteria_group = bacteria_group,
    species_group = species_group,
    reporting_year = reporting_year
  )

  if (is.null(material_group)) {
    aggregated_res_calc(filtered_data,
                        material_summary = "all")
  } else {
    aggregated_res_calc(filtered_data)
  }
}
