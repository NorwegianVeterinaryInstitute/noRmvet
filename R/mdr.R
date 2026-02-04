#' Calculate multi-drug resistance distributions
#'
#' Calculate the multi-drug resistance distribution, i.e. how many classes of antibiotics there are resistance against
#'
#' @param data The data holding the MIC values and sample information
#' @param bacteria_category Bacterial category to filter on
#' @param material_group Material group to filter on
#' @param bacteria_group Bacteria group to filter on
#' @param species_group Animal species to filter on
#' @param reporting_year Report year to filter on
#' @param substance_dist TRUE/FALSE, default FALSE, Which level of reporting, set to TRUE for percent distribution of each antimicrobial group per MDR category
#' @param food Set to TRUE if calculating MDR for food items
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble add_column
#'
mdr <- function(data,
                bacteria_category = NULL,
                material_group = NULL,
                bacteria_group = NULL,
                species_group = NULL,
                reporting_year = NULL,
                substance_dist = FALSE,
                food = FALSE) {

  # Filter data
  filtered_data <- filter_nv_data(
    data,
    bacteria_category = bacteria_category,
    material_group = material_group,
    bacteria_group = bacteria_group,
    species_group = species_group,
    reporting_year = reporting_year,
    keep_all = TRUE
  )

  calc_mdr(
    filtered_data,
    substance_dist = substance_dist,
    food = food
  )
}
