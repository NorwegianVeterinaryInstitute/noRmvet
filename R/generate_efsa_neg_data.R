#' Generate EFSA data for Negative data model
#'
#' Function for generating EFSA data for the negative data model
#'
#' @param data Input data
#' @param year Relevant year to filter on (only one allowed)
#' @param bacteria_group The bacterial groups of interest, may be a vector
#' @param animal_species The animal species of interest, may be a vector
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
generate_efsa_neg_data <- function(data, year, bacteria_group, animal_species) {
  count_samples(
    data,
    year = year
  ) %>%
    filter(
      bakterie_gruppe %in% bacteria_group,
      art_gruppe %in% animal_species,
      Påvist == 0
    ) %>%
    rename(
      "totUnitTested" = Total,
      "totUnitsPositive" = Påvist,
      "repYear" = report_year
    ) %>%
    mutate(
      resultCode = as.character(1:n()),
      repCountry = "NO"
    ) %>%
    left_join(efsa_codes$progCodeNeg) %>%
    left_join(efsa_codes$zoonosis) %>%
    left_join(efsa_codes$sample) %>%
    mutate(
      recId = paste(
        repYear,
        repCountry,
        art_gruppe,
        mat_gruppe,
        bakterie_gruppe,
        sep = "-"
      )
    ) %>%
    select(
      repCountry,
      zoonosis,
      matrix,
      sampUnitType,
      sampStage,
      sampOrig,
      sampType,
      sampContext,
      sampler,
      progSampStrategy,
      recId,
      totUnitTested,
      totUnitsPositive,
      repYear,
      progCode
    )
}
