#' Filter data based on user input
#'
#' Function for filtering data for aggregated analysis and throwing error if filtered data is empty.
#'
#' @param data The data holding the MIC values and sample information
#' @param bacteria_category Bacterial category to filter on
#' @param material_group Material group to filter on, leave blank for summarising across material groups
#' @param bacteria_group Bacteria group to filter on
#' @param species_group Animal species to filter on, leave blank to include all species
#' @param reporting_year Report year to filter on
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
filter_nv_data_aggregated <- function(data,
                                      bacteria_category = NULL,
                                      material_group = NULL,
                                      bacteria_group = NULL,
                                      species_group = NULL,
                                      reporting_year = NULL) {

  if (is.null(material_group)) {
    if (is.null(species_group)) {
      filtered_data <- data %>%
        select(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               sens_undersokelsesnummer,
               report_year,
               art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               MIC,
               substans,
               phenotype) %>%
        filter(
          bakterie_kategori %in% bacteria_category,
          bakterie_gruppe %in% bacteria_group,
          report_year %in% reporting_year) %>%
        filter(!is.na(substans),
               !is.na(phenotype))
    } else {
      filtered_data <- data %>%
        select(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               sens_undersokelsesnummer,
               report_year,
               art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               MIC,
               substans,
               phenotype) %>%
        filter(
          bakterie_kategori %in% bacteria_category,
          bakterie_gruppe %in% bacteria_group,
          art_gruppe %in% species_group,
          report_year %in% reporting_year) %>%
        filter(!is.na(substans),
               !is.na(phenotype))
    }
  } else {
    if (is.null(species_group)) {
      filtered_data <- data %>%
        select(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               sens_undersokelsesnummer,
               report_year,
               art_gruppe,
               mat_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               MIC,
               substans,
               phenotype) %>%
        filter(
          bakterie_kategori %in% bacteria_category,
          mat_gruppe %in% material_group,
          bakterie_gruppe %in% bacteria_group,
          report_year %in% reporting_year) %>%
        filter(!is.na(substans),
               !is.na(phenotype))
    } else {
      filtered_data <- data %>%
        select(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               sens_undersokelsesnummer,
               report_year,
               art_gruppe,
               mat_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               MIC,
               substans,
               phenotype) %>%
        filter(
          bakterie_kategori %in% bacteria_category,
          mat_gruppe %in% material_group,
          bakterie_gruppe %in% bacteria_group,
          art_gruppe %in% species_group,
          report_year %in% reporting_year) %>%
        filter(!is.na(substans),
               !is.na(phenotype))
    }
  }
  if (dim(filtered_data)[1] == 0) {
    stop("No entries found. Please revise filtering groups.", call. = FALSE)
  } else {
    return(filtered_data)
  }
}
