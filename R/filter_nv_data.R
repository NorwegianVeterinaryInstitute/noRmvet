#' Filter data based on user input
#'
#' Function for filtering data and throwing error if filtered data is empty.
#'
#' @param data The data holding the MIC values and sample information
#' @param bacteria_category Bacterial category to filter on
#' @param material_group Material group to filter on, leave blank for summarising across material groups
#' @param bacteria_group Bacteria group to filter on
#' @param species_group Animal species to filter on, leave blank to include all species
#' @param reporting_year Report year to filter on
#' @param keep_all Set to TRUE if you want to keep all columns after filtering
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
filter_nv_data <- function(data,
                           bacteria_category = NULL,
                           material_group = NULL,
                           bacteria_group = NULL,
                           species_group = NULL,
                           reporting_year = NULL,
                           keep_all = NULL) {

  if (is.null(material_group)) {
    if (is.null(species_group)) {
      if (is.null(keep_all)) {
        filtered_data <- data %>%
          filter(
            bakterie_kategori %in% bacteria_category,
            bakterie_gruppe %in% bacteria_group,
            report_year %in% reporting_year) %>%
          filter(!is.na(substans),
                 !is.na(MIC),
                 art_gruppe != "EX" | is.na(art_gruppe)) %>%
          select(report_year,
                 art_gruppe,
                 mat_gruppe,
                 salmonella_materiale,
                 bakterie_gruppe,
                 bakterie_kategori,
                 MIC,
                 panel,
                 substans,
                 phenotype)
      } else {
        filtered_data <- data %>%
          filter(
            bakterie_kategori %in% bacteria_category,
            bakterie_gruppe %in% bacteria_group,
            report_year %in% reporting_year) %>%
          filter(!is.na(substans),
                 !is.na(MIC),
                 art_gruppe != "EX" | is.na(art_gruppe))
      }
    } else {
      if (is.null(keep_all)) {
        filtered_data <- data %>%
          filter(
            bakterie_kategori %in% bacteria_category,
            bakterie_gruppe %in% bacteria_group,
            art_gruppe %in% species_group,
            report_year %in% reporting_year) %>%
          filter(!is.na(substans),
                 !is.na(MIC),
                 art_gruppe != "EX" | is.na(art_gruppe)) %>%
          select(report_year,
                 art_gruppe,
                 mat_gruppe,
                 salmonella_materiale,
                 bakterie_gruppe,
                 bakterie_kategori,
                 MIC,
                 panel,
                 substans,
                 phenotype)
      } else {
        filtered_data <- data %>%
          filter(
            bakterie_kategori %in% bacteria_category,
            bakterie_gruppe %in% bacteria_group,
            art_gruppe %in% species_group,
            report_year %in% reporting_year) %>%
          filter(!is.na(substans),
                 !is.na(MIC),
                 art_gruppe != "EX" | is.na(art_gruppe))
      }
    }
  } else {
    if (is.null(species_group)) {
      if (is.null(keep_all)) {
        filtered_data <- data %>%
          filter(
            bakterie_kategori %in% bacteria_category,
            mat_gruppe %in% material_group,
            bakterie_gruppe %in% bacteria_group,
            report_year %in% reporting_year) %>%
          filter(!is.na(substans),
                 !is.na(MIC),
                 art_gruppe != "EX" | is.na(art_gruppe)) %>%
          select(report_year,
                 art_gruppe,
                 mat_gruppe,
                 salmonella_materiale,
                 bakterie_gruppe,
                 bakterie_kategori,
                 MIC,
                 panel,
                 substans,
                 phenotype)
      } else {
        filtered_data <- data %>%
          filter(
            bakterie_kategori %in% bacteria_category,
            mat_gruppe %in% material_group,
            bakterie_gruppe %in% bacteria_group,
            report_year %in% reporting_year) %>%
          filter(!is.na(substans),
                 !is.na(MIC),
                 art_gruppe != "EX" | is.na(art_gruppe))
      }
    } else {
      if (is.null(keep_all)) {
        filtered_data <- data %>%
          filter(
            bakterie_kategori %in% bacteria_category,
            mat_gruppe %in% material_group,
            bakterie_gruppe %in% bacteria_group,
            art_gruppe %in% species_group,
            report_year %in% reporting_year) %>%
          filter(!is.na(substans),
                 !is.na(MIC),
                 art_gruppe != "EX" | is.na(art_gruppe)) %>%
          select(report_year,
                 art_gruppe,
                 mat_gruppe,
                 salmonella_materiale,
                 bakterie_gruppe,
                 bakterie_kategori,
                 MIC,
                 panel,
                 substans,
                 phenotype)
      } else {
        filtered_data <- data %>%
          filter(
            bakterie_kategori %in% bacteria_category,
            mat_gruppe %in% material_group,
            bakterie_gruppe %in% bacteria_group,
            art_gruppe %in% species_group,
            report_year %in% reporting_year) %>%
          filter(!is.na(substans),
                 !is.na(MIC),
                 art_gruppe != "EX" | is.na(art_gruppe))
      }
    }
  }
  if (dim(filtered_data)[1] == 0) {
    stop("No entries found. Please revise filtering groups.", call. = FALSE)
  } else {
    return(filtered_data)
  }
}
