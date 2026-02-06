#' Create MIC distribution table for Salmonella
#'
#' Script for creating MIC distribution tables from resistance data for Salmonella
#'
#' @param data The data holding the MIC values and sample information
#' @param bacteria_category Bacterial category to filter on
#' @param species_group Animal species to filter on, leave blank to include all species
#' @param reporting_year Report year to filter on
#' @param material Either "Mat" or "Dyr"
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
create_micdist_salmonella <- function(data,
                                      bacteria_category = NULL,
                                      species_group = NULL,
                                      reporting_year = NULL,
                                      material = NULL) {

  filtered_data <- filter_nv_data(
    data,
    bacteria_category = bacteria_category,
    material_group = NULL,
    bacteria_group = "Salmonella",
    species_group = species_group,
    reporting_year = reporting_year
  ) %>%
    filter(salmonella_materiale != "EX") %>%
    filter(salmonella_materiale == material)

  if (is.null(species_group)) {
    micdist <- filtered_data %>%
      select(-c(phenotype, mat_gruppe, art_gruppe)) %>%
      group_by_all() %>%
      count() %>%
      group_by(salmonella_materiale,
               bakterie_gruppe,
               bakterie_kategori,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 1)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      arrange(substans)

    calculate_res_occurrence_salmonella(filtered_data,
                                        species_summary = "all") %>%
      mutate(CI = paste0("[", lwr, "-", upr, "]")) %>%
      select(-c(lwr, upr)) %>%
      left_join(
        micdist,
        by = c(
          "report_year",
          "salmonella_materiale",
          "bakterie_gruppe",
          "bakterie_kategori",
          "substans"
        )
      )
  } else {
    micdist <- filtered_data %>%
      select(-c(phenotype, mat_gruppe)) %>%
      group_by_all() %>%
      count() %>%
      group_by(art_gruppe,
               salmonella_materiale,
               bakterie_gruppe,
               bakterie_kategori,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 1)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      arrange(art_gruppe, substans)

    calculate_res_occurrence_salmonella(filtered_data,
                                        species_summary = "single") %>%
      mutate(CI = paste0("[", lwr, "-", upr, "]")) %>%
      select(-c(lwr, upr)) %>%
      left_join(
        micdist,
        by = c(
          "report_year",
          "art_gruppe",
          "salmonella_materiale",
          "bakterie_gruppe",
          "bakterie_kategori",
          "substans"
        )
      )
  }
}
