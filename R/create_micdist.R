#' Create MIC distribution tables
#'
#' Script for creating MIC distribution tables from resistance data.
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
#' @importFrom tidyr pivot_wider
#'
create_micdist <- function(data,
                           bacteria_category = NULL,
                           material_group = NULL,
                           bacteria_group = NULL,
                           species_group = NULL,
                           reporting_year = NULL) {

  if (length(bacteria_group) > 1) {
    stop("Please supply only a single bacterial group.")
  }

  if (bacteria_group == "Salmonella") {
    stop("Please use the create_micdist_salmonella for Salmonella distributions.")
  }

  filtered_data <- filter_nv_data(
    data,
    bacteria_category = bacteria_category,
    material_group = material_group,
    bacteria_group = bacteria_group,
    species_group = species_group,
    reporting_year = reporting_year
    )

  # No material or species group
  if (is.null(material_group) & is.null(species_group)) {
    micdist <- filtered_data %>%
      select(-c(phenotype,salmonella_materiale,mat_gruppe,art_gruppe)) %>%
      group_by_all() %>%
      count() %>%
      group_by(bakterie_gruppe,
               bakterie_kategori,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 2)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      arrange(substans)

    calculate_res_occurrence(filtered_data,
                             material_summary = "all",
                             species_summary = "all") %>%
      mutate(CI = paste0("[", lwr, "-", upr, "]")) %>%
      select(-c(lwr, upr)) %>%
      left_join(
        micdist,
        by = c(
          "report_year",
          "bakterie_gruppe",
          "bakterie_kategori",
          "substans"
        )
      )
    # No material group, but species group
  } else if (is.null(material_group) & !is.null(species_group)) {
    micdist <- filtered_data %>%
      select(-c(phenotype,salmonella_materiale,mat_gruppe)) %>%
      group_by_all() %>%
      count() %>%
      group_by(art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 2)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      arrange(art_gruppe, substans)

    calculate_res_occurrence(filtered_data,
                             material_summary = "all",
                             species_summary = "single") %>%
      mutate(CI = paste0("[", lwr, "-", upr, "]")) %>%
      select(-c(lwr, upr)) %>%
      left_join(
        micdist,
        by = c(
          "report_year",
          "art_gruppe",
          "bakterie_gruppe",
          "bakterie_kategori",
          "substans"
        )
      )
    # No species group, but material group
  } else if (!is.null(material_group) & is.null(species_group)) {
    micdist <- filtered_data %>%
      select(-c(phenotype,salmonella_materiale,art_gruppe)) %>%
      group_by_all() %>%
      count() %>%
      group_by(mat_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 2)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      arrange(mat_gruppe, substans)

    calculate_res_occurrence(filtered_data,
                             material_summary = "single",
                             species_summary = "all") %>%
      mutate(CI = paste0("[", lwr, "-", upr, "]")) %>%
      select(-c(lwr, upr)) %>%
      left_join(
        micdist,
        by = c(
          "report_year",
          "mat_gruppe",
          "bakterie_gruppe",
          "bakterie_kategori",
          "substans"
        )
      )
    # Both species group and material group
  } else if (!is.null(material_group) & !is.null(species_group)) {
    micdist <- filtered_data %>%
      select(-c(phenotype,salmonella_materiale)) %>%
      group_by_all() %>%
      count() %>%
      group_by(art_gruppe,
               mat_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 2)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      arrange(art_gruppe, mat_gruppe, substans)

    calculate_res_occurrence(filtered_data,
                             material_summary = "single",
                             species_summary = "single") %>%
      mutate(CI = paste0("[", lwr, "-", upr, "]")) %>%
      select(-c(lwr, upr)) %>%
      left_join(
        micdist,
        by = c(
          "report_year",
          "art_gruppe",
          "mat_gruppe",
          "bakterie_gruppe",
          "bakterie_kategori",
          "substans"
        )
      )
  }
}
