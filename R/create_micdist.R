#' Create MIC distribution tables
#'
#' Script for creating MIC distribution tables from resistance data.
#'
#' @param data The data holding the MIC values and sample information
#' @param bacteria_category Bacterial category to filter on
#' @param material_group Material group to filter on, leave blank for summarizing across material groups
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

  if (is.null(material_group) & is.null(species_group)) {
    stop("Please provide a material group or a species group, or both.")
  }

  filtered_data <- filter_nv_data(
    data,
    bacteria_category = bacteria_category,
    material_group = material_group,
    bacteria_group = bacteria_group,
    species_group = species_group,
    reporting_year = reporting_year
  )

  # Define columns
  cols <- c(
    "0.008" = NA_real_,
    "0.016" = NA_real_,
    "0.03" = NA_real_,
    "0.06" = NA_real_,
    "0.125" = NA_real_,
    "0.25" = NA_real_,
    "0.5" = NA_real_,
    "1" = NA_real_,
    "2" = NA_real_,
    "4" = NA_real_,
    "8" = NA_real_,
    "16" = NA_real_,
    "32" = NA_real_,
    "64" = NA_real_,
    "128" = NA_real_,
    "256" = NA_real_,
    "512" = NA_real_,
    "1024" = NA_real_,
    "2048" = NA_real_
  )

  # No material group, but species group
  if (is.null(material_group) & !is.null(species_group)) {
    micdist <- filtered_data %>%
      select(-c(phenotype,salmonella_materiale,mat_gruppe)) %>%
      group_by_all() %>%
      count() %>%
      group_by(art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               panel,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 1)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      select(
        1:6,
        all_of(names(cols))[names(cols) %in% names(.)]
      )

    calculate_res_occurrence(
      data,
      bacteria_category = bacteria_category,
      material_group = material_group,
      bacteria_group = bacteria_group,
      species_group = species_group,
      reporting_year = reporting_year
    ) %>%
      mutate(CI = paste0("[", lwr, "-", upr, "]")) %>%
      select(-c(lwr, upr)) %>%
      left_join(
        micdist,
        by = c(
          "report_year",
          "art_gruppe",
          "bakterie_gruppe",
          "bakterie_kategori",
          "panel",
          "substans"
        )
      ) %>%
      mutate(substans = factor(substans, levels = substance_order)) %>%
      arrange(art_gruppe, panel, substans)

    # No species group, but material group
  } else if (!is.null(material_group) & is.null(species_group)) {
    micdist <- filtered_data %>%
      select(-c(phenotype,salmonella_materiale,art_gruppe)) %>%
      group_by_all() %>%
      count() %>%
      group_by(mat_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               panel,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 1)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      select(
        1:6,
        all_of(names(cols))[names(cols) %in% names(.)]
      )

    calculate_res_occurrence(
      data,
      bacteria_category = bacteria_category,
      material_group = material_group,
      bacteria_group = bacteria_group,
      species_group = species_group,
      reporting_year = reporting_year
    ) %>%
      mutate(CI = paste0("[", lwr, "-", upr, "]")) %>%
      select(-c(lwr, upr)) %>%
      left_join(
        micdist,
        by = c(
          "report_year",
          "mat_gruppe",
          "bakterie_gruppe",
          "bakterie_kategori",
          "panel",
          "substans"
        )
      ) %>%
      mutate(substans = factor(substans, levels = substance_order)) %>%
      arrange(mat_gruppe, panel, substans)

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
               panel,
               substans) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n / total * 100, 1)) %>%
      select(-c(n, total)) %>%
      arrange(MIC) %>%
      pivot_wider(names_from = "MIC",
                  values_from = "percent") %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      select(
        1:7,
        all_of(names(cols))[names(cols) %in% names(.)]
      )

    calculate_res_occurrence(
      data,
      bacteria_category = bacteria_category,
      material_group = material_group,
      bacteria_group = bacteria_group,
      species_group = species_group,
      reporting_year = reporting_year
    ) %>%
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
          "panel",
          "substans"
        )
      ) %>%
      mutate(substans = factor(substans, levels = substance_order)) %>%
      arrange(art_gruppe, mat_gruppe, panel, substans)
  }
}
