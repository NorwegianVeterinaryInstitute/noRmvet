#' Calculate resistance occurrences
#'
#' Calculate resistance occurrence and confidence intervals on a substance level.
#'
#' @param data The data holding the MIC values and sample information
#' @param material_summary Use "all" for summarising across material groups, use "single" for summarising for each separate material
#' @param species_summary Use "all" for summarising across all species groups, use "single" for summarising for each separate species
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble add_column
#'
calculate_res_occurrence <- function(data,
                                     material_summary = "single",
                                     species_summary = "single") {

  cols <- c(Resistant=NA_real_,Sensitive = NA_real_,ND = NA_real_)

  if (material_summary == "single" & species_summary == "single") {
    data %>%
      select(report_year,
             art_gruppe,
             mat_gruppe,
             bakterie_gruppe,
             bakterie_kategori,
             substans,
             phenotype) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      filter(!is.na(phenotype)) %>%
      pivot_wider(names_from = "phenotype",
                  values_from = "n",
                  values_fill = 0) %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      mutate_at(c("Resistant","Sensitive"),
                ~replace_na(., 0)) %>%
      rowwise() %>%
      mutate(Total = Sensitive + Resistant,
             Percent = round(Resistant / Total * 100, 2),
             lwr = round(get_binCI(Resistant, Total)[1],2),
             upr = round(get_binCI(Resistant, Total)[2],2)) %>%
      select(-c(Resistant,Sensitive)) %>%
      mutate(Total = ifelse(!is.na(ND) & ND > 0, ND, Total)) %>%
      select(-ND)

  } else if (material_summary == "single" & species_summary == "all") {
    data %>%
      select(report_year,
             mat_gruppe,
             bakterie_gruppe,
             bakterie_kategori,
             substans,
             phenotype) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      filter(!is.na(phenotype)) %>%
      pivot_wider(names_from = "phenotype",
                  values_from = "n",
                  values_fill = 0) %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      mutate_at(c("Resistant","Sensitive"),
                ~replace_na(., 0)) %>%
      rowwise() %>%
      mutate(Total = Sensitive + Resistant,
             Percent = round(Resistant / Total * 100, 2),
             lwr = round(get_binCI(Resistant, Total)[1],2),
             upr = round(get_binCI(Resistant, Total)[2],2)) %>%
      select(-c(Resistant,Sensitive)) %>%
      mutate(Total = ifelse(!is.na(ND) & ND > 0, ND, Total)) %>%
      select(-ND)

  } else if (material_summary == "all" & species_summary == "single") {
    data %>%
      select(report_year,
             art_gruppe,
             bakterie_gruppe,
             bakterie_kategori,
             substans,
             phenotype) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      filter(!is.na(phenotype)) %>%
      pivot_wider(names_from = "phenotype",
                  values_from = "n",
                  values_fill = 0) %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      mutate_at(c("Resistant","Sensitive"),
                ~replace_na(., 0)) %>%
      rowwise() %>%
      mutate(Total = Sensitive + Resistant,
             Percent = round(Resistant / Total * 100, 2),
             lwr = round(get_binCI(Resistant, Total)[1],2),
             upr = round(get_binCI(Resistant, Total)[2],2)) %>%
      select(-c(Resistant,Sensitive)) %>%
      mutate(Total = ifelse(!is.na(ND) & ND > 0, ND, Total)) %>%
      select(-ND)

  } else if (material_summary == "all" & species_summary == "all") {
    data %>%
      select(report_year,
             bakterie_gruppe,
             bakterie_kategori,
             substans,
             phenotype) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      filter(!is.na(phenotype)) %>%
      pivot_wider(names_from = "phenotype",
                  values_from = "n",
                  values_fill = 0) %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      mutate_at(c("Resistant","Sensitive"),
                ~replace_na(., 0)) %>%
      rowwise() %>%
      mutate(Total = Sensitive + Resistant,
             Percent = round(Resistant / Total * 100, 2),
             lwr = round(get_binCI(Resistant, Total)[1],2),
             upr = round(get_binCI(Resistant, Total)[2],2)) %>%
      select(-c(Resistant,Sensitive)) %>%
      mutate(Total = ifelse(!is.na(ND) & ND > 0, ND, Total)) %>%
      select(-ND)
  }
}
