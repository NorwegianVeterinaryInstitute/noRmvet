#' Calculate resistance occurrences for Salmonella
#'
#' Calculate resistance occurrence and confidence intervals on a substance level for Salmonella.
#'
#' @param data The data holding the MIC values and sample information
#' @param species_summary Use "all" for summarising across all species groups, use "single" for summarising for each separate species
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble add_column
#'
calculate_res_occurrence_salmonella <- function(data,
                                                species_summary = "single") {

  cols <- c(Resistant=NA_real_,Sensitive = NA_real_,ND = NA_real_)

  if (species_summary == "single") {
    data %>%
      filter(salmonella_materiale != "EX") %>%
      select(report_year,
             art_gruppe,
             salmonella_materiale,
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

  } else if (species_summary == "all") {
    data %>%
      filter(salmonella_materiale != "EX") %>%
      select(report_year,
             salmonella_materiale,
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
