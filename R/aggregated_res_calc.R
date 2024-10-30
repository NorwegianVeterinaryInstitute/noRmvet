#' Aggregated resistance occurrence calculation
#'
#' Function for calculating aggregated resistance occurrences.
#'
#' @param data Input data
#' @param material_summary Default: "single", Use "all" to summarise across material groups, use "food" for summarising for material groups, ignoring animal species
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble add_column
#'
aggregated_res_calc <- function(data,
                                material_summary = "single") {

  cols <- c(Resistant=NA_real_,Sensitive = NA_real_)

  if (material_summary == "all") {
    df <- data %>%
      select(-MIC) %>%
      left_join(am_groups, by = "substans") %>%
      filter(phenotype != "ND") %>%
      group_by(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               report_year,
               art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               analyttkode_gruppe) %>%
      mutate(res = ifelse(
        any(phenotype == "Resistant"), "Resistant","Sensitive")
      ) %>%
      distinct(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               report_year,
               art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               analyttkode_gruppe,
               .keep_all = TRUE) %>%
      ungroup() %>%
      select(report_year,
             art_gruppe,
             bakterie_gruppe,
             bakterie_kategori,
             analyttkode_gruppe,
             res)
  }

  if (material_summary == "single") {
    df <- data %>%
      select(-MIC) %>%
      left_join(am_groups, by = "substans") %>%
      filter(phenotype != "ND") %>%
      group_by(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               report_year,
               mat_gruppe,
               art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               analyttkode_gruppe) %>%
      mutate(res = ifelse(
        any(phenotype == "Resistant"), "Resistant","Sensitive")
      ) %>%
      distinct(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               report_year,
               mat_gruppe,
               art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               analyttkode_gruppe,
               .keep_all = TRUE) %>%
      ungroup() %>%
      select(report_year,
             art_gruppe,
             mat_gruppe,
             bakterie_gruppe,
             bakterie_kategori,
             analyttkode_gruppe,
             res)
  }

  if (material_summary == "food") {
    df <- data %>%
      select(-MIC) %>%
      left_join(am_groups, by = "substans") %>%
      filter(phenotype != "ND") %>%
      group_by(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               report_year,
               mat_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               analyttkode_gruppe) %>%
      mutate(res = ifelse(
        any(phenotype == "Resistant"), "Resistant","Sensitive")
      ) %>%
      distinct(aar,
               ansvarlig_seksjon,
               innsendelsesnummer,
               provenummer,
               delprovenummer,
               resultatnummer,
               report_year,
               mat_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               analyttkode_gruppe,
               .keep_all = TRUE) %>%
      ungroup() %>%
      select(report_year,
             mat_gruppe,
             bakterie_gruppe,
             bakterie_kategori,
             analyttkode_gruppe,
             res)
  }

  df %>%
    rename("phenotype" = res) %>%
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
           Percent = round(Resistant / Total * 100, 2)) %>%
    ungroup() %>%
    select(-c(Resistant,Sensitive))
}
