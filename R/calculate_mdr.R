#' Calculate MDR
#'
#' Calculate multi-drug resistance, i.e. how many classes of antibiotics there are resistance against
#'
#' @param data The data holding the MIC values and sample information
#' @param type Which level of reporting, leave empty for overall MDR, and "am_groups" for percent distribution of each antimicrobial group per MDR category
#' @param keep_all Were all columns kept in the filter_nv_data function? If so, set this to TRUE
#' @param group Either "animal" or "food", specify food if calculating MDR for food items
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
calculate_mdr <- function(data,
                          type = "general",
                          keep_all = NULL,
                          group = "animal") {

  cols <- c(Resistant=NA_real_,Sensitive = NA_real_)

  if (is.null(keep_all)) {
    am_data <- data %>%
      select(-MIC) %>%
      left_join(am_groups, by = "substans")
  } else {
    am_data <- data %>%
      select(-MIC) %>%
      left_join(am_groups, by = c("substans", "analyttkode_gruppe"))
  }

  if (group == "animal") {
    corrected_data <- am_data %>%
      filter(phenotype != "ND",
             substans != "Narasin") %>%
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
      ungroup()

    if (type == "am_groups") {
      corrected_data %>%
        select(-c(substans,phenotype)) %>%
        group_by(aar,
                 ansvarlig_seksjon,
                 innsendelsesnummer,
                 provenummer,
                 delprovenummer,
                 resultatnummer,
                 report_year,
                 art_gruppe,
                 bakterie_gruppe,
                 bakterie_kategori) %>%
        mutate(nres = sum(res == "Resistant")) %>%
        ungroup() %>%
        mutate(nres = ifelse(nres >= 3, "≥3", nres),
               nres = factor(nres,
                             levels = c("≥3",
                                        "2",
                                        "1",
                                        "0"))) %>%
        filter(res == "Resistant") %>%
        group_by(report_year,
                 art_gruppe,
                 bakterie_gruppe,
                 bakterie_kategori,
                 analyttkode_gruppe,
                 nres) %>%
        count() %>%
        group_by(report_year) %>%
        mutate(total = sum(n),
               percent = n/total*100) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(lwr = round(get_binCI(n, total)[1],2),
               upr = round(get_binCI(n, total)[2],2)) %>%
        ungroup()
    } else if (type == "general") {
      corrected_data %>%
        group_by(
          aar,
          ansvarlig_seksjon,
          innsendelsesnummer,
          provenummer,
          delprovenummer,
          resultatnummer,
          report_year,
          art_gruppe,
          bakterie_gruppe,
          bakterie_kategori,
          res
        ) %>%
        count() %>%
        ungroup() %>%
        pivot_wider(names_from = "res",
                    values_from = "n",
                    values_fill = 0) %>%
        add_column(!!!cols[!names(cols) %in% names(.)]) %>%
        mutate_at(c("Resistant","Sensitive"),
                  ~replace_na(., 0)) %>%
        mutate(
          Resistant = ifelse(Resistant >= 3, "≥3", Resistant),
          Resistant = factor(Resistant,
                             levels = c("≥3",
                                        "2",
                                        "1",
                                        "0"))
        ) %>%
        select(report_year,
               art_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               Resistant) %>%
        group_by_all() %>%
        count() %>%
        group_by(report_year,
                 art_gruppe,
                 bakterie_gruppe,
                 bakterie_kategori) %>%
        mutate(Total = sum(n)) %>%
        ungroup() %>%
        mutate(Percent = round(n / Total * 100, 3)) %>%
        arrange(report_year,-Percent)
    }
  } else if (group == "food") {
    corrected_data <- am_data %>%
      filter(phenotype != "ND",
             substans != "Narasin") %>%
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
      ungroup()

    if (type == "am_groups") {
      corrected_data %>%
        select(-c(substans,phenotype)) %>%
        group_by(aar,
                 ansvarlig_seksjon,
                 innsendelsesnummer,
                 provenummer,
                 delprovenummer,
                 resultatnummer,
                 report_year,
                 mat_gruppe,
                 bakterie_gruppe,
                 bakterie_kategori) %>%
        mutate(nres = sum(res == "Resistant")) %>%
        ungroup() %>%
        mutate(nres = ifelse(nres >= 3, "≥3", nres),
               nres = factor(nres,
                             levels = c("≥3",
                                        "2",
                                        "1",
                                        "0"))) %>%
        filter(res == "Resistant") %>%
        group_by(report_year,
                 mat_gruppe,
                 bakterie_gruppe,
                 bakterie_kategori,
                 analyttkode_gruppe,
                 nres) %>%
        count() %>%
        group_by(report_year) %>%
        mutate(total = sum(n),
               percent = n/total*100) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(lwr = round(get_binCI(n, total)[1],2),
               upr = round(get_binCI(n, total)[2],2)) %>%
        ungroup()
    } else if (type == "general") {
      corrected_data %>%
        group_by(
          aar,
          ansvarlig_seksjon,
          innsendelsesnummer,
          provenummer,
          delprovenummer,
          resultatnummer,
          report_year,
          mat_gruppe,
          bakterie_gruppe,
          bakterie_kategori,
          res
        ) %>%
        count() %>%
        ungroup() %>%
        pivot_wider(names_from = "res",
                    values_from = "n",
                    values_fill = 0) %>%
        add_column(!!!cols[!names(cols) %in% names(.)]) %>%
        mutate_at(c("Resistant","Sensitive"),
                  ~replace_na(., 0)) %>%
        mutate(
          Resistant = ifelse(Resistant >= 3, "≥3", Resistant),
          Resistant = factor(Resistant,
                             levels = c("≥3",
                                        "2",
                                        "1",
                                        "0"))
        ) %>%
        select(report_year,
               mat_gruppe,
               bakterie_gruppe,
               bakterie_kategori,
               Resistant) %>%
        group_by_all() %>%
        count() %>%
        group_by(report_year,
                 mat_gruppe,
                 bakterie_gruppe,
                 bakterie_kategori) %>%
        mutate(Total = sum(n)) %>%
        ungroup() %>%
        mutate(Percent = round(n / Total * 100, 3)) %>%
        arrange(report_year,-Percent)
    }
  }
}






