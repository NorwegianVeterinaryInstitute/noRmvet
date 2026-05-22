#' Check for duplicates in animal NORM-VET data
#'
#' Function for checking for duplicates in the NORM-VET animal data, based on producer
#'
#' @param data Input data
#' @param animal_species The animal species of interest, can be a vector
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
check_duplicated_animal_samples <- function(
  data,
  year,
  animal_species
) {
  data %>%
    filter(report_year == year) %>%
    mutate(
      sak = paste(
        aar,
        ansvarlig_seksjon,
        innsendelsesnummer,
        provenummer,
        delprovenummer,
        sep = "-"
      )
    ) %>%
    select(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      sak,
      mottatt_dato,
      hensiktkode,
      eier_lokalitetnr,
      eier_lokalitetstype,
      art_gruppe
    ) %>%
    filter(hensiktkode == "0200301") %>%
    distinct() %>%
    filter(art_gruppe %in% animal_species, ansvarlig_seksjon == "01") %>%
    group_by(art_gruppe, eier_lokalitetnr) %>%
    mutate(antall = n()) %>%
    ungroup() %>%
    filter(antall > 1) %>%
    arrange(eier_lokalitetnr, as.numeric(innsendelsesnummer)) %>%
    mutate(
      dupl = duplicated(eier_lokalitetnr)
    ) %>%
    group_by(eier_lokalitetnr) %>%
    filter(any(dupl)) %>%
    ungroup()
}
