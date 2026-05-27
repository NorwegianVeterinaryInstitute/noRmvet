#' Fetch gene data from the raw NORM-VET data
#'
#' Run this function to generate a data frame holding all the gene data in the NORM-VET database
#'
#' @param data The data frame from fetch_nv_data
#'
#' @author Håkon Kaspersen \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
create_gene_data <- function(data, year = NULL) {
  data %>%
    filter(! hensiktkode %in% c("06097","0200303","0700109","0800110")) %>%
    filter(
      report_year %in% year
    ) %>%
    select(
      report_year,
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      provenummer,
      delprovenummer,
      undersokelsesnummer,
      resultatnummer,
      uttaksdato,
      start_dato,
      art_gruppe,
      mat_gruppe,
      bakterie_kategori,
      bakterie_gruppe,
      gene_desc,
      gene_method,
      gene,
      resultat
    ) %>%
    distinct() %>%
    filter(!is.na(gene))
}
