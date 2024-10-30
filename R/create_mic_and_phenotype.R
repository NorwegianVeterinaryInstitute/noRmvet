#' Clean up data from NORM-VET database
#'
#' Run this function to generate corrected MIC values and phenotype definitions, as well as removing redundant columns.
#'
#' @param data The data frame from fetch_nv_data
#' @param retain_plate2 Set to TRUE if you do not wish to filter out plate2 results (020185)
#'
#' @author Håkon Kaspersen \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
create_mic_and_phenotype <- function(data, retain_plate2 = FALSE) {

  data <- data %>%
    filter(metodekode_sens != "020007") %>%
    mutate(verdi_mengde = sub(",", "\\.", verdi_mengde),
           verdi_mengde = as.numeric(verdi_mengde),
           cut_off = as.numeric(cut_off)) %>%
    mutate(
      ## If the operator from the sensititre plate is
      ## ">", it means that the actual MIC value is
      ## doubled.
      MIC = case_when(
        operator == ">" ~ verdi_mengde * 2, TRUE ~ verdi_mengde
      ),
      ## Here the actual phenotype is defined from the
      ## relevant MIC values
      phenotype = case_when(
        is.na(cut_off) ~ "ND",
        MIC > cut_off ~ "Resistant",
        TRUE ~ "Sensitive"
      )
    ) %>%
    select(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      provenummer,
      delprovenummer,
      resultatnummer,
      sens_undersokelsesnummer,
      report_year,
      art_gruppe,
      materialenavn,
      mat_gruppe,
      salmonella_materiale,
      bakterie_kategori,
      bakterie_gruppe,
      resultat,
      metodekode_sens,
      cut_off_gruppe,
      cut_off,
      substans,
      analyttkode_gruppe,
      MIC,
      phenotype
    )

  if (retain_plate2 != TRUE) {
    data <- data %>%
      filter(metodekode_sens != "020185",
             substans != "Cefepim") %>%
      select(-metodekode_sens)

    return(data)

  } else {
    data <- data %>%
      select(-metodekode_sens)

    return(data)
  }

}

