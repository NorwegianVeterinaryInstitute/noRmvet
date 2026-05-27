#' Determine synergy
#'
#' Function for determining synergy between cefotaxime and cefotaxime/clavulanic acid, and ceftazidime and ceftazidime/clavulanic acid
#'
#' @param data Input data
#'
#' @author Håkon Kaspersen, \email{hakon.pedersen.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
syn_test <- function(data) {
  data %>%
    filter(plate_def == "plate2") %>%
    # clean MIC values
    mutate(
      mic = term %>%
        str_remove("^<=|^>=|^<|^>") %>%
        as.numeric()
    ) %>%
    group_by(
      aar,
      ansvarlig_seksjon,
      innsendelsesnummer,
      provenummer,
      delprovenummer,
      undersokelsesnummer,
      resultatnummer,
      sens_undersokelsesnummer
    ) %>%
    summarise(
      cefotaxim = mic[substans == "Cefotaksim"][1],
      cefotax_clav = mic[substans == "Cefotaksim/Klavulansyre"][1],
      ceftazidim = mic[substans == "Ceftazidim"][1],
      cefta_clav = mic[substans == "Ceftazidim/Klavulansyre"][1],
      .groups = "drop"
    ) %>%
    mutate(
      synTestCTX = case_when(
        cefotaxim / cefotax_clav >= 3 ~ "POS",
        TRUE ~ "NEG"
      ),
      synTestCAZ = case_when(
        ceftazidim / cefta_clav >= 3 ~ "POS",
        TRUE ~ "NEG"
      )
    )
}
