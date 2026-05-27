#' Generate EFSA data
#'
#' Function for generating the main EFSA reporting data
#'
#' @param data Input data
#' @param n_df Data frame holding the total number of samples per combination
#' @param year Relevant year to filter on (only one allowed)
#' @param category The bacterial categories of interest, may be a vector
#' @param bacteria_group The bacterial groups of interest, may be a vector
#' @param animal_species The animal species of interest, may be a vector
#' @param exclude_combinations A data frame with unwanted combinations. The data frame holds three columns: bakterie_kategori, bakterie_gruppe, and art_gruppe
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
generate_efsa_data <- function(
  data,
  n_df,
  year,
  category,
  bacteria_group,
  animal_species,
  exclude_combinations = NULL
) {
  # Filter the initial data frame
  efsa_data <- data %>%
    filter(
      report_year == year,
      bakterie_kategori %in% category,
      bakterie_gruppe %in% bacteria_group,
      art_gruppe %in% animal_species,
      resultat != "Ikke påvist",
      !is.na(substans)
    ) %>%
    mutate(
      labIsolCode = paste(
        substr(aar, 3, 4),
        ansvarlig_seksjon,
        innsendelsesnummer,
        provenummer,
        delprovenummer,
        undersokelsesnummer,
        resultatnummer,
        sep = "-"
      ),
      repYear = aar,
      repCountry = "NO"
    )

  # Remove unwanted combinations from given data frame
  if (!is.null(exclude_combinations)) {
    efsa_data <- efsa_data %>%
      anti_join(
        exclude_combinations,
        by = c("bakterie_kategori", "bakterie_gruppe", "art_gruppe")
      )
  }

  # Correct to EFSA MICs and ranges
  efsa_mics <- efsa_mics_and_ranges(efsa_data)

  # Join EFSA codes to data
  efsa_coded_data <- join_efsa_codes(
    efsa_mics,
    gene_data = FALSE
  )

  efsa_data_dates <- generate_efsa_dates(efsa_coded_data)

  efsa_syn <- syn_test(efsa_data_dates)

  # Merge data and create final variables
  efsa_data_clean <- efsa_data_dates %>%
    left_join(efsa_syn) %>%
    left_join(n_df)

  efsa_checks <- check_efsa_data(
    efsa_data_clean,
    empty_check_cols = mic_empty_cols,
    check_operator = TRUE,
    check_dates = TRUE
  )

  efsa_data_final <- efsa_data_clean %>%
    select(
      repYear,
      repCountry,
      zoonosis,
      matrix,
      sampUnitType,
      sampStage,
      sampOrig,
      sampType,
      sampContext,
      sampler,
      progCode,
      progSampStrategy,
      labIsolCode,
      sampY,
      sampM,
      sampD,
      isolY,
      isolM,
      isolD,
      analysisY,
      analysisM,
      analysisD,
      totUnitsPositive,
      totUnitsTested,
      anMethCode,
      lowest,
      highest,
      substance,
      cutoffValue,
      MIC,
      synTestCTX,
      synTestCAZ
    )

  return(
    list(
      data = efsa_data_final,
      checks = efsa_checks
    )
  )
}
