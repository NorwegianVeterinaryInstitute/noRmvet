#' Generate EFSA ESBL gene data
#'
#' Function for generating the ESBL gene EFSA reporting data
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
generate_efsa_gene_data <- function(
  data,
  n_df,
  year,
  category,
  bacteria_group,
  animal_species,
  material,
  exclude_combinations = NULL,
  excluded_genes = NULL
) {
  # Create the initial gene data
  efsa_gene_data <- create_gene_data(data, year = year) %>%
    filter(
      bakterie_kategori %in% category,
      art_gruppe %in% animal_species,
      mat_gruppe %in% material
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
      repCountry = "NO",
    )

  # Remove unwanted combinations from given data frame
  if (!is.null(exclude_combinations)) {
    efsa_gene_data <- efsa_gene_data %>%
      anti_join(
        exclude_combinations,
        by = c("bakterie_kategori", "bakterie_gruppe", "art_gruppe")
      )
  }

  # Remove unwanted genes from given data frame
  if (!is.null(excluded_genes)) {
    efsa_gene_data <- efsa_gene_data %>%
      filter(
        !gene %in% excluded_genes
      )
  }

  # Join EFSA codes to data
  efsa_coded_gene_data <- join_efsa_codes(
    efsa_gene_data,
    gene_data = TRUE
  )

  # Generate date data
  efsa_gene_data_dates <- generate_efsa_dates(efsa_coded_gene_data)

  # Merge data and create final variables
  efsa_gene_data_clean <- efsa_gene_data_dates %>%
    left_join(n_df)

  # Check for missing data and errors
  checks <- check_efsa_data(
    data = efsa_gene_data_clean,
    empty_check_cols = gene_empty_cols,
    check_operator = FALSE,
    check_dates = TRUE
  )

  efsa_gene_data_final <- efsa_gene_data_clean %>%
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
      seqTech,
      esbl,
      ampC
    )

  return(
    list(
      data = efsa_gene_data_final,
      checks = checks
    )
  )
}
