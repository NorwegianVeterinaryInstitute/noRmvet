#' Join EFSA code data to data frame
#'
#' Function for joining EFSA code data to existing data frame
#'
#' @param data Input data
#' @param gene_data If TRUE, will merge with gene-related EFSA data
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
join_efsa_codes <- function(data, gene_data = FALSE) {
  if (isTRUE(gene_data)) {
    data %>%
      left_join(efsa_codes$zoonosis) %>%
      left_join(efsa_codes$sample) %>%
      left_join(efsa_codes$progCode_genes) %>%
      left_join(efsa_codes$esbl_ampc) %>%
      left_join(efsa_codes$seqTech)
  } else {
    data %>%
      left_join(efsa_codes$cutoffs) %>%
      left_join(efsa_codes$substanser) %>%
      left_join(efsa_codes$progCode) %>%
      left_join(efsa_codes$zoonosis) %>%
      left_join(efsa_codes$sample) %>%
      left_join(efsa_codes$mic_verdier) %>%
      mutate(
        cutoffValue = sub("\\.0$", "", as.character(cutoffValue)),
        efsa_range_min = as.character(efsa_range_min),
        efsa_range_min = sub("\\.0$", "", efsa_range_min),
        efsa_range_max = as.character(efsa_range_max),
        efsa_range_max = sub("\\.0$", "", efsa_range_max)
      )
  }
}
