#' Count number of samples
#'
#' Function for calculating the number of samples in the database.
#'
#' @param data Input data
#' @param salmonella If TRUE, will return data on Salmonella samples
#' @param year Year to filter on
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#'
count_samples <- function(data,
                          salmonella = FALSE,
                          year = NULL) {

  if (salmonella == FALSE) {

    df <- data %>%
      filter(resultat != "Undersøkt") %>%
      select(
        report_year,
        art_gruppe,
        mat_gruppe,
        bakterie_gruppe,
        bakterie_kategori,
        resultat
      ) %>%
      filter(bakterie_gruppe != "Salmonella") %>%
      group_by_all() %>%
      count() %>%
      ungroup()

  } else {

    df <- data %>%
      select(
        report_year,
        art_gruppe,
        bakterie_gruppe,
        bakterie_kategori,
        resultat
      ) %>%
      filter(bakterie_gruppe == "Salmonella",
             art_gruppe != "EX",
             resultat == "Påvist") %>%
      group_by_all() %>%
      count() %>%
      ungroup()

  }

  if (!is.null(year)) {
    filter(df, report_year %in% year)
  } else {
    return(df)
  }

}
