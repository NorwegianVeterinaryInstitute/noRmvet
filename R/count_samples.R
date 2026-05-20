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

  cols <- c(Påvist=NA_real_,`Ikke påvist` = NA_real_)

  if (salmonella == FALSE) {

    df <- data %>%
      filter(bakterie_gruppe != "Salmonella",
             report_year %in% year) %>%
      select(
        report_year,
        ansvarlig_seksjon,
        innsendelsesnummer,
        provenummer,
        delprovenummer,
        undersokelsesnummer,
        resultatnummer,
        art_gruppe,
        mat_gruppe,
        salmonella_materiale,
        bakterie_kategori,
        bakterie_gruppe,
        plate_def,
        resultat
      ) %>%
      distinct() %>%
      select(
        report_year,
        art_gruppe,
        mat_gruppe,
        salmonella_materiale,
        bakterie_kategori,
        bakterie_gruppe,
        plate_def,
        resultat
      ) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      pivot_wider(
        names_from = "resultat",
        values_from = "n",
        values_fill = 0
      ) %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      mutate_at(c("Påvist","Ikke påvist"),
                ~replace_na(., 0)) %>%
      mutate(
        Total = Påvist + `Ikke påvist`
      )

  } else {

    df <- data %>%
      filter(bakterie_gruppe == "Salmonella",
             art_gruppe != "EX",
             report_year %in% year) %>%
      select(
        report_year,
        ansvarlig_seksjon,
        innsendelsesnummer,
        provenummer,
        delprovenummer,
        undersokelsesnummer,
        resultatnummer,
        art_gruppe,
        mat_gruppe,
        salmonella_materiale,
        bakterie_kategori,
        bakterie_gruppe,
        plate_def,
        resultat
      ) %>%
      distinct() %>%
      select(
        report_year,
        art_gruppe,
        mat_gruppe,
        salmonella_materiale,
        bakterie_kategori,
        bakterie_gruppe,
        plate_def,
        resultat
      ) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      pivot_wider(
        names_from = "resultat",
        values_from = "n",
        values_fill = 0
      ) %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      mutate_at(c("Påvist","Ikke påvist"),
                ~replace_na(., 0)) %>%
      mutate(
        Total = Påvist + `Ikke påvist`
      )
  }

    return(df)
}
