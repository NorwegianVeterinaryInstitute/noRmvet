#' Plot occurrence over time lineplot
#'
#' Function for creating line plots with resistance occurrence of each antimicrobial group over time.
#'
#' @param data The data holding the MIC values and sample information
#' @param species_group Animal species to filter on
#' @param am_group Antimicrobial groups to filter on
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import ggplot2
#'
plot_occurrence_over_time <- function(data,
                                      am_group = NULL,
                                      species_group = NULL) {

  if (!is.null(am_group)) {
    plot_data <- data %>%
      filter(analyttkode_gruppe %in% am_group,
             art_gruppe %in% species_group)
  } else {
    plot_data <- data %>%
      filter(art_gruppe %in% species_group)
  }

  palette <- list_palettes(group = "am_groups")

  ggplot(
    plot_data,
    aes(
      report_year,
      Percent,
      color = analyttkode_gruppe,
      group = analyttkode_gruppe
    )
  ) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    labs(x = "\u00C5r",
         y = "Prosent (%) resistens",
         color = "Antibiotikagruppe") +
    scale_color_manual(values = palette) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    facet_wrap(~art_gruppe)
}
