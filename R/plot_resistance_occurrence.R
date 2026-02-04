#' Plot occurrence barplot with confidence intervals
#'
#' Function for creating occurrence plots from the nv data.
#'
#' @param data The data holding the MIC values and sample information
#' @param bacteria_category Bacterial category to filter on
#' @param material_group Material group to filter on
#' @param bacteria_group Bacteria group to filter on
#' @param species_group Animal species to filter on
#' @param reporting_year Report year to filter on
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import ggplot2
#'
plot_resistance_occurrence <- function(data,
                                       bacteria_category = NULL,
                                       material_group = NULL,
                                       bacteria_group = NULL,
                                       species_group = NULL,
                                       reporting_year = NULL) {

  plot_data <- calculate_res_occurrence(
    data,
    bacteria_category = bacteria_category,
    material_group = material_group,
    bacteria_group = bacteria_group,
    species_group = species_group,
    reporting_year = reporting_year
  )

  palette <- list_palettes(group = "species")

  ggplot(plot_data, aes(substans, Percent, fill = art_gruppe)) +
    geom_col(color = "black", position = position_dodge(0.9)) +
    geom_errorbar(aes(ymin = lwr, ymax = upr),
                  width = 0.5,
                  position = position_dodge(0.9)) +
    labs(x = "Substans",
         y = "Prosent (%) resistens",
         fill = "Dyreart") +
    scale_fill_manual(values = palette) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25),
          panel.grid = element_blank()) +
    facet_grid(~report_year)

}
