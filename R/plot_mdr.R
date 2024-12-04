#' Plot MDR
#'
#' Plot multi-drug resistance, i.e. how many classes of antibiotics there are resistance against
#'
#' @param data The data holding the MIC values and sample information
#' @param type Which level of reporting, use "general" for overall MDR, and "am_groups" for percent distribution of each antimicrobial group per MDR category
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import ggplot2
#'
plot_mdr <- function(data,
                     type = "general") {

  if (type == "general") {
    palette <- list_palettes(group = "mdr")

    ggplot(data, aes(Percent, report_year, fill = Resistant)) +
      geom_col(color = "black") +
      scale_fill_manual(values = palette) +
      labs(y = "Reporting year",
           x = "Percent (%) isolates",
           fill = "Number of antimicrobial classes") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 1)) +
      facet_wrap(~art_gruppe, ncol = 1)

  } else if (type == "am_groups_nor") {
    ggplot(data, aes(percent, analyttkode_gruppe, group = analyttkode_gruppe)) +
      geom_line() +
      geom_point(aes(fill = report_year),
                 size = 3,
                 pch = 21) +
      labs(x = "Percent (%) resistant isolates per year",
           fill = "Report year") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.y = element_blank()) +
      facet_grid(art_gruppe~nres)
  }
}
