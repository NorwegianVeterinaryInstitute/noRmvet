#' Colour palettes used in noRmvet
#'
#' This function holds the colour palettes used in the plot functions.
#'
#' @param group Which group palette you want to extract, either "species", "food, "am_groups", or "mdr"
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#'
list_palettes <- function(group = NULL) {
  if (!group %in% c("species", "food", "am_groups", "mdr")) {
    stop("Please use correct palette call.", call. = FALSE)
  }

  if (group == "species") {
    palette <- c(
      "H\u00F8ns" = "#a6cee3",
      "Svin" = "#b2df8a",
      "Storfe" = "#33a02c",
      "Hund" = "#fb9a99",
      "Kalkun" = "#1f78b4",
      "Sau" = "#e31a1c",
      "Hest" = "#fdbf6f",
      "Katt" = "#ff7f00",
      "Villfugl" = "#cab2d6",
      "Villsvin" = "#6a3d9a",
      "Rein" = "#ffff99",
      "R\u00e5dyr" = "#b15928"
    )
  }

  if (group == "food") {
    palette <- c(
      "Ost" = "#ffff33",
      "Skjell" = "#377eb8",
      "Krepsdyr og bløtdyr" = "#984ea3",
      "Meieriprodukter" = "#f781bf",
      "Storfekjøtt" = "#33a02c",
      "Kyllingkjøtt" = "#a6cee3",
      "Kalkunkjøtt" = "#1f78b4",
      "Svinekjøtt" = "#b2df8a",
      "Sauekjøtt" = "#e31a1c",
      "Bladsalat" = "#4daf4a",
      "Halva" = "#999999",
      "Krydderurter" = "#a65628"
    )
  }

  if (group == "am_groups") {
    palette <- c(
      # Aminoglycosides (teal)
      "Aminoglykosider" = "#2A9D8F",
      "Andre aminoglykosider" = "#1F6F66",

      # Quinolones (blue)
      "Kinoloner" = "#4E79A7",

      # Tetracyclines (purple)
      "Tetrasykliner" = "#8E6BBE",

      # Cephalosporins (red family)
      "Cefalosporiner (1. gen)" = "#F2AAA3",
      "Cefalosporiner (2. gen)" = "#E9877B",
      "Cefalosporiner (3. gen)" = "#D65F59",
      "Cefalosporiner (4. gen)" = "#BF4A43",
      "Cefalosporiner (5. gen)" = "#8F2D2A",

      # Other major classes
      "Amfenikoler" = "#5F8FB5",
      "Makrolider/Linkosamider" = "#D9902F",
      "Beta-laktamer/Penicilliner" = "#59A14F",
      "Karbapenemer" = "#F28E2B",
      "Polymyxiner, kolistin*" = "#CC79A7",
      "Sulfonamider og trimetoprim" = "#8DAA3A",

      # Miscellaneous
      "Ionoforer" = "#B8B8B8",
      "Glykopeptider" = "#3A3A3A",

      # Individual agents
      "Bacitracin*" = "#8C613C",
      "Avilamycin*" = "#D4A017",
      "Daptomycin*" = "#E15759",
      "Mupirocin*" = "#7F7F7F",
      "Virginiamycin*" = "#E377C2",
      "Antimycobakterielle" = "#5F6368",
      "Pleuromutiliner" = "#4CB391",
      "Steroider" = "#C44E52"
    )
  }

  if (group == "mdr") {
    palette <- c(
      "0" = "#fef0d9",
      "1" = "#fdcc8a",
      "2" = "#fc8d59",
      "≥3" = "#d7301f"
    )
  }

  return(palette)
}
