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
      "H\u00F8ns" = "#5B8DB8",
      "Svin" = "#7AA974",
      "Storfe" = "#4C8C5A",
      "Hund" = "#B96A6C",
      "Kalkun" = "#3F6F94",
      "Sau" = "#A84A4A",
      "Hest" = "#C89B62",
      "Katt" = "#C7773C",
      "Villfugl" = "#8E7DAF",
      "Villsvin" = "#6A568C",
      "Rein" = "#CDBE75",
      "Rev" = "#B85C87",
      "Geit" = "#1F6F66",
      "R\u00e5dyr" = "#8B6B4E"
    )
  }

  if (group == "food") {
    palette <- c(
      # Seafood (blue family)
      "Skjell" = "#3B82A0",
      "Krepsdyr og bløtdyr" = "#7566A8",

      # Dairy (soft cream/pink)
      "Meieriprodukter" = "#C987A8",
      "Ost" = "#D1B65A",

      # Meat (warm earthy tones)
      "Storfekjøtt" = "#5B8C5A",
      "Svinekjøtt" = "#B07A62",
      "Sauekjøtt" = "#A85454",
      "Kyllingkjøtt" = "#7FA6B8",
      "Kalkunkjøtt" = "#4F7896",

      # Plant products (green family)
      "Bladsalat" = "#4E8B57",
      "Krydderurter" = "#7B6B3A",

      # Other/processed
      "Halva" = "#7A7A7A"
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
      "Beta-laktamer/penicilliner" = "#59A14F",
      "Karbapenemer" = "#F28E2B",
      "Polymykiner, kolistin*" = "#CC79A7",
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
