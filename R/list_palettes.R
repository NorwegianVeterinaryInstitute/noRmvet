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

  if (! group %in% c("species","food","am_groups","mdr")) {
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
      "Aminoglykosider" = "#8dd3c7",
      "Andre aminoglykosider" = "#27665b",
      "Kinoloner" = "#b3cbff",
      "Tetrasykliner" = "#bebada",
      "Cefalosporiner (1. gen)" = "#fb8072",
      "Cefalosporiner (2. gen)" = "#fb8072",
      "Cefalosporiner (3. gen)" = "#fb8072",
      "Cefalosporiner (4. gen)" = "#fb8072",
      "Cefalosporiner (5. gen)" = "#fb8072",
      "Amfenikoler" = "#2f6488",
      "Makrolider/linkosamider" = "#fdb462",
      "Beta-laktamer/penicilliner" = "#b3de69",
      "Karbapenemer" = "#ffed6f",
      "Polymyxiner" = "#bc80bd",
      "Sulfonamider og trimetoprim" = "#ccebc5",
      "Ionoforer" = "#d9d9d9",
      "Glykopeptider" = "black",
      "Bacitracin*" = "#b15928",
      "Avilamycin*" = "#ffff99",
      "Daptomycin*" = "#ff7f00",
      "Mupirocin*" = "#d9d9d9",
      "Virginiamycin*" = "#fccde5",
      "Antimykobakterielle" = "grey40",
      "Pleuromutiliner" = "#8dd3a3",
      "Steroider" = "red"
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
