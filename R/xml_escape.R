#' Xml escape
#'
#' Function for escaping characters in xml output
#'
#' @param x Input data
#'
#' @author Håkon Kaspersen, \email{hakon.pedersen.kaspersen@@vetinst.no}
#'
#' @export
xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}
