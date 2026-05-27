#' Write EFSA xml file
#'
#' Function for writing the EFSA xml file
#'
#' @param data Input data
#' @param path Output file path and name
#' @param root Name of root in the xml
#' @param row_tag Name of rows in the xml
#'
#' @author Håkon Kaspersen, \email{hakon.pedersen.kaspersen@@vetinst.no}
#'
#' @export
write_efsa_xml <- function(data, path, root = "dataset", row_tag = "result") {
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  records <- lapply(seq_len(nrow(data)), function(i) {

    vals <- vapply(
      data[i, , drop = FALSE],
      function(x) {
        if (is.na(x)) {
          NA_character_
        } else {
          trimws(as.character(x))
        }
      },
      character(1)
    )

    keep <- !is.na(vals) & vals != ""
    vals <- vals[keep]

    fields <- paste0(
      "<", names(vals), ">",
      xml_escape(vals),
      "</", names(vals), ">"
    )

    paste0(
      "<", row_tag, ">",
      paste(fields, collapse = ""),
      "</", row_tag, ">"
    )
  })

  xml <- paste(
    c(
      '<?xml version="1.0" encoding="UTF-8"?>',
      paste0("<", root, ">"),
      unlist(records, use.names = FALSE),
      paste0("</", root, ">")
    ),
    collapse = "\n"
  )

  cat(xml, file = path, sep = "")
}
