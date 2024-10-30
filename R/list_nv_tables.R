#' List database information
#'
#' This function returns a data frame with information on each table used in the database.
#'
#' @author Håkon Kaspersen \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#'
#'
list_nv_tables <- function() {
  data.frame(
    "rekkefølge" = 1:14,
    "tabellnavn" = c(
      "innsendelse",
      "prove",
      "delprove",
      "undersokelse",
      "resultat",
      "sens_undersokelse",
      "sens_resultat",
      "art_gruppe",
      "materiale_gruppe",
      "bakterie_gruppe",
      "bakterie_kategori",
      "analytt_sens_group",
      "analytt_sens_cutoff",
      "report_sampling_year"
    ),
    "tabellinnhold" = c(
      "Prøveinformasjon, hensikt, datoer",
      "Prøveinformasjon, art, materiale",
      "Prøveinformasjon, delprøvenummer",
      "Metodeinformasjon, hvilken undersøkelse som er gjort",
      "Resultatinformasjon, påvist/ikke påvist, analytt",
      "Metodeinformasjon, metode for sensitivitetstesting",
      "Resultatinformasjon, for sensitivitetstesting, MIC verdier",
      "Gruppeoversikt, liste over artsgrupperingene",
      "Gruppeoversikt, liste over materialegruppene",
      "Gruppeoversikt, liste over bakteriegruppene",
      "Gruppeoversikt, kategorier for bakterier, e.g. Indikator, Viktig, Klinisk, etc.",
      "Gruppeoversikt, liste over substansgruppene",
      "Cutoff-data, liste over alle gjeldende ECOFF verdier",
      "Rapporteringsår for hver gruppe av prøver"
    )
  )
}
