% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_aggregated_res.R
\name{calculate_aggregated_res}
\alias{calculate_aggregated_res}
\title{Calculate aggregated resistance occurrences}
\usage{
calculate_aggregated_res(
  data,
  bacteria_category = NULL,
  material_group = NULL,
  bacteria_group = NULL,
  species_group = NULL,
  reporting_year = NULL
)
}
\arguments{
\item{data}{The data holding the MIC values and sample information}

\item{bacteria_category}{Bacterial category to filter on}

\item{material_group}{Material group to filter on}

\item{bacteria_group}{Bacteria group to filter on}

\item{species_group}{Animal species to filter on}

\item{reporting_year}{Report year to filter on}
}
\description{
Calculate resistance occurrences at an aggregated antimicrobial group level.
}
\author{
Håkon Kaspersen, \email{hakon.kaspersen@vetinst.no}
}
