% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare-SPSS-export.R
\name{defactorize_tsExport_table}
\alias{defactorize_tsExport_table}
\title{Remove factorized variables from tsExport data table.}
\usage{
defactorize_tsExport_table(.data, studydata_name, vallab_lookup)
}
\arguments{
\item{.data}{Data.Frame where the factorized variables should be removed.}

\item{studydata_name}{String. Table name in lookup that relates to the table
that get's labelled.}

\item{vallab_lookup}{List that holds variable label lookup information. It is 
produced by [create_vallab_lookup()].}
}
\value{
Data.Frame without factor variables found in the vallab lookup list.
}
\description{
Remove factor variables that where created during the import
process from data.
}
\keyword{internal}
