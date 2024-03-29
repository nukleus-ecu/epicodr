% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-tsExport.R
\name{create_date_lookup}
\alias{create_date_lookup}
\title{Create a list that stores the date variable names per study data table}
\usage{
create_date_lookup(is, study_id)
}
\arguments{
\item{is}{Data.Frame that contains the item information. Usually this is 
named "is".}

\item{study_id}{String. Study ID in secuTrial.}
}
\value{
named list of study data table names and their date variables
}
\description{
Create a list that stores the date variable names per study data table
}
\keyword{internal}
