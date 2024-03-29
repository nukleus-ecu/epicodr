% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primary-coding.R
\name{recode_6ils}
\alias{recode_6ils}
\title{Recode Six Item Loneliness Scale (6 ILS) items}
\usage{
recode_6ils(six_ils.factor, version)
}
\arguments{
\item{six_ils.factor}{A factor containing the factorized 6 ILS item that needs recoding}

\item{version}{a vector indication whether item wording was positive or negative}
}
\description{
recodes the 6 ILS items depending on positive or negative wording
items need to be dichotomized
for items 1, 4 and 6, levels 0 = "Not applicable at all" or "Rather does not apply" and 1 = "Rather applies" or "Applies completely" are needed
for items 2, 3 and 5, levels 1 = "Not applicable at all" or "Rather does not apply" and 0 = "Rather applies" or "Applies completely" are needed
function recodes items to needed levels
}
