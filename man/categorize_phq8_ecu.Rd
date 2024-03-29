% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primary-coding.R
\name{categorize_phq8_ecu}
\alias{categorize_phq8_ecu}
\title{Categorize Patient Health Questionnaire Depression Scale 8 (PHQ 8)}
\usage{
categorize_phq8_ecu(ecu_phq8_sum)
}
\arguments{
\item{ecu_phq8_sum}{numeric vector with sum score of phq8}
}
\value{
A factorized vector w/ levels "No depression" and "Depression"
}
\description{
Categorize PHQ-8 sum score in depression and no depression
}
