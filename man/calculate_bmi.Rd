% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primary-coding.R
\name{calculate_bmi}
\alias{calculate_bmi}
\title{Compute body mass index (BMI)}
\usage{
calculate_bmi(mass, height)
}
\arguments{
\item{mass, height}{A numerical vector with body mass and height data. `mass`
unit must be kilograms and `height` unit must be meters. If the `height`
unit is centimeters, it is converted to meters before BMI computation and
a warning is shown.}
}
\value{
Returns a double vector with the element-wise body mass index (BMI).

bmi(mass, height)
}
\description{
\code{bmi} calculates the BMI in kilograms per meter squared.
}
