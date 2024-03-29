% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primary-coding.R
\name{calculate_brs_sum}
\alias{calculate_brs_sum}
\title{Calculate Brief Resilience Scale (BRS) sum score}
\usage{
calculate_brs_sum(brs_1, brs_2, brs_3, brs_4, brs_5, brs_6)
}
\arguments{
\item{brs_1}{vector for item "I tend to bounce back quickly after hard times.."}

\item{brs_2}{vector for item "I have a hard time making it through stressful events."}

\item{brs_3}{vector for item "It does not take me long to recover from a stressful event."}

\item{brs_4}{vector for item "It is hard for me to snap back when something bad happens."}

\item{brs_5}{vector for item "I usually come through difficult times with little trouble."}

\item{brs_6}{vector for item "I tend to take a long time to get over set-backs in my life."

Answers were coded /w levels "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"}
}
\value{
A numeric vector with sum score of BRS
}
\description{
Calculate BRS sum score

Patients are asked "The following questions are about your resilience to cope with crises. Please respond to each item by marking one box per row."
}
