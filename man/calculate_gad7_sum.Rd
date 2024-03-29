% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primary-coding.R
\name{calculate_gad7_sum}
\alias{calculate_gad7_sum}
\title{Calculate Generalized Anxiety Disorder 7 (GAD-7) sum score}
\usage{
calculate_gad7_sum(gad7_1, gad7_2, gad7_3, gad7_4, gad7_5, gad7_6, gad7_7)
}
\arguments{
\item{gad7_1}{vector for item "Feeling nervous, anxious or on edge."}

\item{gad7_2}{vector for item "Not being able to stop or control worrying."}

\item{gad7_3}{vector for item "Worrying too much about different things."}

\item{gad7_4}{vector for item "Trouble relaxing."}

\item{gad7_5}{vector for item "Being so restless that it is hard to sit still."}

\item{gad7_6}{vector for item "Becoming easily annoyed or irritable."}

\item{gad7_7}{vector for item "Feeling afraid as if something awful might happen."

Answers were coded /w levels 0 = "Not at all", 1 = "Several days", 2 = "More than half the days" and 3 = "Nearly every day"}
}
\value{
A numeric vector with sum score of GAD-7
}
\description{
Calculate GAD-7 sum score

Patients are asked "Over the last 2 weeks, how often have you been bothered by the following problems?"
}
