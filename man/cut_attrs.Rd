% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_attrs.R
\name{cut_attrs}
\alias{cut_attrs}
\title{Select Attributes by Score Depending on the Cutoff}
\usage{
cut_attrs(attrs, k = 0.5)
}
\arguments{
\item{attrs}{A \link{data.frame} with attributes' importance.}

\item{k}{A numeric. For \code{k >= 1} it takes \code{floor(k)} and then it indicates how many attributes to
take with the highest attribute rank (chooses k best attributes).
For \code{0 < k < 1} it stands for the percent of top attributes to take
(chooses best k * 100\% of attributes).}
}
\description{
Select attributes by their score/rank/weights, depending on the cutoff that may be specified
by the percentage of the highest ranked attributes or by the number of the highest ranked attributes.
}
\examples{

x <- information_gain(Species ~ ., iris)
cut_attrs(attrs = x)
to_formula(cut_attrs(attrs = x), "Species")
cut_attrs(attrs = x, k = 1)
}
\author{
Damian Skrzypiec \email{damian.j.skrzypiec@gmail.com} and
Zygmunt Zawadzki \email{zygmunt@zstat.pl}
}
