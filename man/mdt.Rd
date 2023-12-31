% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdt.R
\name{mdt}
\alias{mdt}
\title{The multivariate Student t Distribution}
\usage{
mdt(X, v = 1)
}
\arguments{
\item{X}{Vector of quantiles.}

\item{df}{degrees of freedom (>0).}
}
\value{
Numeric value.
}
\description{
Returns density for the m-variate t distribution with 'df' degrees of freedom, for a vector of length 'm'.
}
\examples{
mdt(c(1,1), 100)
mdt(1)
}
