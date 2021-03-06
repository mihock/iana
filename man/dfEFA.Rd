% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemanalysis.R
\name{dfEFA}
\alias{dfEFA}
\title{Degrees of Freedom for a Factor Model}
\usage{
dfEFA(p, m)
}
\arguments{
\item{p}{number of observed variables}

\item{m}{number of factors to be extracted}
}
\value{
degrees of freedom or a negative value.
}
\description{
Returns the degrees of freedom for a exploratory factor model, fit by maximum
likelihood. If the model cannot be fitted because it involves too many
observed variables a negative value is returned.
}
\author{
Michael Hock \email{michael.hock@uni-bamberg.de}
}
