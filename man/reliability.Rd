\name{reliability}
\alias{reliability}
\title{Reliability}
\usage{
  reliability(x, invert = TRUE, digits = 3, dfname = NULL)
}
\arguments{
  \item{x}{a numeric data frame in which rows represent
  persons and columns represent items to be analyzed}

  \item{invert}{logical specifying whether items with
  negative loadings on the first principal component of the
  data should be inverted (default: TRUE)}

  \item{digits}{number of digits to use in the output}

  \item{dfname}{name of the data frame (only needed by
  \code{iana})}
}
\description{
  Performs classical item and reliability analysis of set
  of items.
}
\details{
  \code{reliability} checks if items have negative loadings
  on the first principal component of the data and, if so,
  inverts these items.
}
\author{
  Michael Hock \email{michael.hock@uni-bamberg.de}
}

