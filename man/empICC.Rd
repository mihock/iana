\name{empICC}
\alias{empICC}
\title{Empirical ICCs}
\usage{
  empICC(x, score = c("factor", "mean", "sum"),
    method = loess, alpha = 0, jitter = TRUE)
}
\arguments{
  \item{x}{data frame in which rows are subjects and colums
  are test items}

  \item{score}{total score to use}

  \item{method}{function to use for curve fitting, e.g.
  loess or lm}

  \item{alpha}{opaqueness of the points in the scatterplot}

  \item{jitter}{should jitter be added to the points?}
}
\description{
  Plot of empirical ICCs of the items in a data frame.
}
\details{
  To examine item characteristics, item scores are plotted
  against total scores or factor scores. To avoid
  overplotting, a small amount of jitter is added to
  overlapping points. (These are the light points in the
  plot.) In the upper left corner, the correlation of the
  total/factor score and the item score is given. The lines
  are locally weighted regression lines, the shaded regions
  represent 95\% confidence intervals around the expected
  item scores. If many data points are present it may be
  useful to decrease the opaqueness of the points and/or to
  deactivate jitter.
}
\author{
  Michael Hock \email{michael.hock@uni-bamberg.de}
}
