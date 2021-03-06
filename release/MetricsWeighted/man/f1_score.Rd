% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f1_score.R
\name{f1_score}
\alias{f1_score}
\title{F1 Score}
\usage{
f1_score(actual, predicted, w = NULL, ...)
}
\arguments{
\item{actual}{Observed values (0 or 1).}

\item{predicted}{Predicted values (0 or 1).}

\item{w}{Optional case weights.}

\item{...}{Further arguments passed to \code{precision} and \code{recall}.}
}
\value{
A numeric vector of length one.
}
\description{
Calculates weighted F1 score or F measure defined as the harmonic mean of precision and recall, see \url{https://en.wikipedia.org/wiki/Precision_and_recall} for some background. The higher, the better.
}
\examples{
f1_score(c(0, 0, 1, 1), c(0, 0, 1, 1))
f1_score(c(1, 0, 0, 1), c(0, 0, 1, 1))
f1_score(c(1, 0, 0, 1), c(0, 0, 1, 1), w = 1:4)
}
\seealso{
\code{\link{precision}, \link{recall}}.
}
