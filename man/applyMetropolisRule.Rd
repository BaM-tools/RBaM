% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCMC.R
\name{applyMetropolisRule}
\alias{applyMetropolisRule}
\title{Metropolis Rule}
\usage{
applyMetropolisRule(x, candid, fx, fcandid)
}
\arguments{
\item{x}{numeric vector, current sample.}

\item{candid}{numeric vector, candidate sample.}

\item{fx}{data frame, a 1-row data frame with fields logPosterior, logPrior and
logLikelihood evaluated for the current sample.}

\item{fcandid}{data frame, a 1-row data frame with fields logPosterior, logPrior and
logLikelihood evaluated for the candidate sample.}
}
\value{
A list with fields x (resulting sample), fx (resulting target) and move (did the chain move?).
}
\description{
Application of Metropolis rule.
}
\keyword{internal}
