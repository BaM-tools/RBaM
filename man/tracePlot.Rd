% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RUI.R
\name{tracePlot}
\alias{tracePlot}
\title{MCMC reporting}
\usage{
tracePlot(sim, ylab = "values", keep = NULL, col = "black", psize = 0.5)
}
\arguments{
\item{sim}{vector or matrix or data frame, MCMC simulations}

\item{ylab}{Character, label of y-axis to be used if sim has no names}

\item{keep}{Integer vector, indices of samples to be kept in cooked MCMC sample}

\item{col}{Color}

\item{psize}{Numeric, point size}
}
\value{
A ggplot (or a list thereof if several columns in sim)
}
\description{
2DO (adapt from STooDs): Generate pdf report files summarizing mcmc samples
}
\details{
tracePlot

returns a trace plot ggplot (or a list thereof if several columns in sim)
}
\examples{
# Create Monte Carlo samples
n=1000
sim=data.frame(p1=rnorm(n),p2=rlnorm(n),p3=runif(n))
# create trace plot for each component
figures=tracePlot(sim)
}
