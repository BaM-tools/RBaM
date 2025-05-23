% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RUI.R
\name{readMCMC}
\alias{readMCMC}
\title{MCMC Reader}
\usage{
readMCMC(
  file = "Results_Cooking.txt",
  burnFactor = 0,
  slimFactor = 1,
  sep = "",
  reportFile = NULL,
  panelPerCol = 10,
  panelHeight = 3,
  panelWidth = 23/panelPerCol
)
}
\arguments{
\item{file}{Character, full path to MCMC file.}

\item{burnFactor}{Numeric, burn factor. 0.1 means the first 10% iterations
are discarded.}

\item{slimFactor}{Integer, slim factor. 10 means that only one iteration
every 10 is kept.}

\item{sep}{Character, separator used in MCMC file.}

\item{reportFile}{Character, full path to pdf report file, not created if NULL}

\item{panelPerCol}{Integer, max number of panels per column}

\item{panelHeight}{Numeric, height of each panel}

\item{panelWidth}{Numeric, width of each panel}
}
\value{
A data frame containing the cooked mcmc samples.
}
\description{
Read raw MCMC samples, return cooked (burnt & sliced) ones
}
\examples{
# Create Monte Carlo samples and write them to file
n=4000
sim=data.frame(p1=rnorm(n),p2=rlnorm(n),p3=runif(n))
workspace=tempdir()
write.table(sim,file=file.path(workspace,'MCMC.txt'),row.names=FALSE)
# Read file, burn the first half and keep every other row
M=readMCMC(file=file.path(workspace,'MCMC.txt'),burnFactor=0.5,slimFactor=2)
dim(M)
}
