#' Evolution of two populations
#'
#' Size of two populations of the same species put in two different environments,
#' as a function of time and local temperature.
#' \itemize{
#'   \item Input variables: time t, temperature at site 1 T1, temperature at site 2 T2.
#'   \item Output variables: population size at site 1 P1, population size at site 2 P2.
#'   \item Data are synthetically generated from a logistic model.
#' }
"twoPopulations"

#' Sauze Gaugings
#'
#' Stage-discharge gaugings from the hydrometric station 'the Ardèche River at Sauze-St-Martin'.
#' See https://en.wikipedia.org/wiki/Ardèche_(river) for a description of the river
#' See https://hal.science/hal-00934237 for an article using this dataset
#'
#' @format A data frame with 38 rows and 3 variables:
#' \describe{
#'   \item{H}{Stage (m)}
#'   \item{Q}{Discharge (m3/s)}
#'   \item{uQ}{Discharge uncertainty (m3/s) expressed as a standard deviation}
#' }
"SauzeGaugings"

#' Meyras Gaugings
#'
#' Stage-discharge gaugings from the hydrometric station 'the Ardèche River at Meyras'.
#' See https://en.wikipedia.org/wiki/Ardèche_(river) for a description of the river
#' See https://doi.org/10.1029/2018WR023389 for an article using this dataset
#'
#' @format A data frame with 104 rows and 4 variables:
#' \describe{
#'   \item{h}{Stage (m)}
#'   \item{Q}{Discharge (m3/s)}
#'   \item{uQ}{Discharge uncertainty (m3/s) expressed as a standard deviation}
#'   \item{Period}{Stability period on which a single rating curve can be used}
#' }
"MeyrasGaugings"
