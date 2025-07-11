#***************************************************************************----
# Constructor ----
#' parameter object constructor.
#'
#' Creates a new instance of a 'parameter' object
#'
#' @param name character, parameter name.
#' @param init numeric, initial guess.
#' @param prior.dist character, prior distribution.
#' @param prior.par numeric vector, prior parameters
#' @return An object of class 'parameter'.
#' @examples
#' p <- parameter(name='par',init=0,prior.dist='Gaussian',prior.par=c(0,1))
#' @export
parameter<-function(name,init,prior.dist='FlatPrior',prior.par=NULL){
  o<-new_parameter(name,init,prior.dist,prior.par)
  return(validate_parameter(o))
}

#***************************************************************************----
# toString function ----
#' parameter to string
#'
#' Convert an object of class 'parameter' into a ready-to-write vector of string
#'
#' @param x parameter object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' p <- parameter(name='par',init=0,prior.dist='Gaussian',prior.par=c(0,1))
#' toString(p)
#' @export
toString.parameter<-function(x,...){
  value=list(x$name,x$init,x$prior$dist,x$prior$par)
  comment=c(
    'Parameter name',
    'Initial guess',
    'Prior distribution - type getCatalogue() in RBaM to see available distributions',
    'Prior parameters'
  )
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# Utility functions ----

#' Get initial values
#'
#' Get the initial values of a list of parameters and return them as a vector.
#'
#' @param parameters List of parameter objects, parameters whose initial values are sought.
#' @return A numeric vector containing the initial values
#' @examples
#' ps <- list(parameter(name='par1',init=0),parameter(name='par2',init=1))
#' getInitPar(ps)
#' @export
getInitPar <- function(parameters){
  out=sapply(parameters,function(x){x$init})
  return(out)
}

#' Set initial values
#'
#' Set the initial values of a list of parameters.
#'
#' @param parameters List of parameter objects, parameters whose initial values are to be set.
#' @param values Numeric vector, initial values.
#' @return a list of parameter objects equal to the input list 'parameters', except
#'     for their initial values.
#' @examples
#' ps <- list(parameter(name='par1',init=0),parameter(name='par2',init=1))
#' ps=setInitPar(ps,c(10,100))
#' print(ps)
#' @export
setInitPar <- function(parameters,values){
  if(length(parameters)!=length(values)){
    stop("`parameters` and `values` should have the same length",call.=FALSE)
  }
  for(i in 1:length(parameters)){parameters[[i]]$init=values[i]}
  return(parameters)
}

#***************************************************************************----
# is function ----
#' parameter tester
#'
#' Is an object of class 'parameter'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'parameter', FALSE otherwise.
#' @keywords internal
is.parameter<-function(o){
  return(class(o)=='parameter')
}

#***************************************************************************----
# internal constructor ----
new_parameter<-function(name,init,prior.dist,prior.par){
  stopifnot(is.character(name))
  if(!is.null(init)) stopifnot(is.numeric(init))
  stopifnot(is.character(prior.dist))
  if(!is.null(prior.par)) stopifnot(is.numeric(prior.par) | is.character(prior.par))
  o <- list(name=name,init=init,prior=list(dist=prior.dist,par=prior.par))
  class(o) <- 'parameter'
  return(o)
}

#***************************************************************************----
# validator ----
validate_parameter<-function(x){
  # 2DO: consider adding checks on prior distribution & parameters,
  #      such as prior.dist is in the catalogue, prior.par has the
  #      correct size, etc. At the moment any error will be caught
  #      by BaM.exe run, not by the R interface.
  return(x)
}

