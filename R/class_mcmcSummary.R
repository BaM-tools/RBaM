#***************************************************************************----
# Constructor ----
#' mcmcSummary constructor.
#'
#' Creates a new instance of a 'mcmcSummary' object
#'
#' @param fname Character, configuration file name.
#' @param result.fname Character, result file name.
#' @return An object of class 'mcmcSummary'.
#' @examples
#' m <- mcmcSummary()
#' @export
mcmcSummary<-function(fname="Config_Summary.txt",result.fname='Results_Summary.txt'){
  o<-new_mcmcSummary(fname,result.fname)
  return(validate_mcmcSummary(o))
}

#***************************************************************************----
# toString function ----
#' mcmcSummary to string
#'
#' Convert an object of class 'mcmcSummary' into a ready-to-write vector of string
#'
#' @param x mcmcSummary object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(mcmcSummary())
#' @export
toString.mcmcSummary<-function(x,...){
  value=list(x$result.fname)
  comment=c(
    'result.fname, name of MCMC summary file (|!| name of the file only, not full path)')
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' mcmcSummary tester
#'
#' Is an object of class 'mcmcSummary'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'mcmcSummary', FALSE otherwise.
#' @keywords internal
is.mcmcSummary<-function(o){
  return(class(o)=='mcmcSummary')
}

#***************************************************************************----
# internal constructor ----
new_mcmcSummary<-function(fname,result.fname){
  stopifnot(is.character(fname))
  stopifnot(is.character(result.fname))
  o <- list(fname=fname,result.fname=result.fname)
  class(o) <- 'mcmcSummary'
  return(o)
}

#***************************************************************************----
# validator ----
validate_mcmcSummary<-function(x){
  # nothing to do
  return(x)
}
