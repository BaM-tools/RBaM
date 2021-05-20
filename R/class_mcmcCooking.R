#***************************************************************************----
# Constructor ----
#' mcmcCooking constructor.
#'
#' Creates a new instance of a 'mcmcCooking' object
#'
#' @param fname Character, configuration file name.
#' @param result.fname Character, result file name.
#' @param burn numeric, burn factor, >=0 and <1. 0.4 means the first 40% of
#'     MCMC samples are discarded).
#' @param nSlim Integer, slimming period: 10 means only one MCMC sample
#'     every 10 is kept (after burning).
#' @return An object of class 'mcmcCooking'.
#' @examples
#' m <- mcmcCooking()
#' @export
mcmcCooking<-function(fname="Config_Cooking.txt",result.fname='Results_Cooking.txt',
                      burn=0.5,nSlim=10){
  o<-new_mcmcCooking(fname,result.fname,burn,nSlim)
  return(validate_mcmcCooking(o))
}

#***************************************************************************----
# toString function ----
#' mcmcCooking to string
#'
#' Convert an object of class 'mcmcCooking' into a ready-to-write vector of string
#'
#' @param x mcmcCooking object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(mcmcCooking())
#' @export
toString.mcmcCooking<-function(x,...){
  value=list(x$result.fname,x$burn,x$nSlim)
  comment=c(
    'result.fname, name of MCMC cooking file (|!| name of the file only, not full path)',
    'Burn factor (0.4 mean the first 40% of MCMC samples are burnt)',
    'nSlim, slimming period (10 means only one MCMC sample every 10 is kept (after burning)'
    )
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' mcmcCooking tester
#'
#' Is an object of class 'mcmcCooking'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'mcmcCooking', FALSE otherwise.
#' @keywords internal
is.mcmcCooking<-function(o){
  return(class(o)=='mcmcCooking')
}

#***************************************************************************----
# internal constructor ----
new_mcmcCooking<-function(fname,result.fname,burn,nSlim){
  stopifnot(is.character(fname))
  stopifnot(is.character(result.fname))
  stopifnot(is.numeric(burn))
  stopifnot(is.numeric(nSlim))
  o <- list(fname=fname,result.fname=result.fname,burn=burn,nSlim=nSlim)
  class(o) <- 'mcmcCooking'
  return(o)
}

#***************************************************************************----
# validator ----
validate_mcmcCooking<-function(x){
  # nSlim: > 0
  if(x$nSlim <= 0){stop("`nSlim` should be strictly positive",call.=FALSE)}
  # burn in [0-1)
  if(x$burn < 0 | x$burn >= 1){stop("`burn` should be in [0;1)",call.=FALSE)}
  return(x)
}
