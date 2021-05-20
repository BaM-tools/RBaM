#***************************************************************************----
# Constructor ----
#' runOptions constructor.
#'
#' Creates a new instance of a 'runOptions' object
#'
#' @param fname Character, configuration file name.
#' @param doMCMC logical, do MCMC sampling?
#' @param doSummary logical, do MCMC summarizing?
#' @param doResiduals logical, do residuals analysis?
#' @param doPrediction logical, do prediction experiments?
#' @return An object of class 'runOptions'.
#' @examples
#' o <- runOptions()
#' @export
runOptions<-function(fname="Config_RunOptions.txt",doMCMC=TRUE,
                     doSummary=TRUE,doResiduals=TRUE,doPrediction=FALSE){
  o<-new_runOptions(fname,doMCMC,doSummary,doResiduals,doPrediction)
  return(validate_runOptions(o))
}

#***************************************************************************----
# toString function ----
#' runOptions to string
#'
#' Convert an object of class 'runOptions' into a ready-to-write vector of string
#'
#' @param x runOptions object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(runOptions())
#' @export
toString.runOptions<-function(x,...){
  value=list(x$doMCMC,x$doSummary,x$doResiduals,x$doPrediction)
  comment=c('Do MCMC sampling?','Do MCMC summarizing?',
    'Do residual analysis?','Do prediction experiments?')
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' runOptions tester
#'
#' Is an object of class 'runOptions'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'runOptions', FALSE otherwise.
#' @keywords internal
is.runOptions<-function(o){
  return(class(o)=='runOptions')
}

#***************************************************************************----
# internal constructor ----
new_runOptions<-function(fname,doMCMC,doSummary,doResiduals,doPrediction){
  stopifnot(is.character(fname))
  stopifnot(is.logical(doMCMC))
  stopifnot(is.logical(doSummary))
  stopifnot(is.logical(doResiduals))
  stopifnot(is.logical(doPrediction))
  o <- list(fname=fname,doMCMC=doMCMC,doSummary=doSummary,
            doResiduals=doResiduals,doPrediction=doPrediction)
  class(o) <- 'runOptions'
  return(o)
}

#***************************************************************************----
# validator ----
validate_runOptions<-function(x){
  # nothing to do
  return(x)
}
