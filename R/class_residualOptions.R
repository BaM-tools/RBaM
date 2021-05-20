#***************************************************************************----
# Constructor ----
#' residualOptions constructor.
#'
#' Creates a new instance of a 'residualOptions' object
#'
#' @param fname Character, configuration file name.
#' @param result.fname Character, result file name.
#' @return An object of class 'residualOptions'.
#' @examples
#' r <- residualOptions()
#' @export
residualOptions<-function(fname="Config_Residuals.txt",result.fname='Results_Residuals.txt'){
  o<-new_residualOptions(fname,result.fname)
  return(validate_residualOptions(o))
}

#***************************************************************************----
# toString function ----
#' residualOptions to string
#'
#' Convert an object of class 'residualOptions' into a ready-to-write vector of string
#'
#' @param x residualOptions object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(residualOptions())
#' @export
toString.residualOptions<-function(x,...){
  value=list(x$result.fname)
  comment=c(
    'result.fname, name of residuals file (|!| name of the file only, not full path)')
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' residualOptions tester
#'
#' Is an object of class 'residualOptions'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'residualOptions', FALSE otherwise.
#' @keywords internal
is.residualOptions<-function(o){
  return(class(o)=='residuals')
}

#***************************************************************************----
# internal constructor ----
new_residualOptions<-function(fname,result.fname){
  stopifnot(is.character(fname))
  stopifnot(is.character(result.fname))
  o <- list(fname=fname,result.fname=result.fname)
  class(o) <- 'residualOptions'
  return(o)
}

#***************************************************************************----
# validator ----
validate_residualOptions<-function(x){
  # nothing to do
  return(x)
}
