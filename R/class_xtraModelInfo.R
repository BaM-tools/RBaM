#***************************************************************************----
# Constructor ----
#' xtraModelInfo constructor.
#'
#' Creates a new instance of a 'xtraModelInfo' object containing extra model information
#'
#' @param fname Character, configuration file name.
#' @param object any R object containing xtra model info - typically a list of stuff.
#'   The content and meaning of 'object' is completely model-specific.
#' @return An object of class 'xtraModelInfo'.
#' @examples
#' x <- xtraModelInfo()
#' @export
xtraModelInfo<-function(fname="Config_Xtra.txt",object=NULL){
  o<-new_xtraModelInfo(fname,object)
  return(validate_xtraModelInfo(o))
}

#***************************************************************************----
# is function ----
#' xtraModelInfo tester
#'
#' Is an object of class 'xtraModelInfo'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'xtraModelInfo', FALSE otherwise.
#' @keywords internal
is.xtraModelInfo<-function(o){
  return(class(o)=='xtraModelInfo')
}

#***************************************************************************----
# internal constructor ----
new_xtraModelInfo<-function(fname,object){
  stopifnot(is.character(fname))
  o <- list(fname=fname,object=object)
  class(o) <- 'xtraModelInfo'
  return(o)
}

#***************************************************************************----
# validator ----
validate_xtraModelInfo<-function(x){
  # nothing to do
  return(x)
}
