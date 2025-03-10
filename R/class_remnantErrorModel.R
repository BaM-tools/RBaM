#***************************************************************************----
# Constructor ----
#' remnantErrorModel object constructor.
#'
#' Creates a new instance of a 'remnantErrorModel' object
#'
#' @param fname Character, configuration file name.
#' @param funk Character, function f used in remnant sdev = f(Ysim).
#'   Available: 'Constant', 'Proportional', 'Linear' (default), 'Exponential', 'Gaussian'.
#' @param par list of parameter objects, parameters of the function above.
#'   respectively, npar= 1,1,2,3,3
#' @return An object of class 'remnantErrorModel'.
#' @examples
#' r <- remnantErrorModel()
#' @export
remnantErrorModel<-function(fname='Config_RemnantSigma.txt',funk='Linear',
                            par=list(parameter('g1',1,prior.dist='FlatPrior+'),
                                     parameter('g2',0.1,prior.dist='FlatPrior+'))
                  ){
  o<-new_remnantErrorModel(fname,funk,par)
  return(validate_remnantErrorModel(o))
}

#***************************************************************************----
# toString function ----
#' remnantErrorModel to string
#'
#' Convert an object of class 'remnantErrorModel' into a ready-to-write vector of string
#'
#' @param x remnantErrorModel object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(remnantErrorModel())
#' @export
toString.remnantErrorModel<-function(x,...){
  npar=length(x$par)
  value=list(x$funk,npar)
  comment=c('Function f used in sdev=f(Ysim) - available: Constant, Linear, Exponential,Gaussian - resp. npar: 1,2,3,3',
            'Number of parameters gamma for the function f above')
  if(npar>0){
    for(i in 1:npar){
      p=x$par[[i]]
      if(is.null(p$prior$par)){param=''} else {param=list(p$prior$par)}
      value=c(value,p$name,p$init,p$prior$dist,param)
      comment=c(comment,
        'Parameter name',
        'Initial guess',
        'Prior distribution - type getCatalogue() in RBaM to see available distributions',
        'Prior parameters')
    }
  }
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' remnantErrorModel tester
#'
#' Is an object of class 'remnantErrorModel'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'remnantErrorModel', FALSE otherwise.
#' @keywords internal
is.remnantErrorModel<-function(o){
  return(class(o)=='remnantErrorModel')
}

#***************************************************************************----
# internal constructor ----
new_remnantErrorModel<-function(fname,funk,par){
  stopifnot(is.character(fname))
  stopifnot(is.character(funk))
  stopifnot(is.list(par))
  npar=length(par)
  if(npar>0){
    for(i in 1:npar){
      stopifnot(is.parameter(par[[i]]))
    }
  }
  o <- list(fname=fname,funk=funk,par=par)
  class(o) <- 'remnantErrorModel'
  return(o)
}

#***************************************************************************----
# validator ----
validate_remnantErrorModel<-function(x){
  # 2DO: consider adding checks on remnant function & parameters,
  #      such as funk is in the catalogue, par has the
  #      correct size, etc. At the moment any error will be caught
  #      by BaM.exe run, not by the R interface.
  return(x)
}

