#***************************************************************************----
# Constructor ----
#' model object constructor.
#'
#' Creates a new instance of a 'model' object
#'
#' @param fname Character, configuration file name.
#' @param ID Character, model ID. Type 'getCatalogue()' for available models.
#' @param nX Integer, number of input variables.
#' @param nY Integer, number of output variables.
#' @param par list of parameter objects, parameters of the model.
#' @param xtra xtraModelInfo object.
#' @return An object of class 'model'.
#' @examples
#' # defaut linear regression model Y=aX+b
#' mod <- model()
#' # BaRatin model for a single-control rating curve Y=a(X-b)^c
#' mod <- model(ID='BaRatin',nX=1,nY=1,
#'              par=list(parameter('a',10,prior.dist='LogNormal',prior.par=c(log(10),0.1)),
#'                       parameter('b',-1,prior.dist='Gaussian',prior.par=c(-1,1)),
#'                       parameter('c',5/3,prior.dist='Gaussian',prior.par=c(5/3,0.05))),
#'              xtra=xtraModelInfo(object=matrix(1,nrow=1,ncol=1)))
#' @export
model<-function(fname='Config_Model.txt',ID='Linear',
                nX=1,nY=1,
                par=list(parameter('Xeffect',1,prior.dist='FlatPrior')),
                xtra=xtraModelInfo()
){
  o<-new_model(fname,ID,nX,nY,par,xtra)
  return(validate_model(o))
}

#***************************************************************************----
# toString function ----
#' model to string
#'
#' Convert an object of class 'model' into a ready-to-write vector of string
#'
#' @param x model object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(model())
#' @export
toString.model<-function(x,...){
  npar=length(x$par)
  value=list(x$ID,x$nX,x$nY,npar)
  comment=c('Model ID - type getCatalogue() in RBaM to see available models',
            'nX: number of input variables',
            'nY: number of output variables',
            'nPar: number of parameters theta')
  if(npar>0){
    for(i in 1:npar){
      p=x$par[[i]]
      if(is.null(p$prior$par)){param=''} else {param=list(p$prior$par)}
      value=c(value,p$name,p$init,p$prior$dist,param)
      comment=c(comment,
                c('Parameter name',
                  'Initial guess',
                  'Prior distribution - type getCatalogue() in RBaM to see available distributions',
                  'Prior parameters')
      )
    }
  }
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' model tester
#'
#' Is an object of class 'model'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'model', FALSE otherwise.
#' @keywords internal
is.model<-function(o){
  return(class(o)=='model')
}

#***************************************************************************----
# internal constructor ----
new_model<-function(fname,ID,nX,nY,par,xtra){
  stopifnot(is.character(fname))
  stopifnot(is.character(ID))
  stopifnot(is.numeric(nX))
  stopifnot(is.numeric(nY))
  stopifnot(is.list(par))
  npar=length(par)
  if(npar>0){
    for(i in 1:npar){
      stopifnot(is.parameter(par[[i]]))
    }
  }
  stopifnot(is.xtraModelInfo(xtra))
  o <- list(fname=fname,ID=ID,nX=nX,nY=nY,par=par,xtra=xtra)
  class(o) <- 'model'
  return(o)
}

#***************************************************************************----
# validator ----
validate_model<-function(x){
  # nX and nY: >= 0
  if(x$nX < 0){stop("`nX` should be positive",call.=FALSE)}
  if(x$nY < 0){stop("`nY` should be positive",call.=FALSE)}
  # model ID is in the catalogue
  if(!(x$ID %in% getCatalogue()$models)){stop("model `ID` does not exist in BaM catalogue",call.=FALSE)}
  return(x)
}
