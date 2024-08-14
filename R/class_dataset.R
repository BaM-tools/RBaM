#***************************************************************************----
# Constructor ----
#' dataset object constructor.
#'
#' Creates a new instance of a 'dataset' object
#'
#' @param X data frame, observed input variables.
#' @param Y data frame, observed output variables (same number of rows as X).
#' @param data.dir Character, directory where a copy of the dataset will be
#'     written if required. Default is the current working directory, but you
#'     may prefer to use the BaM workspace.
#' @param data.fname Character, data file name.
#' @param fname Character, configuration file name.
#' @param Xu data frame, random uncertainty in X, expressed as a standard deviation.
#'     Same dimension as X.
#' @param Xb data frame, systematic uncertainty in X, expressed as a standard deviation.
#'     Same dimension as X.
#' @param Xb.indx data frame, index of systematic errors in X.
#'     Same dimension as X.
#' @param Yu data frame, random uncertainty in Y, expressed as a standard deviation.
#'     Same dimension as Y.
#' @param Yb data frame, systematic uncertainty in Y, expressed as a standard deviation.
#'     Same dimension as Y.
#' @param Yb.indx data frame, index of systematic errors in Y.
#'     Same dimension as Y.
#' @param VAR.indx data frame, indices used for defining how VAR parameters vary.
#' @return An object of class 'dataset'.
#' @examples
#' X=data.frame(input1=rnorm(100),input2=rnorm(100))
#' Y=data.frame(output=X$input1+0.8*X$input2+0.1*rnorm(100))
#' d <- dataset(X=X,Y=Y,data.dir=getwd())
#' @export
dataset<-function(X,Y,data.dir=getwd(),data.fname='CalibrationData.txt',
                  fname="Config_Data.txt",
                  Xu=NULL,Xb=NULL,Xb.indx=NULL,
                  Yu=NULL,Yb=NULL,Yb.indx=NULL,
                  VAR.indx=NULL){
  o<-new_dataset(X,Y,data.dir,data.fname,fname,Xu,Xb,Xb.indx,Yu,Yb,Yb.indx,VAR.indx)
  return(validate_dataset(o))
}

#***************************************************************************----
# toString function ----
#' dataset to string
#'
#' Convert an object of class 'dataset' into a ready-to-write vector of string
#'
#' @param x dataset object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' X=data.frame(input1=rnorm(100),input2=rnorm(100))
#' Y=data.frame(output=X$input1+0.8*X$input2+0.1*rnorm(100))
#' d <- dataset(X=X,Y=Y,data.dir=getwd())
#' toString(d)
#' @export
toString.dataset<-function(x,...){
  value=list(x$data.file,1,nrow(x$data),ncol(x$data),
             x$col.X,x$col.Xu,x$col.Xb,x$col.Xbindx,
             x$col.Y,x$col.Yu,x$col.Yb,x$col.Ybindx)
  comment=c(
    'Full path to data file',
    'number of header lines',
    'Nobs, number of rows in data file (excluding header lines)',
    'number of columns in data file',
    'columns for X (observed inputs) in data file - comma-separated if several',
    'columns for Xu (random uncertainty in X, EXPRESSED AS A STANDARD DEVIATION - use 0 for a no-error assumption)',
    'columns for Xb (systematic uncertainty in X, EXPRESSED AS A STANDARD DEVIATION - use 0 for a no-error assumption)',
    'columns for Xb_indx (index of systematic errors in X - use 0 for a no-error assumption)',
    'columns for Y (observed outputs) in data file - comma-separated if several',
    'columns for Yu (random uncertainty in Y, EXPRESSED AS A STANDARD DEVIATION - use 0 for a no-error assumption)',
    'columns for Yb (systematic uncertainty in Y, EXPRESSED AS A STANDARD DEVIATION - use 0 for a no-error assumption)',
    'columns for Yb_indx (index of systematic errors in Y - use 0 for a no-error assumption)'
  )
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' dataset tester
#'
#' Is an object of class 'dataset'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'dataset', FALSE otherwise.
#' @keywords internal
is.dataset<-function(o){
  return(class(o)=='dataset')
}

#***************************************************************************----
# internal constructor ----
new_dataset<-function(X,Y,data.dir,data.fname,fname,Xu,Xb,Xb.indx,Yu,Yb,Yb.indx,VAR.indx){
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # basic checks
  stopifnot(is.data.frame(X))
  stopifnot(is.data.frame(Y))
  stopifnot(is.character(data.dir))
  stopifnot(is.character(data.fname))
  stopifnot(is.character(fname))
  if(!is.null(Xu)) stopifnot(is.data.frame(Xu))
  if(!is.null(Xb)) stopifnot(is.data.frame(Xb))
  if(!is.null(Xb.indx)) stopifnot(is.data.frame(Xb.indx))
  if(!is.null(Yu)) stopifnot(is.data.frame(Yu))
  if(!is.null(Yb)) stopifnot(is.data.frame(Yb))
  if(!is.null(Yb.indx)) stopifnot(is.data.frame(Yb.indx))
  if(!is.null(VAR.indx)) stopifnot(is.data.frame(VAR.indx))
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data file
  data.file=file.path(R.utils::getAbsolutePath(data.dir),data.fname)
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Inputs
  k=0;n=NROW(X)
  W=X
  col.X=(k+1):(k+NCOL(X))
  k=k+NCOL(X)
  # input random uncertainty
  if(is.null(Xu)){
    col.Xu=rep(0,NCOL(X))
  } else {
    if(NROW(Xu)!=n | NCOL(Xu)!=NCOL(X)){
      stop("`Xu` should have the same dimensions (nrow,ncol) as `X`",call.=FALSE)
    }
    W=cbind(W,Xu)
    col.Xu=(k+1):(k+NCOL(Xu))
    k=k+NCOL(Xu)
  }
  # input systematic uncertainty
  if(is.null(Xb)){
    col.Xb=rep(0,NCOL(X))
  } else {
    if(NROW(Xb)!=n | NCOL(Xb)!=NCOL(X)){
      stop("`Xb` should have the same dimensions (nrow,ncol) as `X`",call.=FALSE)
    }
    W=cbind(W,Xb)
    col.Xb=(k+1):(k+NCOL(Xb))
    k=k+NCOL(Xb)
  }
  # input index of systematic errors
  if(is.null(Xb.indx)){
    col.Xb.indx=rep(0,NCOL(X))
  } else {
    if(NROW(Xb.indx)!=n | NCOL(Xb.indx)!=NCOL(X)){
      stop("`Xb.indx` should have the same dimensions (nrow,ncol) as `X`",call.=FALSE)
    }
    if(any(Xb.indx%%1!=0)){
      stop("Xb.indx data frame contains non-integer values",call.=FALSE)
    }
    if(any(Xb.indx<=0)){
      stop("Xb.indx data frame contains negative values",call.=FALSE)
    }
    W=cbind(W,Xb.indx)
    col.Xb.indx=(k+1):(k+NCOL(Xb.indx))
    k=k+NCOL(Xb.indx)
  }
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Outputs
  if(NROW(Y)!=n){
    stop("`Y` should have the same number of rows as `X`",call.=FALSE)
  }
  W=cbind(W,Y)
  col.Y=(k+1):(k+NCOL(Y))
  k=k+NCOL(Y)
  # output random uncertainty
  if(is.null(Yu)){
    col.Yu=rep(0,NCOL(Y))
  } else {
    if(NROW(Yu)!=n | NCOL(Yu)!=NCOL(Y)){
      stop("`Yu` should have the same dimensions (nrow,ncol) as `Y`",call.=FALSE)
    }
    W=cbind(W,Yu)
    col.Yu=(k+1):(k+NCOL(Yu))
    k=k+NCOL(Yu)
  }
  # output systematic uncertainty
  if(is.null(Yb)){
    col.Yb=rep(0,NCOL(Y))
  } else {
    if(NROW(Yb)!=n | NCOL(Yb)!=NCOL(Y)){
      stop("`Yb` should have the same dimensions (nrow,ncol) as `Y`",call.=FALSE)
    }
    W=cbind(W,Yb)
    col.Yb=(k+1):(k+NCOL(Yb))
    k=k+NCOL(Yb)
  }
  # output index of systematic errors
  if(is.null(Yb.indx)){
    col.Yb.indx=rep(0,NCOL(Y))
  } else {
    if(NROW(Yb.indx)!=n | NCOL(Yb.indx)!=NCOL(Y)){
      stop("`Yb.indx` should have the same dimensions (nrow,ncol) as `Y`",call.=FALSE)
    }
    if(any(Yb.indx%%1!=0)){
      stop("Yb.indx data frame contains non-integer values",call.=FALSE)
    }
    if(any(Yb.indx<=0)){
      stop("Yb.indx data frame contains negative values",call.=FALSE)
    }
    W=cbind(W,Yb.indx)
    col.Yb.indx=(k+1):(k+NCOL(Yb.indx))
    k=k+NCOL(Yb.indx)
  }
  # index for VAR parameters
  if(is.null(VAR.indx)){
    col.VAR.indx=0
  } else {
    if(NROW(VAR.indx)!=n){
      stop("`VAR.indx` should have the same number of rows as `X` and `Y`",call.=FALSE)
    }
    if(any(VAR.indx%%1!=0)){
      stop("VAR.indx data frame contains non-integer values",call.=FALSE)
    }
    if(any(VAR.indx<=0)){
      stop("VAR.indx data frame contains negative values",call.=FALSE)
    }
    W=cbind(W,VAR.indx)
    col.VAR.indx=(k+1):(k+NCOL(VAR.indx))
    k=k+NCOL(VAR.indx)
  }
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assemble dataset object
  o <- list(fname=fname,data.file=data.file,
            col.X=col.X,col.Xu=col.Xu,col.Xb=col.Xb,col.Xbindx=col.Xb.indx,
            col.Y=col.Y,col.Yu=col.Yu,col.Yb=col.Yb,col.Ybindx=col.Yb.indx,
            col.VAR.indx=col.VAR.indx,data=W)
  class(o) <- 'dataset'
  return(o)
}

#***************************************************************************----
# validator ----
validate_dataset<-function(x){
  # nothing to do - checks were done in the constructor
  return(x)
}

