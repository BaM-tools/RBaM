#***************************************************************************----
# Constructor ----
#' Varying parameter object constructor.
#'
#' Creates a new instance of a 'parameter_VAR' object
#'
#' @param name character, parameter name.
#' @param index character, name of column in VAR.indx (see ?dataset) containing the index for this varying parameter
#' @param d dataset object, the dataset containing (amongst other things) the index above
#' @param init numeric vector, initial guesses for each instance of the VAR parameter.
#' @param prior.dist character vector, prior distribution for each instance of the VAR parameter.
#' @param prior.par list of numeric vectors, prior parameters for each instance of the VAR parameter
#' @return An object of class 'parameter_VAR'.
#' @examples
#' X=data.frame(input1=rnorm(100),input2=rnorm(100))
#' Y=data.frame(output=X$input1+0.8*X$input2+0.1*rnorm(100))
#' VAR.indx=data.frame(indx=c(rep(1,50),rep(2,50)))
#' workspace=tempdir()
#' d <- dataset(X=X,Y=Y,data.dir=workspace,VAR.indx=VAR.indx)
#' p <- parameter_VAR(name='par',index='indx',d=d,
#'                    init=c(-1,1,2),
#'                    prior.dist=c('Gaussian','FlatPrior','Triangle'),
#'                    prior.par=list(c(-1,1),NULL,c(2,0,5)))
#' @export
parameter_VAR<-function(name,index,d,init,prior.dist=rep('FlatPrior',length(init)),prior.par=rep(list(NULL),length(init))){
  o<-new_parameter_VAR(name,index,d,init,prior.dist,prior.par)
  return(validate_parameter_VAR(o))
}

#***************************************************************************----
# toString function ----
#' parameter_VAR to string
#'
#' Convert an object of class 'parameter_VAR' into a ready-to-write vector of string
#'
#' @param x parameter_VAR object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' X=data.frame(input1=rnorm(100),input2=rnorm(100))
#' Y=data.frame(output=X$input1+0.8*X$input2+0.1*rnorm(100))
#' VAR.indx=data.frame(indx=c(rep(1,50),rep(2,50)))
#' workspace=tempdir()
#' d <- dataset(X=X,Y=Y,data.dir=workspace,VAR.indx=VAR.indx)
#' p <- parameter_VAR(name='par',index='indx',d=d,
#'                    init=c(-1,1,2),
#'                    prior.dist=c('Gaussian','FlatPrior','Triangle'),
#'                    prior.par=list(c(-1,1),NULL,c(2,0,5)))
#' toString(p)
#' @export
toString.parameter_VAR<-function(x,...){
  col=which(names(x$d$data)==x$index)
  nVal=length(x$init)
  value=list(nVal,col)
  comment=c(
    'nVal: number of distinct values that the parameter can take',
    'col: column in data file giving the index series'
  )
  for (i in 1:nVal){
    value=c(value,paste0(x$name,'_',i),x$init[i],x$prior[[i]]$dist,list(x$prior[[i]]$par))
    comment=c(comment,
      'Parameter name - unused for VARPAR, naming is automatic based on name of parent parameters',
      'Initial guess',
      'Prior distribution - type getCatalogue() in RBaM to see available distributions',
      'Prior parameters'
    )
  }
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' parameter_VAR tester
#'
#' Is an object of class 'parameter_VAR'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'parameter_VAR', FALSE otherwise.
#' @keywords internal
is.parameter_VAR<-function(o){
  return(class(o)=='parameter_VAR')
}

#***************************************************************************----
# internal constructor ----
new_parameter_VAR<-function(name,index,d,init,prior.dist,prior.par){
  stopifnot(is.character(name))
  stopifnot(is.character(index))
  stopifnot(is.dataset(d))
  if(!is.null(init)) stopifnot(is.numeric(init))
  stopifnot(is.character(prior.dist))
  if(!is.null(prior.par)) stopifnot(is.list(prior.par))
  for(i in 1:length(prior.par)){
    if(!is.null(prior.par[[i]])){
      stopifnot(is.numeric(prior.par[[i]]) | is.character(prior.par[[i]]))
    }
  }
  # index column
  col=which(names(d$data)==index)
  if(length(col)==0){
    stop(paste0("index column `",index,"` not found in data frame d"),call.=FALSE)
  }
  # Sizes
  nVal=length(init)
  if(length(prior.dist)!=nVal){
    stop(paste0("size of `prior.dist`=",length(prior.dist)," different from size of `init`=",nVal),call.=FALSE)
  }
  if(length(prior.par)!=nVal){
    stop(paste0("size of `prior.par`=",length(prior.par)," different from size of `init`=",nVal),call.=FALSE)
  }
  if(max(d$data[,col])>nVal){
    stop(paste0("The maximum value found in index column `",index,"` is larger than the size of init/prior.dist/prior.par"),call.=FALSE)
  }
  # create object
  prior=vector(mode='list',length=nVal)
  for (i in 1:nVal){
    prior[[i]]=list(dist=prior.dist[i],par=prior.par[[i]])
  }
  o <- list(name=name,index=index,d=d,init=init,prior=prior)
  class(o) <- 'parameter_VAR'
  return(o)
}

#***************************************************************************----
# validator ----
validate_parameter_VAR<-function(x){
  # 2DO: consider adding checks on prior distribution & parameters,
  #      such as prior.dist is in the catalogue, prior.par has the
  #      correct size, etc. At the moment any error will be caught
  #      by BaM.exe run, not by the R interface.
  return(x)
}

#***************************************************************************----
# write Config_par_VAR.txt ----
writeConfig.parameter_VAR<-function(workspace,fname,x){
  txt=toString(x)
  quickWrite(txt,workspace,fname)
}
