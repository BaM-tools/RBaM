#***************************************************************************----
# Constructor ----
#' mcmcOptions object constructor.
#'
#' Creates a new instance of a 'mcmcOptions' object
#'
#' @param fname Character, configuration file name.
#' @param result.fname Character, result file name.
#' @param nAdapt Integer, adaptation period: jump sizes are increased/decreased
#'     every Nadapt iterations to comply with the desired moving rates.
#' @param nCycles Integer, number of adaptation cycles
#'    (total number of iterations is hence Nadapt * Ncycles).
#' @param minMoveRate Numeric in (0;1), lower bound for the desired move rate interval.
#' @param maxMoveRate Numeric in (0;1), upper bound for the desired move rate interval.
#' @param downMult Numeric in (0:1), multiplication factor used to decrease jump size
#'     when move rate is too low.
#' @param upMult Numeric (>1, avoid 1/dowMult) multiplication factor used to increase
#'     jump size when move rate is too high.
#' @param multFactor Numeric >0, multiplicative factor to set initial jump standard deviations
#'    to multFactor*|initValue| (AUTO mode).
#' @param manualMode logical, should jump standard deviations be entered manually?
#' @param thetaStd Numeric vector (>0), jump standard deviations for model parameters theta (MANUAL mode).
#' @param gammaStd list of numeric vectors (>0), size = number of output variables of the model.
#'     Jump standard deviations for structural error parameters gamma of each output variable (MANUAL mode).
#' @return An object of class 'mcmcOptions'.
#' @examples
#' m <- mcmcOptions()
#' @export
mcmcOptions<-function(fname="Config_MCMC.txt",result.fname='Results_MCMC.txt',
               nAdapt=100,nCycles=100,
               minMoveRate=0.1,maxMoveRate=0.5,
               downMult=0.9,upMult=1.1,
               multFactor=0.1,manualMode=FALSE,
               thetaStd=9999,gammaStd=list(9999)){
  o<-new_mcmcOptions(fname,result.fname,nAdapt,nCycles,minMoveRate,maxMoveRate,downMult,upMult,multFactor,
                     manualMode,thetaStd,gammaStd)
  return(validate_mcmcOptions(o))
}

#***************************************************************************----
# toString function ----
#' mcmcOptions to string
#'
#' Convert an object of class 'mcmcOptions' into a ready-to-write vector of string
#'
#' @param x mcmcOptions object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(mcmcOptions())
#' @export
toString.mcmcOptions<-function(x,...){
  value=list(x$result.fname,x$nAdapt,x$nCycles,
             x$minMoveRate,x$maxMoveRate,x$downMult,x$upMult,
             ifelse(x$manualMode,1,0), # Mode for setting the initial Std of the jump distribution - 0=auto
             '****    DEFINITION OF INITIAL JUMP STD   ****', # just a cosmetics line
             x$multFactor, # multiplicative factor in auto mode
             x$thetaStd)# jump stdevs for theta in manual mode
  comment=c(
    'result.fname, name of MCMC file (|!| name of the file only, not full path)',
    'nAdapt, adaptation period: jump sizes are increased/decreased every Nadapt iterations to comply with the desired moving rates',
    'nCycles, no more adaptation after iteration number stopAdapt',
    'minMoveRate, lower bound for the desired move rate interval',
    'maxMoveRate, upper bound for the desired move rate interval',
    'downMult, (<1) multiplication factor used to decrease jump size when move rate is too low',
    'upMult, (>1, avoid 1/dowMult) multiplication factor used to increase jump size when move rate is too high',
    'Mode for setting the initial Std of the jump distribution - 0=auto[recommended]',
    'Cosmetic line, unused',
    'multFactor, multiplicative factor to set initial jump standard deviations to multFactor*|initValue| [AUTO mode]',
    'Jump standard deviations for model parameters theta [MANUAL mode]')
  for(i in 1:length(x$gammaStd)){
    value=c(value,x$gammaStd[i])
    comment=c(comment,paste('Jump standard deviations for structural error parameters gamma of output variable',i,'[MANUAL mode]'))
  }
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' mcmcOptions tester
#'
#' Is an object of class 'mcmcOptions'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'mcmcOptions', FALSE otherwise.
#' @keywords internal
is.mcmcOptions<-function(o){
  return(class(o)=='mcmcOptions')
}

#***************************************************************************----
# internal constructor ----
new_mcmcOptions<-function(fname,result.fname,nAdapt,nCycles,minMoveRate,maxMoveRate,
                          downMult,upMult,multFactor,manualMode,thetaStd,gammaStd){
  stopifnot(is.character(fname))
  stopifnot(is.character(result.fname))
  stopifnot(is.numeric(nAdapt))
  stopifnot(is.numeric(nCycles))
  stopifnot(is.numeric(minMoveRate))
  stopifnot(is.numeric(maxMoveRate))
  stopifnot(is.numeric(downMult))
  stopifnot(is.numeric(upMult))
  stopifnot(is.numeric(multFactor))
  stopifnot(is.logical(manualMode))
  stopifnot(is.numeric(thetaStd))
  stopifnot(is.list(gammaStd))
  o <- list(fname=fname,result.fname=result.fname,nAdapt=nAdapt,nCycles=nCycles,
            minMoveRate=minMoveRate,maxMoveRate=maxMoveRate,
            downMult=downMult,upMult=upMult,multFactor=multFactor,
            manualMode=manualMode,thetaStd=thetaStd,gammaStd=gammaStd)
  class(o) <- 'mcmcOptions'
  return(o)
}

#***************************************************************************----
# validator ----
validate_mcmcOptions<-function(x){
  # nAdapt: > 0
  if(x$nAdapt <= 0){stop("`nAdapt` should be strictly positive",call.=FALSE)}
  # nCycles: > 0
  if(x$nCycles <= 0){stop("`nCycles` should be strictly positive",call.=FALSE)}
  # minMoveRate: in 0-1
  if(x$minMoveRate < 0 | x$minMoveRate > 1){stop("`minMoveRate` should be in [0;1]",call.=FALSE)}
  # maxMoveRate: in 0-1 and >= minMoveRate
  if(x$maxMoveRate < 0 | x$maxMoveRate > 1){stop("`maxMoveRate` should be in [0;1]",call.=FALSE)}
  if(x$maxMoveRate < x$minMoveRate){stop("`maxMoveRate` should be larger than `minMoveRate`",call.=FALSE)}
  # downMult: in (0-1]
  if(x$downMult <= 0 | x$downMult > 1){stop("`downMult` should be in (0;1]",call.=FALSE)}
  # upMult: >=1 , > downMult, avoid 1/downmult
  if(x$upMult < 1){stop("`upMult` should be larger than 1",call.=FALSE)}
  if(x$upMult <= x$downMult){stop("`upMult` should be larger than `downMult`",call.=FALSE)}
  if(x$upMult == 1/x$downMult){warning("avoid `upMult` = 1/`downMult` (oscillating adaptation)",call.=TRUE)}
  # multFactor: >0
  if(x$multFactor <= 0 ){stop("`multFactor` should be strictly positive",call.=FALSE)}
  # thetaStd: >0
  if( any(x$thetaStd <= 0) ){stop("`thetaStd` should be all be strictly positive",call.=FALSE)}
  # gammaStd: >0
  for(i in 1:length(gammaStd)){
    if( any(x$gammaStd[[i]] <= 0) ){stop("`gammaStd` should be all be strictly positive",call.=FALSE)}
  }
  return(x)
}

