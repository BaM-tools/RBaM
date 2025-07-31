#***************************************************************************----
# Log-prior functions  ----

#*******************************************************************************
#' Log-prior function: improper flat prior
#'
#' Computes the log-density of an improper flat prior distribution.
#'
#' @param parvector numeric vector, parameter vector, including thetas (model parameters)
#'     and gammas (structural errors parameters).
#' @return A numeric value equal to the prior log-density.
#' @examples
#' logPrior_Flat(c(1,1,0.2))
#' @export
logPrior_Flat <- function(parvector){
  return(0.)
}

#***************************************************************************----
# Log-likelihood functions  ----

#*******************************************************************************
#' Log-likelihood function: iid Gaussian
#'
#' Computes the log-likelihood from model-simulated values, based on a Gaussian iid error model:
#' \itemize{
#'     \item Yobs = Ysim + 𝛿 + 𝜀
#'     \item Measurement errors: 𝛿~ N(0,sdev=Yu)
#'     \item Structural errors: 𝜀~ N(0,sdev=𝛾)
#' }
#' If Yobs/Ysim are multi-variate, this error model is applied independently to each component.
#'
#' @param Ysim data frame, model-simulated values.
#' @param Yobs data frame, corresponding observed values, same dimensions as Ysim. NAs are skipped.
#' @param Yu data frame, measurement uncertainties (standard deviations), same dimensions as Ysim and Yobs.
#' @param gamma numeric vector, structural error parameters. length(gamma) = number of columns in Ysim.
#' @return A numeric value equal to the log-likelihood.
#' @examples
#' Yobs=SauzeGaugings['Q']
#' Yu=SauzeGaugings['uQ']
#' Ysim=100*(SauzeGaugings['H']+0.5)^1.6
#' llfunk_iid_Gaussian(Ysim,Yobs,Yu,gamma=100)
#' @export
#' @importFrom stats dnorm
llfunk_iid_Gaussian <- function(Ysim,Yobs,Yu,gamma){
  p=NCOL(Ysim)
  out=0
  for(i in 1:p){
    ps=stats::dnorm(x=Yobs[,i],mean=Ysim[,i],sd=sqrt(gamma[i]^2+Yu[,i]^2),log=TRUE)
    out=out+sum(ps,na.rm=TRUE)
  }
  return(out)
}

#*******************************************************************************
#' Log-likelihood function: independent-linear Gaussian
#'
#' Computes the log-likelihood from model-simulated values based on a Gaussian independent
#' error model with linearly-varying standard deviation:
#' \itemize{
#'     \item Yobs = Ysim + 𝛿 + 𝜀
#'     \item Measurement errors: 𝛿~ N(0,sdev=Yu)
#'     \item Structural errors: 𝜀~ N(0,sdev=g1+g2*|Ysim|)
#' }
#' If Yobs/Ysim are multi-variate, this error model is applied independently to each component.
#'
#' @param Ysim data frame, model-simulated values.
#' @param Yobs data frame, corresponding observed values, same dimensions as Ysim. NAs are skipped.
#' @param Yu data frame, measurement uncertainties (standard deviations), same dimensions as Ysim and Yobs.
#' @param gamma numeric vector, structural error parameters, organized as:
#'     gamma=c((g1,g2) for the 1st component of Ysim,(g1,g2) for the 2nd component of Ysim, etc.)
#'     => length(gamma) = 2*(number of columns in Ysim).
#' @return A numeric value equal to the log-likelihood.
#' @examples
#' Yobs=SauzeGaugings['Q']
#' Yu=SauzeGaugings['uQ']
#' Ysim=100*(SauzeGaugings['H']+0.5)^1.6
#' llfunk_iLinear_Gaussian(Ysim,Yobs,Yu,gamma=c(1,0.1))
#' @export
#' @importFrom stats dnorm
llfunk_iLinear_Gaussian <- function(Ysim,Yobs,Yu,gamma){
  p=NCOL(Ysim)
  out=0
  for(i in 1:p){
    g0=gamma[2*i-1];g1=gamma[2*i]
    m=Ysim[,i]
    s=g0+g1*abs(m)
    ps=dnorm(x=Yobs[,i],mean=m,sd=sqrt(s^2+Yu[,i]^2),log=TRUE)
    out=out+sum(ps,na.rm=TRUE)
  }
  return(out)
}

# Generic log-likelihood function for a model

#*******************************************************************************
#' BaM log-likelihood
#'
#' Log-likelihood engine for a model available in BaM. Unlike functions llfunk_***,
#' which compute the log-likelihood from model-simulated values Ysim (see e.g. \link[RBaM]{llfunk_iid_Gaussian}),
#' this function computes the log-likelihood from (parameters + inputs X + model mod).
#'
#' @param parvector numeric vector, parameter vector, including thetas (model parameters)
#'     and gammas (structural errors parameters).
#' @param X data frame, model inputs.
#' @param Yobs data frame, corresponding observed values.
#' @param Yu data frame, measurement uncertainties (standard deviations), same dimensions as Yobs.
#' @param llfunk function, function computing the log-likelihood given Ysim, see e.g. \link[RBaM]{llfunk_iid_Gaussian}.
#' @param mod model object, the model to be calibrated.
#' @param Ysim data frame, model-simulated values. When NULL (default), the model is run to provide simulations.
#'     When a non-NULL data frame is provided, it is used as pre-computed simulations, and the model is
#'     hence not run. This is useful to speed-up some MCMC strategies.
#' @param llargs object, any other arguments to be passed to llfunk.
#' @return A list with the following components:
#'     \item{logLikelihood}{numeric, the log-likelihood.}
#'     \item{Ysim}{data frame, the model-simulated values.}
#' @examples
#' # Single-control rating curve model - see https://github.com/BaM-tools/RBaM
#' # Parameters are activation stage k, coefficient a and exponent c
#' k=parameter(name='k',init=-0.5)
#' a=parameter(name='a',init=100)
#' c=parameter(name='c',init=1.6)
#' # Define control matrix: columns are controls, rows are stage ranges.
#' controlMatrix=matrix(1,nrow=1,ncol=1)
#' # Stitch it all together into a model object
#' M=model(ID='BaRatin',
#'         nX=1,nY=1, # number of input/output variables
#'         par=list(k,a,c), # list of model parameters
#'         xtra=xtraModelInfo(object=controlMatrix)) # use xtraModelInfo() to pass the control matrix
#' # Define calibration data
#' X=SauzeGaugings['H']
#' Yobs=SauzeGaugings['Q']
#' Yu=SauzeGaugings['uQ']
#' # Define the parameter vector (model parameters + structural error parameters)
#' parvector=c(RBaM::getInitPar(M$par),c(1,0.1))
#' # Compute log-likelihood
#' logLikelihood_BaM(parvector=parvector,X=X,Yobs=Yobs,Yu=Yu,
#'                   llfunk=llfunk_iLinear_Gaussian,mod=M)
#' @export
logLikelihood_BaM <- function(parvector,X,Yobs,Yu,llfunk,mod,Ysim=NULL,llargs=NULL){
  # separate parvector into theta and gamma
  n=length(mod$par);D=length(parvector)
  theta=parvector[1:n] # model parameters
  if(n<D){gamma=parvector[(n+1):D]} else {gamma=NULL} # structural errors parameters
  if(is.null(Ysim)){ # only run model if Ysim not provided as input argument
    # Assign model parameters and run model
    mod$par=setInitPar(parameters=mod$par,values=theta)
    Ysim=runModel(workspace=tempdir(),mod=mod,X=X,stout=NULL)
  }
  if(any(is.na(Ysim)) | is.null(Ysim)){
    return(list(logLikelihood=-Inf,Ysim=Ysim))
  }
  if(is.null(llargs)){
    ll=llfunk(Ysim,Yobs,Yu,gamma)
  } else {
    ll=llfunk(Ysim,Yobs,Yu,gamma,llargs)
  }
  return(list(logLikelihood=ll,Ysim=Ysim))
}

#***************************************************************************----
# Log-posterior functions  ----

#*******************************************************************************
#' BaM log-posterior
#'
#' Log-posterior engine for a model available in BaM.
#'
#' @param parvector numeric vector, parameter vector, including thetas (model parameters)
#'     and gammas (structural errors parameters).
#' @param X data frame, model inputs.
#' @param Yobs data frame, corresponding observed values.
#' @param Yu data frame, measurement uncertainties (standard deviations), same dimensions as Yobs.
#' @param lpfunk function, function computing the log-prior density of parvector.
#' @param llfunk function, function computing the log-likelihood given Ysim, see e.g. \link[RBaM]{llfunk_iid_Gaussian}.
#' @param mod model object, the model to be calibrated.
#' @param Ysim data frame, model-simulated values. When NULL (default), the model is run to provide simulations.
#'     When a non-NULL data frame is provided, it is used as pre-computed simulations, and the model is
#'     hence not run. This is useful to speed-up some MCMC strategies.
#' @param logLikelihood_engine function, engine function used to compute the log-likelihood, see e.g. \link[RBaM]{logLikelihood_BaM}.
#' Unlike functions llfunk, which computes the log-likelihood from model-simulated values Ysim (see e.g. \link[RBaM]{llfunk_iid_Gaussian},
#' logLikelihood_engine computes the log-likelihood from (parameters + inputs X + model mod).
#' @param llargs object, any other arguments to be passed to llfunk.
#' @return A list with the following components:
#'     \item{logPosterior}{numeric, the unnormalized posterior log-pdf.}
#'     \item{logPrior}{numeric, the prior log-pdf.}
#'     \item{logLikelihood}{numeric, the log-likelihood.}
#'     \item{Ysim}{data frame, the model-simulated values.}
#' @examples
#' # Single-control rating curve model - see https://github.com/BaM-tools/RBaM
#' # Parameters are activation stage k, coefficient a and exponent c
#' k=parameter(name='k',init=-0.5)
#' a=parameter(name='a',init=100)
#' c=parameter(name='c',init=1.6)
#' # Define control matrix: columns are controls, rows are stage ranges.
#' controlMatrix=matrix(1,nrow=1,ncol=1)
#' # Stitch it all together into a model object
#' M=model(ID='BaRatin',
#'         nX=1,nY=1, # number of input/output variables
#'         par=list(k,a,c), # list of model parameters
#'         xtra=xtraModelInfo(object=controlMatrix)) # use xtraModelInfo() to pass the control matrix
#' # Define calibration data
#' X=SauzeGaugings['H']
#' Yobs=SauzeGaugings['Q']
#' Yu=SauzeGaugings['uQ']
#' # Define the parameter vector (model parameters + structural error parameters)
#' parvector=c(RBaM::getInitPar(M$par),c(1,0.1))
#' # Define prior function - here for instance just an informative prior on the third parameter
#' myPrior <-function(parvector){dnorm(parvector[3],1.6,0.1,log=TRUE)}
#' # Compute log-likelihood
#' logPosterior_BaM(parvector=parvector,X=X,Yobs=Yobs,Yu=Yu,
#'                  lpfunk=myPrior,llfunk=llfunk_iLinear_Gaussian,mod=M)
#' @export
logPosterior_BaM <- function(parvector,X,Yobs,Yu,lpfunk,llfunk,mod,
                             Ysim=NULL,logLikelihood_engine=logLikelihood_BaM,llargs=NULL){
  # start with the prior to exit early and cheaply if parvector is prior-incompatible
  prior=lpfunk(parvector)
  if(is.infinite(prior)){
    return(list(logPosterior=-Inf,logPrior=-Inf,logLikelihood=NA,Ysim=NULL))
  }
  # finish
  lik=logLikelihood_engine(parvector=parvector,X=X,Yobs=Yobs,Yu=Yu,mod=mod,
                           llfunk=llfunk,Ysim=Ysim,llargs=llargs)
  post=prior+lik$logLikelihood
  return(list(logPosterior=post,logPrior=prior,logLikelihood=lik$logLikelihood,Ysim=lik$Ysim))
}

#*******************************************************************************
#' BaM log-posterior
#'
#' Log-posterior engine for a model available in BaM. This is a wrapped version of
#' \link[RBaM]{logPosterior_BaM}, returning a single numeric value (the log-posterior).
#' This function can hence be passed to standard optimization (e.g. \link[stats]{optim})
#' or MCMC (e.g. \link[mcmc]{metrop}) tools.
#'
#' @inheritParams logPosterior_BaM
#' @return the unnormalized posterior log-pdf (numeric).
#' @examples
#' # Single-control rating curve model - see https://github.com/BaM-tools/RBaM
#' # Parameters are activation stage k, coefficient a and exponent c
#' k=parameter(name='k',init=-0.5)
#' a=parameter(name='a',init=100)
#' c=parameter(name='c',init=1.6)
#' # Define control matrix: columns are controls, rows are stage ranges.
#' controlMatrix=matrix(1,nrow=1,ncol=1)
#' # Stitch it all together into a model object
#' M=model(ID='BaRatin',
#'         nX=1,nY=1, # number of input/output variables
#'         par=list(k,a,c), # list of model parameters
#'         xtra=xtraModelInfo(object=controlMatrix)) # use xtraModelInfo() to pass the control matrix
#' # Define calibration data
#' X=SauzeGaugings['H']
#' Yobs=SauzeGaugings['Q']
#' Yu=SauzeGaugings['uQ']
#' # Define the parameter vector (model parameters + structural error parameters)
#' parvector=c(RBaM::getInitPar(M$par),c(1,0.1))
#' # Define prior function - here for instance just an informative prior on the third parameter
#' myPrior <-function(parvector){dnorm(parvector[3],1.6,0.1,log=TRUE)}
#' # Compute log-likelihood
#' logPosterior_BaM_wrapped(parvector=parvector,X=X,Yobs=Yobs,Yu=Yu,
#'                  lpfunk=myPrior,llfunk=llfunk_iLinear_Gaussian,mod=M)
#' @export
logPosterior_BaM_wrapped <- function(parvector,X,Yobs,Yu,lpfunk,llfunk,mod,
                                 Ysim=NULL,logLikelihood_engine=logLikelihood_BaM,llargs=NULL){
  foo=logPosterior_BaM(parvector=parvector,X=X,Yobs=Yobs,Yu=Yu,lpfunk=lpfunk,llfunk=llfunk,mod=mod,
                       Ysim=Ysim,logLikelihood_engine=logLikelihood_engine,llargs=llargs)
  return(foo$logPosterior)
}
