#' Adaptive Metropolis sampler
#'
#' An adaptive Metropolis sampler largely inspired by Haario et al. (2001, \url{https://doi.org/10.2307/3318737}).
#' The jump covariance is adapted using the empirical covariance of previously-sampled values,
#' and the scaling factor is adapted in order to comply with a specified move rate interval.
#'
#' @param logPdf function, evaluating the log-density of the distribution to sample from (up to a proportionality constant).
#'     logPdf can return either a single numeric value, interpreted as the target log-pdf,
#'     or a list containing components named 'logPosterior', 'logLikelihood' and 'logPrior'.
#' @param x0 numeric vector, starting point
#' @param C0 numeric matrix, covariance matrix of the Gaussian jump distribution (up to a scale factor, see next).
#' @param scaleFactor numeric >0, used to scale the jump covariance. The covariance of the jump distribution is equal to (scaleFactor^2)*C0
#' @param nAdapt integer > 1, number of iterations before adapting covariance C and scaleFactor.
#' @param nCycles integer > 1, number of adaption cycles. Total number of iterations is hence equal to nAdapt*nCycles.
#'     nCycles=1 leads to the standard non-adaptive Metropolis sampler.
#' @param minMoveRate numeric in (0;1), lower bound for the desired move rate interval.
#' @param maxMoveRate numeric in (0;1), upper bound for the desired move rate interval.
#' @param downMult numeric in (0;1), multiplication factor used to decrease scaleFactor when move rate is too low.
#' @param upMult numeric (>1, avoid 1/downMult) multiplication factor used to increase scaleFactor when move rate is too high.
#' @param burnCov numeric in (0;1), fraction of initial values to be discarded before computing the empirical covariance of
#'     sampled vectors, which is used to adapt the jump covariance.
#' @param dofCovMin integer, minimum number of degrees of freedom required to compute the empirical covariance of
#'     sampled vectors and hence to adapt the jump covariance. If D denotes the length of x0, at least
#'     dofCovMin*(D+0.5*(D-1)*(D-2)) iterations are required before adapting the jump covariance
#'     (i.e. dofCovMin times the number of unknown elements in the covariance matrix).
#' @param nCovMax integer, maximum number of iterations used to compute the empirical covariance.
#'     If the number of available iterations is larger than nCovMax, iterations are 'slimmed' to reach nCovMax.
#' @param ... other arguments passed to function logPdf
#' @return A list with the following components:
#'     \item{samples}{data frame, MCMC simulations.}
#'     \item{components}{data frame, corresponding values of the log-posterior, the log-prior and the log-likelihood.}
#'     \item{C}{matrix, the adapted jump covariance matrix.}
#'     \item{scaleFactor}{numeric, the adapted scaling factor.}
#' @examples
#' # Define a 2-dimensional target log-pdf
#' logPdf <- function(x){
#'     p1=log(0.6*dnorm(x[1],0,1)+0.4*dnorm(x[1],2,0.5))
#'     p2=log(dlnorm(x[2],0,1))
#'     return(p1+p2)
#' }
#' # Sample from it
#' mcmc=MCMC_AM(logPdf,c(1,1))
#' plot(mcmc$samples)
#' @export
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats cov var
MCMC_AM <- function(logPdf,x0,
                    C0=diag((0.01*(abs(x0)+0.1))^2),scaleFactor=2.4/sqrt(length(x0)),
                    nAdapt=50,nCycles=20,minMoveRate=0.2,maxMoveRate=0.5,downMult=0.9,upMult=1.1,
                    burnCov=0.2,dofCovMin=10,nCovMax=1000,
                    ...){
  # Set up
  D=length(x0)
  samples=data.frame(matrix(NA,nAdapt*nCycles+1,D))
  if(!is.null(names(x0))){names(samples) <- names(x0)}
  comps=data.frame(matrix(NA,nAdapt*nCycles+1,3)) # components: post, prior, lkh
  names(comps) <- c('logPosterior','logPrior','logLikelihood')
  # Starting value and posterior
  k=1
  samples[k,]=x0
  fx=getComponents(logPdf(x0,...))
  if(is.infinite(fx$logPosterior) | is.na(fx$logPosterior)){
    stop('logPdf(x0) is -Inf or NA.',call.=FALSE)
  }
  comps[k,]=fx[1:3]
  # Jump covariance
  C=C0
  nelement=D+0.5*(D-1)*(D-2) # number of elements in covariance matrix
  nmin=nelement*dofCovMin
  nmax=max(nmin,nCovMax)
  # Start iterations
  for(j in 1:nCycles){
    jumps=mvtnorm::rmvnorm(n=nAdapt,mean=rep(0,D),sigma=(scaleFactor^2)*C)
    move=rep(FALSE,nAdapt)
    for(i in 1:nAdapt){
      k=k+1
      candid=as.numeric(samples[k-1,])+jumps[i,]
      fcandid=getComponents(logPdf(candid,...))
      # Apply Metropolis rule
      foo=applyMetropolisRule(x=samples[k-1,],candid=candid,fx=fx,fcandid=fcandid)
      fx=foo$fx
      samples[k,]=foo$x
      comps[k,]=foo$fx[1:3]
      move[i]=foo$move
    }
    # Adapt scale factor
    mr=mean(move)
    if(mr<minMoveRate){
      scaleFactor=scaleFactor*downMult
    } else if(mr>maxMoveRate){
      scaleFactor=scaleFactor*upMult
    }
    # Adapt covariance
    # 2DO: recursive formula ?
    n0=max(as.integer(burnCov*k),1)
    nval=k-n0+1
    if(nval>nmin){ # enough values to update Cov
      ix=unique(as.integer(seq(n0,k,length.out=min(nval,nmax))))
      vars=apply(samples[ix,],2,var)
      if(min(vars)>0) {C=cov(samples[ix,])}
    } else if (nval>(D*dofCovMin)){ # enough values to estimate a diagonal Cov
      vars=apply(samples[n0:k,],2,var)
      if(min(vars)>0) {C=diag(vars)}
    }
  }
  return(list(samples=samples,components=comps,C=C,scaleFactor=scaleFactor))
}

#' Adaptive One-At-A-Time Metropolis sampler
#'
#' An adaptive Metropolis sampler that updates the parameter vector one component at a time
#' using a 1-dimensional jump. This allows easily adapting the jump standard deviation for each
#' component in order to comply with a specified move rate interval.
#'
#' @param logPdf function, evaluating the log-density of the distribution to sample from (up to a proportionality constant).
#'     logPdf can return either a single numeric value, interpreted as the target log-pdf,
#'     or a list containing components named 'logPosterior', 'logLikelihood' and 'logPrior'.
#' @param x0 numeric vector, starting point.
#' @param s0 numeric vector, starting jump standard deviations.
#' @param nTheta integer>0, size of the "theta" part of x0, i.e. components that represent the
#'     model parameters rather than structural errors parameters (gamma).
#'     This is used to speed-up the sampler by avoiding running the model for gamma components.
#'     nTheta=length(x0) (default) implies no attempt at speeding up.
#' @param nAdapt integer > 1, number of iterations before adapting the jump standard deviations.
#' @param nCycles integer > 1, number of adaption cycles. Total number of iterations is hence equal to nAdapt*nCycles.
#'     nCycles=1 leads to a non-adaptive one-at-a-time Metropolis sampler.
#' @param minMoveRate numeric in (0;1), lower bound for the desired move rate interval.
#' @param maxMoveRate numeric in (0;1), upper bound for the desired move rate interval.
#' @param downMult numeric in (0;1), multiplication factor used to decrease the jump standard deviation when move rate is too low.
#' @param upMult numeric (>1, avoid 1/downMult) multiplication factor used to increase the jump standard deviations when move rate is too high.
#' @param ... other arguments passed to function logPdf
#' @return A list with the following components:
#'     \item{samples}{data frame, MCMC simulations.}
#'     \item{components}{data frame, corresponding values of the log-posterior, the log-prior and the log-likelihood.}
#'     \item{sjump}{numeric vector, the adapted jump standard deviations.}
#' @examples
#' # Define a 2-dimensional target log-pdf
#' logPdf <- function(x){
#'     p1=log(0.6*dnorm(x[1],0,1)+0.4*dnorm(x[1],2,0.5))
#'     p2=log(dlnorm(x[2],0,1))
#'     return(p1+p2)
#' }
#' # Sample from it
#' mcmc=MCMC_OAAT(logPdf,c(1,1))
#' plot(mcmc$samples)
#' @export
#' @importFrom stats rnorm
MCMC_OAAT <- function(logPdf,x0,s0=0.05*(abs(x0)+0.1),nTheta=length(x0),
                      nAdapt=50,nCycles=20,minMoveRate=0.2,maxMoveRate=0.5,downMult=0.9,upMult=1.1,
                      ...){
  # Set up
  D=length(x0)
  samples=data.frame(matrix(NA,nAdapt*nCycles+1,D))
  if(!is.null(names(x0))){names(samples) <- names(x0)}
  comps=data.frame(matrix(NA,nAdapt*nCycles+1,3)) # components: post, prior, lkh
  names(comps) <- c('logPosterior','logPrior','logLikelihood')
  # Starting value and posterior
  logPdfArgs=names(formals(MCMC_AM)) # list of arguments of logPdf function
  returnYsim= ('Ysim' %in% logPdfArgs) & (nTheta<D) # return Ysim to implement speed-up for gammas
  k=1
  samples[k,]=x0
  if(returnYsim){
    fx=getComponents(logPdf(x0,...),returnYsim=TRUE)
  } else {
    fx=getComponents(logPdf(x0,...))
  }
  if(is.infinite(fx$logPosterior) | is.na(fx$logPosterior)){
    stop('logPdf(x0) is -Inf or NA.',call.=FALSE)
  }
  comps[k,]=fx[1:3]
  x=x0
  sjump=s0
  # Start iterations
  for(j in 1:nCycles){
    move=rep(0,D)
    for(i in 1:nAdapt){
      for(d in 1:D){
        candid=x;candid[d]=x[d]+rnorm(1,0,sjump[d])
        if(returnYsim){
          if(d > nTheta){Ysim=fx$Ysim} else {Ysim=NULL} # speed-up
          fcandid=getComponents(logPdf(candid,Ysim=Ysim,...),returnYsim=TRUE)
        } else {
          fcandid=getComponents(logPdf(candid,...))
        }
        # Apply Metropolis rule
        foo=applyMetropolisRule(x=x,candid=candid,fx=fx,fcandid=fcandid)
        x=foo$x
        fx=foo$fx
        move[d]=move[d]+as.integer(foo$move)
      }
      k=k+1
      samples[k,]=x
      comps[k,]=fx[1:3]
    }
    # Adapt scale factor
    mr=move/nAdapt
    for(d in 1:D){
      if(mr[d]<minMoveRate){
        sjump[d]=sjump[d]*downMult
      } else if(mr[d]>maxMoveRate){
        sjump[d]=sjump[d]*upMult
      }
    }
  }
  return(list(samples=samples,components=comps,sjump=sjump))
}


#*******************************************************************************
#' Get Inference Components
#'
#' Function ensuring that the result of the log-posterior evaluation returns
#' a list with fields logPosterior, logPrior and logLikelihood
#'
#' @param obj object, object to be cast.
#' @param returnYsim boolean, return Ysim if available ?
#' @return A 1-row data frame with fields logPosterior, logPrior, logLikelihood and Ysim
#' @keywords internal
getComponents <- function(obj,returnYsim=FALSE){
  if('logPosterior' %in% names(obj)){lpost=obj$logPosterior} else {lpost=obj}
  if('logPrior' %in% names(obj)){lp=obj$logPrior} else {lp=NA}
  if('logLikelihood' %in% names(obj)){ll=obj$logLikelihood} else {ll=NA}
  if(returnYsim & ('Ysim' %in% names(obj))){Ysim=obj$Ysim} else {Ysim=NA}
  return(data.frame(logPosterior=lpost,logPrior=lp,logLikelihood=ll,Ysim=Ysim))
}

#*******************************************************************************
#' Metropolis Rule
#'
#' Application of Metropolis rule.
#'
#' @param x numeric vector, current sample.
#' @param candid numeric vector, candidate sample.
#' @param fx data frame, a 1-row data frame with fields logPosterior, logPrior and
#'     logLikelihood evaluated for the current sample.
#' @param fcandid data frame, a 1-row data frame with fields logPosterior, logPrior and
#'     logLikelihood evaluated for the candidate sample.
#' @return A list with fields x (resulting sample), fx (resulting target) and move (did the chain move?).
#' @keywords internal
#' @importFrom stats runif
applyMetropolisRule <- function(x,candid,fx,fcandid){
  move=FALSE
  # if NA or -Inf, reject candid
  if(is.na(fcandid$logPosterior) | fcandid$logPosterior==-Inf){
    return(list(x=x,fx=fx,move=move))
  }
  # Metropolis rule
  ratio=exp(fcandid$logPosterior-fx$logPosterior)
  u=runif(1) # throw the dice
  if(u<=ratio){ # accept
    x=candid
    fx=fcandid
    move=TRUE
  }
  return(list(x=x,fx=fx,move=move))
}
