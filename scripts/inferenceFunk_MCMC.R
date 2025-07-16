library(RBaM)
# setPathToBaM('/home/benjamin.renard/BEN/GitHub/BaM/makefile/')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example of using function runModel() ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define parameters - only name and initial value are needed, no need to specify priors
k1=parameter(name='k1',init=-0.5)
a1=parameter(name='a1',init=50)
c1=parameter(name='c1',init=1.5)
k2=parameter(name='k2',init=1)
a2=parameter(name='a2',init=100)
c2=parameter(name='c2',init=1.67)
# Define xtra object - control matrix for BaRatin
controlMatrix=rbind(c(1,0),c(0,1))
# Assemble Model object
M=model(ID='BaRatin',nX=1,nY=1,par=list(k1,a1,c1,k2,a2,c2),
        xtra=xtraModelInfo(object=controlMatrix))
# Define input data frame
X=data.frame(H=seq(-1,7,length.out=100))
# Run model
Ysim=runModel(workspace=tempdir(),mod=M,X=X,stout=NULL)
plot(X$H,Ysim$Y1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example of computing the log-posterior ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define a prior function, which takes as input a numeric vector (parameter values)
# and return a value (the prior log-density, or -Inf if prior is 0 or unfeasible)
logPrior <- function(parvector){
  out=
    # Priors for model parameters
    dunif(parvector[1],min=-1.5,max=0,log=TRUE)+ # k1
    dlnorm(parvector[2],meanlog=log(50),sdlog=1,log=TRUE)+ # a1
    dnorm(parvector[3],mean=1.5,sd=0.05,log=TRUE)+ # c1
    dnorm(parvector[4],mean=1,sd=1,log=TRUE)+ # k2
    dlnorm(parvector[5],meanlog=log(100),sdlog=1,log=TRUE)+ # a2
    dnorm(parvector[6],mean=1.67,sd=0.05,log=TRUE)+ # c2
    # Priors for structural error parameters
    dlnorm(parvector[7],meanlog=log(1),sdlog=1,log=TRUE)+ # gamma0
    dlnorm(parvector[8],meanlog=log(0.1),sdlog=0.5,log=TRUE) # gamma1
  if(is.na(out) | is.infinite(out)){out=-Inf}
  return(out)
}
# Define calibration data
X=SauzeGaugings['H']
Yobs=SauzeGaugings['Q']
Yu=SauzeGaugings['uQ']
# Select the log-likelihood function from the available RBaM catalog.
# You can also define your own log-likelihood function, which takes as inputs Ysim, Yobs, Yu
# (3 data frames with identical sizes), gamma (vector of structural error parameters),
# and returns a value (the log-likelihood, -Inf if the likelihood is zero or unfeasible).
logLikelihood=llfunk_iLinear_Gaussian
# Compute the posterior log-density at some parameter values
parvector=c(RBaM::getInitPar(M$par),1,0.2)
logpost=logPosterior_BaM(parvector=parvector, # parameter values at which the posterior is evaluated
                         X=X,Yobs=Yobs,Yu=Yu, # calibration data
                         lpfunk=logPrior,llfunk=logLikelihood,mod=M) # inference functions and model

# The logPosterior function returns a list containing log-post, log-prior, log-lkh and simulated values
logpost

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example of maximizing the log-posterior ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Just use function optim() coming with R base installation
maxpost=stats::optim(par=parvector, # starting point
                     fn=logPosterior_BaM_wrapped, # function to be optimized. It's a wrapped version of function logPosterior, returning the log-post value only. See ?logPosterior_wrapped
                     control=list(fnscale=-1), # tells optim to maximize rather than minimize
                     X=X,Yobs=Yobs,Yu=Yu,lpfunk=logPrior,llfunk=logLikelihood,mod=M) # arguments passed to logPosterior_wrapped
maxpost$par

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example of MCMC-sampling the log-posterior ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x0=maxpost$par
names(x0) <- c(RBaM::getNames(M$par),'g1','g2')
# Adaptive Metropolis
ptm <- proc.time()
mcmc=MCMC_AM(logPdf=logPosterior_BaM,x0=x0,
             X=X,Yobs=Yobs,Yu=Yu,lpfunk=logPrior,llfunk=logLikelihood,mod=M) # arguments passed to logPosterior
proc.time() - ptm
pairs(cbind(mcmc$samples,mcmc$components$logPosterior))
# One-At-A-Time Metropolis, no speed-up
ptm <- proc.time()
mcmc=MCMC_OAAT(logPdf=logPosterior_BaM,x0=x0,
             X=X,Yobs=Yobs,Yu=Yu,lpfunk=logPrior,llfunk=logLikelihood,mod=M) # arguments passed to logPosterior
proc.time() - ptm
pairs(cbind(mcmc$samples,mcmc$components$logPosterior))
# One-At-A-Time Metropolis, with speed-up
ptm <- proc.time()
mcmc=MCMC_OAAT(logPdf=logPosterior_BaM,x0=x0,nTheta=length(x0)-2,
               X=X,Yobs=Yobs,Yu=Yu,lpfunk=logPrior,llfunk=logLikelihood,mod=M) # arguments passed to logPosterior
proc.time() - ptm
pairs(cbind(mcmc$samples,mcmc$components$logPosterior))
