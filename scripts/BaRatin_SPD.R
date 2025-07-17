library(RBaM)

# BaRatin flavor
BaRatinFlavor='BaRatin' # 'BaRatinBAC'
remnant=rep(list(remnantErrorModel()),1)

# Calibration data
H=MeyrasGaugings$h
Q=MeyrasGaugings$Q
uQ=MeyrasGaugings$uQ

# Define control matrix: columns are controls, rows are stage ranges.
controlMatrix=rbind(c(1,0,0),c(0,1,0),c(0,1,1))

# Declare variable parameters.
# Only b/k's and a's are allowed to be VAR (all c's are stable).
kVAR=c(TRUE, TRUE, FALSE) # k's for first 2 controls (k1 and k2) are VAR
aVAR=c(TRUE, FALSE, FALSE) # a for first control (a1) is VAR

# Define priors as usual.
# For VAR parameters, they will be interpreted as the prior for period 1.
# Also for VAR parameters, the prior distribution for k/b's has to be 'Gaussian',
# while the prior distribution for a's has to be 'LogNormal'.
k1=parameter(name='k1',init=-0.6,prior.dist='Gaussian',prior.par=c(-0.6,0.5))
a1=parameter(name='a1',init=exp(2.65),prior.dist='LogNormal',prior.par=c(2.65,0.35))
c1=parameter(name='c1',init=1.5,prior.dist='Gaussian',prior.par=c(1.5,0.025))
k2=parameter(name='k2',init=0,prior.dist='Gaussian',prior.par=c(0,0.5))
a2=parameter(name='a2',init=exp(3.28),prior.dist='LogNormal',prior.par=c(3.28,0.33))
c2=parameter(name='c2',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.025))
k3=parameter(name='k3',init=1.2,prior.dist='Gaussian',prior.par=c(1.2,0.2))
a3=parameter(name='a3',init=exp(3.48),prior.dist='LogNormal',prior.par=c(3.46,0.38))
c3=parameter(name='c3',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.025))
allPars=list(k1,a1,c1,k2,a2,c2,k3,a3,c3)

# Define properties of VAR parameters.
# 1. Prior parameters for incremental changes.
# For k/b's, the 2 parameters are the mean/sd of the Gaussian prior for ADDITIVE incremental changes.
# For a's, the 2 parameters are the meanlog/sdlog of the LogNormal prior for MULTIPLICATIVE incremental changes.
# Parameters should be put in a named list, with the names corresponding to the names of the parameters that have been declared variable.
deltaPars=list(k1=c(0,0.5),a1=c(0,0.2),k2=c(0,0.5))
# 2. Periods
# Periods for each VAR parameter should be put in a named list as previously.
# Periods do not need to be the same for all VAR parameters - see a1 below for example.
periods=list(k1=MeyrasGaugings$Period,a1=c(rep(1,49),rep(2,55)),k2=MeyrasGaugings$Period)
# 3. Number of periods
# Number of periods for each VAR parameter should be put in a named list as previously.
# In general this will just be the max of each period defined previously, but not always:
# there could be one or several additional periods with no gaugings.
nPeriods=lapply(periods,max)

#' Properties of VAR parameters
#'
#' This function computes the properties of VAR parameters (prior means, sds and correlation)
#' given the prior for period 1 and the prior for incremental changes.
#'
#' @param p1Par numeric vector of length 2, parameters of the prior for period 1.
#' @param deltaPar numeric vector of length 2, parameters of the prior for incremental changes.
#' @param nPeriod integer, number of periods.
#' @return A list with the following components:
#'     \item{mean}{numeric vector of length nPeriod, prior means.}
#'     \item{sd}{numeric vector of length nPeriod, prior standard deviations.}
#'     \item{cor}{matrix of dim nPeriod*nPeriod, prior correlation matrix.}
#' @examples
#' res=getVARproperties(p1Par=c(2,0.1),deltaPar=c(0,0.3),nPeriod=25)
#' image(res$cor)
#' @importFrom stats cov2cor
getVARproperties <- function(p1Par,deltaPar,nPeriod){
  if(nPeriod<=1){stop('nPeriod should be strictly larger than 1',call.=FALSE)}
  mu=cumsum(c(p1Par[1],rep(deltaPar[1],nPeriod-1)))
  v=cumsum(c(p1Par[2],rep(deltaPar[2],nPeriod-1))^2)
  cov=diag(v)
  for(i in 1:(nPeriod-1)){
    for(j in (i+1):nPeriod){
      cov[i,j]=v[i]
      cov[j,i]=cov[i,j]
    }
  }
  return(list(mean=mu,sd=sqrt(v),cor=stats::cov2cor(cov)))
}

#' Bloc-diagonal matrix constructor
#'
#' This function creates a square bloc-diagonal matrix from a list of square blocs.
#'
#' @param blocs list, each element is a square matrix.
#' @return A square matrix.
#' @examples
#' blocDiag(list(1,cbind(c(1,2),c(3,4)),25))
blocDiag <- function(blocs){
  n=sum(sapply(blocs,NROW))
  M=diag(1,n,n)
  k=0
  for(i in 1:length(blocs)){
    p=NROW(blocs[[i]])
    M[(k+1):(k+p),(k+1):(k+p)]=blocs[[i]]
    k=k+p
  }
  return(M)
}

workspace=file.path(getwd(),'BaM_workspace')
nControl=NROW(controlMatrix)
parlist=corlist=vector('list',length(allPars))
names(parlist)=getNames(allPars)

dfperiods=as.data.frame(periods)
names(dfperiods) <- paste0(names(dfperiods),'_period')
D=dataset(X=data.frame(H=H),Y=data.frame(Q=Q),Yu=data.frame(uQ=uQ),VAR.indx=dfperiods,data.dir=workspace)
m=0
for(i in 1:nControl){
  # k
  m=m+1
  if(kVAR[i]){
    nam=allPars[[m]]$name
    if(!(nam %in% names(deltaPars))){
      mess=paste('Parameter',nam,'not found in named list deltaPars.')
      stop(mess,call.=FALSE)
    }
    if(!(nam %in% names(nPeriods))){
      mess=paste('Parameter',nam,'not found in named list nPeriods')
      stop(mess,call.=FALSE)
    }
    np=nPeriods[[nam]]
    props=getVARproperties(p1Par=allPars[[m]]$prior$par,deltaPar=deltaPars[[nam]],nPeriod=np)
    priorpars=split(cbind(props$mean,props$sd),seq(np))
    foo=parameter_VAR(name=nam, # parameter name
                      index=paste0(nam,'_period'), # Name of the column in D$VAR.indx containing the period index
                      d=D, # The dataset mentioned above
                      init=props$mean, # first guesses
                      prior.dist=rep('Gaussian',np), # prior distributions
                      prior.par=priorpars # prior parameters
    )
    parlist[[m]]=foo
    corlist[[m]]=props$cor
  } else {
    parlist[[m]]=allPars[[m]]
    corlist[[m]]=1
  }
  # a
  m=m+1
  if(aVAR[i]){
    nam=allPars[[m]]$name
    if(!(nam %in% names(deltaPars))){
      mess=paste('Parameter',nam,'not found in named list deltaPars.')
      stop(mess,call.=FALSE)
    }
    if(!(nam %in% names(nPeriods))){
      mess=paste('Parameter',nam,'not found in named list nPeriods')
      stop(mess,call.=FALSE)
    }
    np=nPeriods[[nam]]
    props=getVARproperties(p1Par=allPars[[m]]$prior$par,deltaPar=deltaPars[[nam]],nPeriod=np)
    priorpars=split(cbind(props$mean,props$sd),seq(np))
    foo=parameter_VAR(name=nam, # parameter name
                      index=paste0(nam,'_period'), # Name of the column in D$VAR.indx containing the period index
                      d=D, # The dataset mentioned above
                      init=exp(props$mean), # first guesses
                      prior.dist=rep('LogNormal',np), # prior distributions
                      prior.par=priorpars # prior parameters
    )
    parlist[[m]]=foo
    corlist[[m]]=props$cor
  } else {
    parlist[[m]]=allPars[[m]]
    corlist[[m]]=1
  }
  # c
  m=m+1
  parlist[[m]]=allPars[[m]]
  corlist[[m]]=1
}
corlist=c(corlist,rep(1,length(remnant[[1]]$par)))
cormat=blocDiag(corlist)

