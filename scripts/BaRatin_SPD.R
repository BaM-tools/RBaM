library(RBaM)

# BaRatin flavor
BaRatinFlavor='BaRatinBAC' # should work also with 'BaRatin' - what's the safest default?

# Calibration data
H=MeyrasGaugings$h
Q=MeyrasGaugings$Q
uQ=MeyrasGaugings$uQ

# Define control matrix: columns are controls, rows are stage ranges.
controlMatrix=rbind(c(1,0,0),c(0,1,0),c(0,1,1))

# Declare variable parameters.
bVAR=c(TRUE, TRUE, FALSE) # b's for first 2 controls (k1 and k2) are VAR
aVAR=c(TRUE, FALSE, FALSE) # a for first control (a1) is VAR

# Define priors.
b1=parameter(name='b1',init=-0.6,prior.dist='Gaussian',prior.par=c(-0.6,0.5))
a1=parameter(name='a1',init=exp(2.65),prior.dist='LogNormal',prior.par=c(2.65,0.35))
c1=parameter(name='c1',init=1.5,prior.dist='Gaussian',prior.par=c(1.5,0.025))
b2=parameter(name='b2',init=0,prior.dist='Gaussian',prior.par=c(-0.6,0.5))
a2=parameter(name='a2',init=exp(3.28),prior.dist='LogNormal',prior.par=c(3.28,0.33))
c2=parameter(name='c2',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.025))
b3=parameter(name='b3',init=1.2,prior.dist='Gaussian',prior.par=c(1.2,0.2))
a3=parameter(name='a3',init=exp(3.48),prior.dist='LogNormal',prior.par=c(3.46,0.38))
c3=parameter(name='c3',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.025))
pars=list(b1,a1,c1,b2,a2,c2,b3,a3,c3)

# Define properties of VAR parameters.
# 1. Prior parameters for incremental changes.
# For b/k's, the 2 parameters are the mean/sd of the Gaussian prior for ADDITIVE incremental changes.
# For a's, the 2 parameters are the meanlog/sdlog of the LogNormal prior for MULTIPLICATIVE incremental changes.
# Parameters should be put in a named list, with the names corresponding to the names of the parameters that have been declared variable.
deltaPars=list(b1=c(0,0.25),a1=c(0,0.2),b2=c(0,0.5))
# 2. Periods
# Periods for each VAR parameter should be put in a named list as previously.
# Periods do not need to be the same for all VAR parameters - see a1 below for example.
periods=list(b1=MeyrasGaugings$Period,a1=c(rep(1,49),rep(2,55)),b2=MeyrasGaugings$Period)
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
#' @export
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
#' @export
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

#' Estimation of a BaRatin-SPD model
#'
#' Run BaM to estimate a BaRatin-SPD model. The following assumptions hold:
#' \itemize{
#'   \item Only parameters b/k's and a's can be variable. Exponents c's are stable.
#'   \item Incremental changes affecting parameters b/k's are additive, while
#'         incremental changes affecting parameters a's are multiplicative.
#'   \item The prior distribution for parameters b/k's and associated incremental changes has to be 'Gaussian'.
#'   \item The prior distribution for parameters a's and associated incremental changes has to be 'LogNormal'.
#' }
#'
#' @param workspace Character, directory where config and result files are stored.
#' @param controlMatrix Integer matrix, control matrix, dimension nControl*nControl.
#' @param pars list of parameter objects, parameters of the model.
#'     For VAR parameters, they will be interpreted as the prior for period 1.
#' @param bVAR Logical vector, size nControl. bVAR[i]=TRUE means that the b/k parameter of control i
#'     is variable, otherwise it is stable.
#' @param aVAR Logical vector, size nControl. aVaR[i]=TRUE means that the a parameter of control i
#'     is variable, otherwise it is stable.
#' @param deltaPars list, prior parameters of incremental changes for each VAR parameter.
#'     deltaPars should be a named list, with the names corresponding to the names of the parameters that have been declared variable.
#'     Each element of deltaPars is then a numeric vector of size 2.
#'     For b/k's, the 2 values are the mean/sd of the Gaussian prior for ADDITIVE incremental changes.
#'     For a's, the 2 parameters are the meanlog/sdlog of the LogNormal prior for MULTIPLICATIVE incremental changes.
#' @param periods list, period index for each VAR parameter.
#'     periods should be a named list as previously.
#'     Each element of the list is an integer vector (starting at 1) with same length as the calibration data.
#'     Periods do not need to be the same for all VAR parameters.
#' @param H numeric vector, gauging stages.
#' @param Q numeric vector, gauging discharges.
#' @param uQ numeric vector, gauging discharge uncertainties.
#' @param nPeriods list, number of periods for each VAR parameter.
#'     nPeriods should be a named list as deltaPars and periods.
#'     In general and by default, nPeriods[[i]] is just the max of periods[[i]],
#'     but this is not compulsory: there could be one or several additional periods with no gaugings.
#' @param BaRatinFlavor character, either 'BaRatinBAC' (default) or 'BaRatin' (the original k-a-c parameterization).
#'     It is in general easier to specify priors on changes affecting b's than k's, hence the default choice.
#'     However, 'BaRatinBAC' requires some numerical resolution and it is hence a bit slower and more prone to failures than 'BaRatin'.
#' @param remnant remnantErrorModel object, by default the structural standard deviation varies as an affine
#'     function of simulated discharges, with very wide priors on coefficients g1 and g2.
#' @param mcmcOpt mcmcOptions object, MCMC options passed to BaM.
#' @return A data frame containing the MCMC simulations performed by BaM.
#' @examples
#' # Calibration data
#' H=MeyrasGaugings$h
#' Q=MeyrasGaugings$Q
#' uQ=MeyrasGaugings$uQ
#' # Control matrix
#' controlMatrix=rbind(c(1,0,0),c(0,1,0),c(0,1,1))
#' # Declare variable parameters.
#' bVAR=c(TRUE, TRUE, FALSE) # b's for first 2 controls (k1 and k2) are VAR
#' aVAR=c(TRUE, FALSE, FALSE) # a for first control (a1) is VAR
#' # Define priors.
#' b1=parameter(name='b1',init=-0.6,prior.dist='Gaussian',prior.par=c(-0.6,0.5))
#' a1=parameter(name='a1',init=exp(2.65),prior.dist='LogNormal',prior.par=c(2.65,0.35))
#' c1=parameter(name='c1',init=1.5,prior.dist='Gaussian',prior.par=c(1.5,0.025))
#' b2=parameter(name='b2',init=0,prior.dist='Gaussian',prior.par=c(-0.6,0.5))
#' a2=parameter(name='a2',init=exp(3.28),prior.dist='LogNormal',prior.par=c(3.28,0.33))
#' c2=parameter(name='c2',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.025))
#' b3=parameter(name='b3',init=1.2,prior.dist='Gaussian',prior.par=c(1.2,0.2))
#' a3=parameter(name='a3',init=exp(3.48),prior.dist='LogNormal',prior.par=c(3.46,0.38))
#' c3=parameter(name='c3',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.025))
#' pars=list(b1,a1,c1,b2,a2,c2,b3,a3,c3)
#' # Define properties of VAR parameters.
#' deltaPars=list(b1=c(0,0.25),a1=c(0,0.2),b2=c(0,0.5))
#' periods=list(b1=MeyrasGaugings$Period,a1=c(rep(1,49),rep(2,55)),b2=MeyrasGaugings$Period)
#' # Run BaM and estimate SPD parameters
#' mcmc=estimateSPD(workspace=tempdir(),controlMatrix=controlMatrix,pars=pars,
#'                  bVAR=bVAR,aVAR=aVAR,deltaPars=deltaPars,periods=periods,
#'                  H=H,Q=Q,uQ=uQ)
#'  @export
estimateSPD <-function(workspace,controlMatrix,pars,
                  bVAR,aVAR,deltaPars,periods,
                  H,Q,uQ=0*Q,nPeriods=lapply(periods,max),
                  BaRatinFlavor='BaRatinBAC',
                  remnant=remnantErrorModel(funk='Linear',
                                            par=list(parameter('g1',1,'LogNormal',c(0,10)),
                                                     parameter('g2',0.1,'LogNormal',c(log(0.1),10)))),
                  mcmcOpt=mcmcOptions()
                  ){
  # Size checks
  nCalib=NROW(H)
  if( NROW(Q)!=nCalib | NROW(uQ)!=nCalib | any(sapply(periods,NROW)!=nCalib) ){
    mess=paste('Size mismatch in H, Q, uQ or periods.')
    stop(mess,call.=FALSE)
  }
  nControl=NROW(controlMatrix)
  if( NROW(bVAR)!=nControl | NROW(aVAR)!=nControl |
      NROW(deltaPars)!=nControl | NROW(periods)!=nControl | NROW(nPeriods)!=nControl |
      NROW(pars)!=3*nControl ){
    mess=paste('Size mismatch in aVAR, bVAR, deltaPars, periods, nPeriods or pars')
    stop(mess,call.=FALSE)
  }

  # Initialize
  parlist=corlist=vector('list',length(pars))
  names(parlist)=getNames(pars)
  dfperiods=as.data.frame(periods)
  names(dfperiods) <- paste0(names(dfperiods),'_period')
  D=dataset(X=data.frame(H=H),Y=data.frame(Q=Q),Yu=data.frame(uQ=uQ),VAR.indx=dfperiods,data.dir=workspace)
  m=0
  for(i in 1:nControl){
    # k
    m=m+1
    if(bVAR[i]){
      nam=pars[[m]]$name
      if(!(nam %in% names(deltaPars))){
        mess=paste('Parameter',nam,'not found in named list deltaPars.')
        stop(mess,call.=FALSE)
      }
      if(!(nam %in% names(nPeriods))){
        mess=paste('Parameter',nam,'not found in named list nPeriods')
        stop(mess,call.=FALSE)
      }
      if(pars[[m]]$prior$dist!='Gaussian'){
        mess=paste0("Prior distribution can only be 'Gaussian' for VAR parameter ",nam,'.')
        stop(mess,call.=FALSE)
      }
      np=nPeriods[[nam]]
      props=getVARproperties(p1Par=pars[[m]]$prior$par,deltaPar=deltaPars[[nam]],nPeriod=np)
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
      parlist[[m]]=pars[[m]]
      corlist[[m]]=1
    }
    # a
    m=m+1
    if(aVAR[i]){
      nam=pars[[m]]$name
      if(!(nam %in% names(deltaPars))){
        mess=paste('Parameter',nam,'not found in named list deltaPars.')
        stop(mess,call.=FALSE)
      }
      if(!(nam %in% names(nPeriods))){
        mess=paste('Parameter',nam,'not found in named list nPeriods')
        stop(mess,call.=FALSE)
      }
      if(pars[[m]]$prior$dist!='LogNormal'){
        mess=paste0("Prior distribution can only be 'LogNormal' for VAR parameter ",nam,'.')
        stop(mess,call.=FALSE)
      }
      np=nPeriods[[nam]]
      props=getVARproperties(p1Par=pars[[m]]$prior$par,deltaPar=deltaPars[[nam]],nPeriod=np)
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
      parlist[[m]]=pars[[m]]
      corlist[[m]]=1
    }
    # c - cannot be VAR
    m=m+1
    parlist[[m]]=pars[[m]]
    corlist[[m]]=1
  }
  # Add elements to corlist for structural error parameters
  corlist=c(corlist,rep(1,length(remnant$par)))
  # Transform corlist into correlation matrix
  cormat=blocDiag(corlist)
  write.table(cormat,file=file.path(workspace,'PriorCorrelation.txt'),
              row.names=FALSE,col.names=FALSE)

  # Set up model
  hmax=max(H)+2*diff(range(H))
  if(BaRatinFlavor=='BaRatinBAC'){
    foo=list(controlMatrix,hmax)
  } else {
    foo=controlMatrix
  }
  M=model(ID=BaRatinFlavor,nX=1,nY=1,par=parlist,
          xtra=xtraModelInfo(object=foo))

  # Go !
  BaM(workspace=workspace,mod=M,data=D,remnant=list(remnant),
      mcmc=mcmcOpt,cook=mcmcCooking(),preClean=FALSE)
  fname=file.path(workspace,'Results_Cooking.txt')
  if(file.exists(fname)){res=readMCMC(fname)} else {res=NULL}
  return(res)
}

workspace=file.path(getwd(),'BaM_workspace')
mcmc=estimateSPD(workspace=workspace,controlMatrix=controlMatrix,pars=pars,
                 bVAR=bVAR,aVAR=aVAR,deltaPars=deltaPars,periods=periods,
                 H=H,Q=Q,uQ=uQ)
violinPlot(mcmc[,c(1:5,9:13)])
