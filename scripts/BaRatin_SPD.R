library(RBaM)

# Define BaRatin flavor.
# It should also work with 'BaRatin', need to think about what is the best default.
BaRatinFlavor='BaRatinBAC'

# Calibration data
H=MeyrasGaugings$h
Q=MeyrasGaugings$Q
uQ=MeyrasGaugings$uQ

# Define control matrix.
controlMatrix=rbind(c(1,0,0),c(0,1,0),c(0,1,1))

# Declare variable parameters.
# Note that exponents c cannot be declared variable.
bVAR=c(TRUE, TRUE, FALSE) # b's for first 2 controls (b1 and b2) are VAR
aVAR=c(TRUE, FALSE, FALSE) # a for first control (a1) is VAR

# Define priors. Note that for parameters declared as variable (here b1, a1 and b2),
# the specified prior applies to period 1.
b1=parameter(name='b1',init=-0.6,prior.dist='Gaussian',prior.par=c(-0.6,0.5))
a1=parameter(name='a1',init=exp(2.65),prior.dist='LogNormal',prior.par=c(2.65,0.35))
c1=parameter(name='c1',init=1.5,prior.dist='Gaussian',prior.par=c(1.5,0.025))
b2=parameter(name='b2',init=0,prior.dist='Gaussian',prior.par=c(0,0.5))
a2=parameter(name='a2',init=exp(3.28),prior.dist='LogNormal',prior.par=c(3.28,0.33))
c2=parameter(name='c2',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.025))
b3=parameter(name='b3',init=1.2,prior.dist='Gaussian',prior.par=c(1.2,0.2))
a3=parameter(name='a3',init=exp(3.46),prior.dist='LogNormal',prior.par=c(3.46,0.38))
c3=parameter(name='c3',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.025))
pars=list(b1,a1,c1,b2,a2,c2,b3,a3,c3)

# Define properties of VAR parameters.
# 1. Prior parameters for incremental changes.
# Should be put in a named list, with the names corresponding to the names of the parameters that have been declared variable.
# For b/k's, the 2 values are the mean/sd of the Gaussian prior for ADDITIVE incremental changes.
# For a's, the 2 values are the meanlog/sdlog of the LogNormal prior for MULTIPLICATIVE incremental changes.
# Might need to implement some translator in BaRatinAGE to express these as values +/- extended U.
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

# Ready to estimate
workspace=file.path(getwd(),'BaM_workspace')
mcmc=SPD_estimate(workspace=workspace,controlMatrix=controlMatrix,pars=pars,
                  bVAR=bVAR,aVAR=aVAR,deltaPars=deltaPars,periods=periods,nPeriods=nPeriods,
                  H=H,Q=Q,uQ=uQ)
# Quick look at results
violinPlot(mcmc[,c(1:5)])+ggplot2::labs(title='b1 across 5 periods')
violinPlot(mcmc[,c(6:7)])+ggplot2::labs(title='a1 across 2 periods')
violinPlot(mcmc[,c(9:13)])+ggplot2::labs(title='b2 across 5 periods')
