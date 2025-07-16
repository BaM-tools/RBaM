library(RBaM)

# BaRatin flavor
BaRatin_model='BaRatin'

# Calibration data
H=MeyrasGaugings$h
Q=MeyrasGaugings$Q
uQ=MeyrasGaugings$uQ
period_k=MeyrasGaugings$Period
period_a=rep(1,length(H));period_a[period_k %in% c(4,5)]=2

# Define control matrix: columns are controls, rows are stage ranges.
controlMatrix=rbind(c(1,0,0),c(0,1,0),c(0,1,1))

# Define variable vs. stable parameters
kVAR=c(TRUE, TRUE, FALSE)
aVAR=c(TRUE, FALSE, FALSE)

k1=list(VU_p1=c(-0.6,1),VU_delta=c(0,1),period=period_k)
a1=list(VU_p1=c(14,10),VU_delta=c(1,0.2),period=period_a)

c1=parameter(name='c1',init=1.5,prior.dist='Gaussian',prior.par=c(1.5,0.025))
