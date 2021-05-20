require(R.utils)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User-specific declarations - change it!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# define the folder where BaM_RUI.R stands on your file system
dir.RUI=getAbsolutePath("C:\\BEN\\FORGE\\BaM\\trunk\\R")
# define the folder where BaM executable stands on your file system, and its name
dir.exe=getAbsolutePath("C:\\BEN\\FORGE\\BaM\\trunk\\core\\IVF\\BaM_MiniDMSL\\Release")
file.exe='BaM_MiniDMSL.exe'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load and set all basic stuff
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load R User Interface utilities
source(file.path(dir.RUI,'BaM_RUI.R'))
# Load BaMplot utilities
source(file.path(dir.RUI,'BaMplots.R'))
# Model
mdl.ID='BaRatin'
mdl.nX=1;mdl.nY=1 # number of input/output variables
mdl.matrix=t(matrix(c(1,0,0,1),2,2)) # control matrix
mdl.nPar=NCOL(mdl.matrix)*3 # number of parameters
# Data
dat=read.table('Gaugings.txt',header=T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pilot BaM 1 - minimalistic approach, using all defaults
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1. select workspace (where all config & result files are written)
# workspace folder will be created if needed
workspace=file.path(getwd(),'example1')

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 2. define object containing calibration data
cdata=getObject.data(X=dat$H,Y=dat$Q, # Inputs, outputs
                     data.dir=workspace # folder where data are rewritten in BaM format
                     )

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 3. define object representing the model to be calibrated
# 3.a. define object containing model parameters (names, priors, etc.)
par.names=c('k1','a1','c1','k2','a2','c2') # parameter names
par.init=c(-0.5,53,1.5,1.5,144,1.67) # Initial guess for each parameter
par.priordist=c('Uniform','LogNormal','LogNormal', # prior distributions for 1st control
                'Gaussian','LogNormal','LogNormal') # prior distributions for 2nd control
par.priorpar=list(c(-1,-0.1), # k1 - parameters of Uniform prior
                  c(log(53),0.2), # a1 - parameters of LogNormal prior
                  c(log(1.5),0.02), # c1 - parameters of LogNormal prior
                  c(1.5,0.5), # k2 - parameters of Gaussian prior
                  c(log(144),0.3), # a2 - parameters of LogNormal prior
                  c(log(1.67),0.02) # c2 - parameters of LogNormal prior
                  )
mpar=getObject.par(name=par.names,
                   init=par.init,
                   prior.dist=par.priordist,
                   prior.par=par.priorpar
                   )
# 3.b now can define model object
model=getObject.model(ID=mdl.ID,nX=mdl.nX,nY=mdl.nY,nPar=mdl.nPar, # basics
                      par=mpar, # parameters 
                      xtra=getObject.xtra(object=mdl.matrix) #Xtra model information - for BaRatin, the control matrix
                      )

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 4. calibrate
bam1=BaM.run(workspace=workspace,exedir=dir.exe,exename=file.exe,
             model=model,data=cdata)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 5. do basic plots
MCMCfile=file.path(bam1$workspace,bam1$cook$result.fname)
# MCMC trace
g=MCMCplot(MCMCfile=MCMCfile,type="trace")
X11();print(g)
# prior/posterior marginal pdfs
g=MCMCplot(MCMCfile=MCMCfile,type="density",prior=bam1$model$par)
X11();print(g)
# standardized residuals
ResFile=file.path(bam1$workspace,bam1$residuals$result.fname)
g=Residualplot(ResFile=ResFile,nX=bam1$model$nX,nY=bam1$model$nY)
X11();print(g[[3]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pilot BaM 2 - Play around with various options
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1. set workspace 
workspace=file.path(getwd(),'example2')

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 2. define data object - same as previously, just added some streamflow uncertainty
cdata=getObject.data(X=dat$H,Y=dat$Q, # Inputs, outputs
                     Yu=0.05*dat$Q, # 5% uncertainty (stdev) on all gauged discharges  
                     data.dir=workspace # folder where data are rewritten in BaM format
                     )

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 3. define model object - same as previously, just changed name of configuration file
model=getObject.model(ID=mdl.ID,nX=mdl.nX,nY=mdl.nY,nPar=mdl.nPar, # basics
                      par=mpar, # parameters 
                      xtra=getObject.xtra(object=mdl.matrix), #Xtra model information - for BaRatin, the control matrix
                      fname='Config_BaRatin.txt' # change name of configuration file (default: Config_Model.txt)
                      )

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 4. Define other configurations objects, playing around with options
# a. MCMC: change result file name (default: 'Results_MCMC.txt') and increase number of MCMC cycles (default: 100)
mcmc=getObject.MCMC(result.fname='Results_MCMC_raw.txt',nCycle=1000)
# b. MCMC cooking: burn less (default: 0.5), slim more (default: 10)
cook=getObject.cook(burn=0.2,nSlim=100)
# c. MCMC summary: change name of result file (default: 'Results_Summary.txt')
summary=getObject.summary(result.fname='Results_MCMC_summary.txt')
# d. Residual analysis: change name of result file (default: 'Results_Residuals.txt')
residuals=getObject.residuals(result.fname='Results_ResAnalysis.txt')
# e. remnant error model: define it explicitly
remnant=getObject.remnant(funk='Linear',nPar=2,
                          par=getObject.par(c('gamma1','gamma2'),
                                              init=c(1,0.1),
                                              prior.dist=c('LogNormal','Uniform'),
                                              prior.par=list(c(log(10),1),c(0,1))
                                            )
                          )

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 5. calibrate
bam2=BaM.run(workspace=workspace,exedir=dir.exe,exename=file.exe,
             model=model,data=cdata,remnant=remnant,
             mcmc=mcmc,cook=cook,residuals=residuals,summary=summary,
             doCalib=T,doPredict=F,# Define what to do
             doClean=T # pre-cleaning of the workspace. Warning: this will delete any existing file!
             )

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 5. do basic plots
MCMCfile=file.path(bam2$workspace,bam2$cook$result.fname)
# MCMC trace
g=MCMCplot(MCMCfile=MCMCfile,type="trace")
X11();print(g)
# prior/posterior marginal pdfs
g=MCMCplot(MCMCfile=MCMCfile,type="density",prior=bam2$model$par)
X11();print(g)
# standardized residuals
ResFile=file.path(bam2$workspace,bam2$residuals$result.fname)
g=Residualplot(ResFile=ResFile,nX=bam2$model$nX,nY=bam2$model$nY)
X11();print(g[[3]])

