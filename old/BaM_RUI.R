#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load useful libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(R.utils)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# globally-accessible variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ConfigBaM.fname="Config_BaM.txt"
ConfigData.fname="Config_Data.txt"
ConfigModel.fname="Config_Model.txt"
ConfigXtra.fname="Config_Xtra.txt"
ConfigMCMC.fname="Config_MCMC.txt"
ConfigCooking.fname="Config_Cooking.txt"
ConfigResiduals.fname="Config_Residuals.txt"
ConfigSummary.fname="Config_Summary.txt"
ConfigRunOptions.fname="Config_RunOptions.txt"
ConfigRemnant.fname="Config_RemnantSigma.txt"
ConfigPredictions.fname="Config_Predictions.txt"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions that need to be updated with every new model made available in BaM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

writeConfig.xtra<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write Model Xtra configuration file ("Config_Xtra.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 11/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all Model parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
 
  x=o$xtra$object
  switch(o$ID,
         #oooooooooooooooooooooooooooooooo
         'BaRatin'={
           val=list()
           for(i in 1:NCOL(x)){val=c(val,list(x[i,]))}
           writeConfig(val,NULL,dir,o$xtra$fname)
           },
         #oooooooooooooooooooooooooooooooo
         'SFD'={writeConfig(x,NULL,dir,o$xtra$fname)},
         #oooooooooooooooooooooooooooooooo
         'SGD'={ # no extra info for this model, should never arrive here
           warning(paste('warning: Xtra not yet handled with model:',o$ID))
         }, 
         #oooooooooooooooooooooooooooooooo
         'Vegetation'={writeConfig(x,NULL,dir,o$xtra$fname)},
         #oooooooooooooooooooooooooooooooo
         'SWOT'={writeConfig(x,NULL,dir,o$xtra$fname)},
         #oooooooooooooooooooooooooooooooo
         'Sediment'={writeConfig(x,NULL,dir,o$xtra$fname)},
         #oooooooooooooooooooooooooooooooo
         'Linear'={ # no extra info for this model, should never arrive here
           warning(paste('warning: Xtra not yet handled with model:',o$ID))
         }, 
         #oooooooooooooooooooooooooooooooo
         'Mixture'={writeConfig(x,NULL,dir,o$xtra$fname)},
         #oooooooooooooooooooooooooooooooo
         'Orthorectification'={writeConfig(x,NULL,dir,o$xtra$fname)},
         #oooooooooooooooooooooooooooooooo
         'GR4J'={
           val=as.list(readLines(x))
           writeConfig(val,NULL,dir,o$xtra$fname,addQuote=F)
           },
         #oooooooooooooooooooooooooooooooo
         warning(paste('warning: unknown model:',o$ID))
         )
}

BaM.catalogue<-function(){
  models=c('BaRatin','SFD','SGD','Vegetation','SWOT','Sediment',
           'Linear','Mixture','Orthorectification','GR4J')
  distributions=c('Gaussian','Uniform','Triangle','LogNormal',
                  'Exponential','GPD','Gumbel','GEV','Inverse Chi2',
                  'PearsonIII','Geometric','Poisson','Binomial','NegBinomial',
                  'FlatPrior','FlatPrior+','FlatPrior-',
                  'FIX','VAR')
  message('MODELS:')
  print(models)
  message('DISTRIBUTIONS:')
  print(distributions)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main interface functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BaM.run<-function(workspace,
                  # BaM executable
                  exedir,exename='BaM.exe',
                  # model/data configuration objects
                  model,data,
                  # remnant uncertainty configuration object
                  remnant=NULL,
                  # other basic configuration objects
                  mcmc=getObject.MCMC(),
                  cook=getObject.cook(),
                  residuals=getObject.residuals(),
                  summary=getObject.summary(),
                  # Define what to do
                  doCalib=T,doPredict=F,
                  # misc.
                  doClean=F
                  ){
  # make sure workspace format is ok.
  # if path is implicit, uses the current R working directory as root
  wk=getAbsolutePath(workspace)
  if(doClean){
    file.remove(list.files(wk,full.names=T))
  }
  # write basic configuration files
  writeConfig.model(dir=wk,o=model)
  writeConfig.data(dir=wk,o=data)
  writeConfig.MCMC(dir=wk,o=mcmc)
  writeConfig.cook(dir=wk,o=cook)
  writeConfig.residuals(dir=wk,o=residuals)  
  writeConfig.summary(dir=wk,o=summary)
  # remnant configuration file
  rem=vector("list",model$nY)
  if(is.null(remnant)){ # use defaults
    for (i in 1:model$nY){
      rem[[i]]=getObject.remnant(
        fname=gsub('.',paste(i,'.',sep=''),ConfigRemnant.fname,fixed=T)
        )
    }
  } else {
    if(model$nY>1 & length(remnant)!=model$nY){
      warning('error: remnant list should have size nY')
      return(NULL)
    }
    for (i in 1:model$nY){
      if(model$nY==1){rem[[i]]=remnant} else {rem[[i]]=remnant[[i]]}
    }
  }
  for (i in 1:model$nY){
    writeConfig.remnant(dir=wk,o=rem[[i]])
  }
  # run options configuration file
  runoptions=getObject.runOptions(mcmc=doCalib,summary=doCalib,
                                  residuals=doCalib,predict=doPredict)
  writeConfig.runOptions(dir=wk,o=runoptions)
  # General controller
  rem.txt=c()
  for(i in 1:length(rem)){
    rem.txt=c(rem.txt,rem[[i]]$fname)
  }
  val=list(paste(wk,'/',sep=''),runoptions$fname,model$fname,model$xtra$fname,
           data$fname,rem.txt,mcmc$fname,cook$fname,
           summary$fname,residuals$fname,ConfigPredictions.fname)
  comment=c('Workspace','Config file: run options','Config file: model',
            'Config file: xtra model information','Config file: Data',
            'Config file: Remnant sigma (as many files as there are output variables separated by commas)',
            'Config file: MCMC','Config file: cooking of MCMC samples',
            'Config file: summary of MCMC samples','Config file: residual analysis',
            'Config file: prediction experiments (master file)'
            )

  writeConfig(val,comment,exedir,ConfigBaM.fname)
  
  # Go!!!!
  runExe(exedir,exename)
  
  # return all info
  return(
    list(
      workspace=workspace,model=model,data=data,
      remnant=rem,mcmc=mcmc,cook=cook,residuals=residuals,
      summary=summary,runoptions=runoptions
      )
    )
}
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get default objects
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getObject.model<-function(ID,nX,nY,nPar,
                          par,
                          fname=ConfigModel.fname,
                          xtra=getObject.xtra()
                          ){
  out=list(fname=fname,ID=ID,nX=nX,nY=nY,nPar=nPar,par=par,xtra=xtra)
  return(out)
}

getObject.xtra<-function(fname=ConfigXtra.fname,object=NULL){
  out=list(fname=fname,object=object)
  return(out)
}

getObject.data<-function(X,Y,data.dir,
                         fname=ConfigData.fname,
                         data.fname='CalibrationData.txt',
                         Xu=NULL,Xb=NULL,Xb.indx=NULL,
                         Yu=NULL,Yb=NULL,Yb.indx=NULL
                         ){
  n=NROW(X)
  if(NROW(Y)!=n){
    warning('error: X and Y have different length');return(NULL)
  }
  data.file=file.path(getAbsolutePath(data.dir),data.fname)
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # create big data frame W
  k=0
  # inputs
  W=data.frame(X)
  names(W)<-paste('X',1:NCOL(X),sep='')
  col.X=(k+1):(k+NCOL(X))
  k=k+NCOL(X)
  # input uncertainty
  if(is.null(Xu)){
    col.Xu=rep(0,NCOL(X))
  } else {
    if(NROW(Xu)!=n){
      warning('error: Xu has incorrect length');return(NULL)
    }
    if(NCOL(Xu)!=NCOL(X)){
      warning('error: Xu has incorrect number of columns');return(NULL)
    }
    foo=data.frame(Xu)
    names(foo)<-paste('Xu',1:NCOL(Xu),sep='')
    W=cbind(W,foo)
    col.Xu=(k+1):(k+NCOL(Xu))
    k=k+NCOL(Xu)
  }
  # input biases
  if(is.null(Xb)){
    col.Xb=rep(0,NCOL(X))
  } else {
    if(NROW(Xb)!=n){
      warning('error: Xb has incorrect length');return(NULL)
    }
    if(NCOL(Xb)!=NCOL(X)){
      warning('error: Xb has incorrect number of columns');return(NULL)
    }
    foo=data.frame(Xb)
    names(foo)<-paste('Xb',1:NCOL(Xb),sep='')
    W=cbind(W,foo)
    col.Xb=(k+1):(k+NCOL(Xb))
    k=k+NCOL(Xb)
  }
  # input bias indices
  if(is.null(Xb.indx)){
    col.Xb.indx=rep(0,NCOL(X))
  } else {
    if(NROW(Xb.indx)!=n){
      warning('error: Xb.indx has incorrect length');return(NULL)
    }
    if(NCOL(Xb.indx)!=NCOL(X)){
      warning('error: Xb.indx has incorrect number of columns');return(NULL)
    }
    foo=data.frame(Xb.indx)
    names(foo)<-paste('Xbindx',1:NCOL(Xb.indx),sep='')
    W=cbind(W,foo)
    col.Xb.indx=(k+1):(k+NCOL(Xb.indx))
    k=k+NCOL(Xb.indx)
  }
  # outputs
  foo=data.frame(Y)
  names(foo)<-paste('Y',1:NCOL(Y),sep='')
  W=cbind(W,foo)
  col.Y=(k+1):(k+NCOL(Y))
  k=k+NCOL(Y)
  # output uncertainty
  if(is.null(Yu)){
    col.Yu=rep(0,NCOL(Y))
  } else {
    if(NROW(Yu)!=n){
      warning('error: Yu has incorrect length');return(NULL)
    }
    if(NCOL(Yu)!=NCOL(Y)){
      warning('error: Yu has incorrect number of columns');return(NULL)
    }
    foo=data.frame(Yu)
    names(foo)<-paste('Yu',1:NCOL(Yu),sep='')
    W=cbind(W,foo)
    col.Yu=(k+1):(k+NCOL(Yu))
    k=k+NCOL(Yu)
  }
  # output biases
  if(is.null(Yb)){
    col.Yb=rep(0,NCOL(Y))
  } else {
    if(NROW(Yb)!=n){
      warning('error: Yb has incorrect length');return(NULL)
    }
    if(NCOL(Yb)!=NCOL(Y)){
      warning('error: Yb has incorrect number of columns');return(NULL)
    }
    foo=data.frame(Yb)
    names(foo)<-paste('Yb',1:NCOL(Yb),sep='')
    W=cbind(W,foo)
    col.Yb=(k+1):(k+NCOL(Yb))
    k=k+NCOL(Yb)
  }
  # output bias indices
  if(is.null(Yb.indx)){
    col.Yb.indx=rep(0,NCOL(Y))
  } else {
    if(NROW(Yb.indx)!=n){
      warning('error: Yb.indx has incorrect length');return(NULL)
    }
    if(NCOL(Yb.indx)!=NCOL(Y)){
      warning('error: Yb.indx has incorrect number of columns');return(NULL)
    }
    foo=data.frame(Yb.indx)
    names(foo)<-paste('Ybindx',1:NCOL(Yb.indx),sep='')
    W=cbind(W,foo)
    col.Yb.indx=(k+1):(k+NCOL(Yb.indx))
    k=k+NCOL(Yb.indx)
  }
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assemble data object  
  out=list(fname=fname,data.file=data.file,
           col.X=col.X,col.Xu=col.Xu,col.Xb=col.Xb,col.Xbindx=col.Xb.indx,
           col.Y=col.Y,col.Yu=col.Yu,col.Yb=col.Yb,col.Ybindx=col.Yb.indx,
           data=W)
  return(out)
}

getObject.remnant<-function(fname=ConfigRemnant.fname,
                            funk='Linear',nPar=2,
                            par=getObject.par(c('gamma1','gamma2'),
                                              init=c(1,0.1),
                                              prior.dist=rep('FlatPrior+',2),
                                              prior.par=list(NULL,NULL)
                                              )
                            ){
  out=list(fname=fname,funk=funk,nPar=nPar,par=par)
  return(out)
}

getObject.MCMC<-function(fname=ConfigMCMC.fname,
                         result.fname='Results_MCMC.txt',
                         nAdapt=100,nCycles=100,
                         minMoveRate=0.1,maxMoveRate=0.5,
                         downMult=0.9,upMult=1.1,
                         multFactor=0.1){
  out=list(fname=fname,result.fname=result.fname,
           nAdapt=nAdapt,nCycles=nCycles,
           minMoveRate=minMoveRate,maxMoveRate=maxMoveRate,
           downMult=downMult,upMult=upMult,multFactor=multFactor)
  return(out)
}

getObject.cook<-function(fname=ConfigCooking.fname,
                         result.fname='Results_MCMC_Cooked.txt',
                         burn=0.5,nSlim=10){
  out=list(fname=fname,result.fname=result.fname,burn=burn,nSlim=nSlim)
  return(out)
}

getObject.residuals<-function(fname=ConfigResiduals.fname,
                         result.fname='Results_Residuals.txt'){
  out=list(fname=fname,result.fname=result.fname)
  return(out)
}

getObject.summary<-function(fname=ConfigSummary.fname,
                            result.fname='Results_Summary.txt'){
  out=list(fname=fname,result.fname=result.fname)
  return(out)
}

getObject.runOptions<-function(fname=ConfigRunOptions.fname,
                         mcmc=T,summary=T,residuals=T,predict=F){
  out=list(fname=fname,mcmc=mcmc,summary=summary,
           residuals=residuals,predict=predict)
  return(out)
}

getObject.par<-function(name,init,prior.dist,prior.par){
  n=length(name)
  if(n==1){ # single-parameter
    out=list(name=name,init=init,
             prior=list(dist=prior.dist,par=prior.par))
    return(out)
  } else { # several parameters
    # check sizes
    if(length(init)!=n | length(prior.dist)!=n | length(prior.par)!=n){
      warning('error: input arguments do not have identical lengths')
      return(NULL)
    }
    out=vector("list",n)
    for(i in 1:n){
      out[[i]]=getObject.par(name[[i]],init[[i]],prior.dist[[i]],prior.par[[i]])
    }
    return(out)    
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write configuration files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

writeConfig.data<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write Data configuration file ("Config_Data.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 10/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all Data parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  val=list(o$data.file,1,nrow(o$data),ncol(o$data),
           o$col.X,o$col.Xu,o$col.Xb,o$col.Xbindx,
           o$col.Y,o$col.Yu,o$col.Yb,o$col.Ybindx)
  
  comment=c(
    'path to data file',
    'number of header lines',
    'Nobs, number of rows in data file (excluding header lines)',
    'number of columns in the data file',
    'columns for X (observed inputs) in data file - comma-separated if several',
    'columns for Xu (random uncertainty in X, EXPRESSED AS A STANDARD DEVIATION - use 0 for a no-error assumption)',
    'columns for Xb (systematic uncertainty in X, EXPRESSED AS A STANDARD DEVIATION - use 0 for a no-error assumption)',
    'columns for Xb_indx (index of systematic errors in X - use 0 for a no-error assumption)',
    'columns for Y (observed outputs) in data file - comma-separated if several',
    'columns for Yu (random uncertainty in Y, EXPRESSED AS A STANDARD DEVIATION - use 0 for a no-error assumption)',
    'columns for Yb (systematic uncertainty in Y, EXPRESSED AS A STANDARD DEVIATION - use 0 for a no-error assumption)',
    'columns for Yb_indx (index of systematic errors in Y - use 0 for a no-error assumption)'
  )

  writeConfig(val,comment,dir,o$fname)
  write.table(x=o$data,file=o$data.file,row.names=F,sep='\t')
}

writeConfig.model<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write Model configuration file ("Config_Model.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 11/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all Model parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  val=list(o$ID,o$nX,o$nY,o$nPar)

  comment=c(
    'Model ID - type BaM.catalogue() in R User Interface to see available models',
    'nX: number of input variables',
    'nY: number of output variables',
    'nPar: number of parameters theta'
    )
  
  if(length(o$par)!=o$nPar){
    warning('error: parameter list has incorrect size')
    return(NULL)
  }
  
  for(i in 1:o$nPar){
    val=c(val,o$par[[i]]$name,o$par[[i]]$init,
          o$par[[i]]$prior$dist,list(o$par[[i]]$prior$par))
    comment=c(comment,
              'Parameter name',
              'Initial guess',
              'Prior distribution - type BaM.catalogue() in R User Interface to see available distributions',
              'Prior parameters'
              )
          
  }
  writeConfig(val,comment,dir,o$fname)
  
  # Write Xtra model information if needed
  if(!is.null(o$xtra$object)){
    writeConfig.xtra(dir,o)
  }
}

writeConfig.remnant<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write Remnant configuration file ("Config_RemnantSigma.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 11/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all remnant parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  val=list(o$funk,o$nPar)
  comment=c(
    'Function f used in sdev=f(Ysim) - available: Constant, Linear, Exponential,Gaussian - resp. npar: 1,2,3,3',
    'Number of parameters gamma for the function f above'
    )
  
  if(length(o$par)!=o$nPar){
    warning('error: parameter list has incorrect size')
    return(NULL)
  }
  
  for(i in 1:o$nPar){
    val=c(val,o$par[[i]]$name,o$par[[i]]$init,
          o$par[[i]]$prior$dist,list(o$par[[i]]$prior$par))
    comment=c(comment,
              'Parameter name',
              'Initial guess',
              'Prior distribution - type BaM.catalogue() in R User Interface to see available distributions',
              'Prior parameters'
              )
  }
  writeConfig(val,comment,dir,o$fname)
}

writeConfig.MCMC<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write MCMC configuration file ("Config_MCMC.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 10/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all MCMC parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: MCMC sampler described here:
  #^*       Renard, B., V. Garreta, and M. Lang (2006), An application of Bayesian
  #^*       analysis and MCMC methods to the estimation of a regional trend in 
  #^*       annual maxima, Water Resources Research, 42(12).
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  val=list(o$result.fname,o$nAdapt,o$nCycles,
           o$minMoveRate,o$maxMoveRate,o$downMult,o$upMult,
           0,'****    DEFINITION OF INITIAL JUMP STD   ****',
           o$multFactor,-9999,-9999)

  comment=c(
    'File name',
    'nAdapt, number of iterations before adaptation',
    'nCycles, number of adaptation cycles (total number of iterations is hence Nadapt * Ncycles)',
    'minMoveRate, lower bound for the targeted move rate',
    'maxMoveRate, higher bound for the targeted move rate',
    'downMult, multiplicative factor to decrease jump standard deviation (<1)',
    'upMult, multiplicative factor to increase jump standard deviation (>1)',
    'Mode for setting the initial Std of the jump distribution - 0=auto[recommended]',
    'Cosmetic line, unused',
    'multFactor, multiplicative factor to set initial jump standard deviations',
    'manual mode, disabled',
    'manual mode, disabled'
    )
  
  writeConfig(val,comment,dir,o$fname)
}

writeConfig.cook<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write Cooking configuration file ("Config_Cooking.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 10/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all Cooking parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  val=list(o$result.fname,o$burn,o$nSlim)
  comment=c(
    'File name',
    'Burn factor (0.4 mean the first 40% of MCMC samples are burnt)',
    'nSlim, slimming period (10 means only one MCMC sample every 10 is kept (after burning)'
    )

  writeConfig(val,comment,dir,o$fname)
}

writeConfig.residuals<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write Residuals configuration file ("Config_Residuals.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 10/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all Residuals parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  val=list(o$result.fname)
  comment=c('File name')  
  writeConfig(val,comment,dir,o$fname)
}

writeConfig.summary<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write Summary configuration file ("Config_Summary.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 10/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all Summary parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  val=list(o$result.fname)
  comment=c('! File name')
  writeConfig(val,comment,dir,o$fname)
}

writeConfig.runOptions<-function(dir,o){
  #^******************************************************************************
  #^* PURPOSE: Write Run Options configuration file ("Config_RunOptions.txt")
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 11/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string] dir, directory where config file is written
  #^*    2. [list] o, object (list) containing all run options parameters
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  val=list(o$mcmc,o$summary,o$residuals,o$predict)
  comment=c('Do MCMC?','Do MCMC summary?','Do residual analysis?','Do predictions?')
  writeConfig(val,comment,dir,o$fname)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Private functions - utilities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

runExe<-function(exedir,exename){
  #^******************************************************************************
  #^* PURPOSE: run an executable file
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon, stolen from Ivan Horner, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 26/02/2016
  #^******************************************************************************
  #^* IN
  #^*    1. [string] exedir, directory of the executable 
  #^*    2. [string] exename, name of the executable 
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  saveWD <- getwd(); # remember current working dir
  setwd(exedir) # need to set working dir to the exe dir
  system2(exename, stdout = "") # run exe
  setwd(saveWD) # move back to initial working directory
}

writeConfig<-function(val,comment,dir,fname,addQuote=T){
  #^******************************************************************************
  #^* PURPOSE: Generic function to write a configuration file
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 12/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [list] val, list of values to be written in the configuration file 
  #^*    2. [string vector] comment, comments (same length as val) 
  #^*    3. [string] dir, destination directory 
  #^*    4. [string] fname, file name 
  #^*    5. [logical] addQuote, add double quotes to any string encountered in val? 
  #^* OUT
  #^*    1. nothing  
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: Overwrites any pre-existing file. Directory created if needed
  #^******************************************************************************
  
  n=length(val)
  txt=vector("character",n)
  for (i in 1:n){
    foo=val[[i]]
    # add double quotes to strings
    if(is.character(foo) & addQuote){foo=addQuotes(foo)}
    # transform R logicals to Fortran logicals
    if(is.logical(foo)){foo=paste(ifelse(foo,'.true.','.false.'))}
    # stitch values and comment
    if(is.null(comment[i])){
      txt[i]=paste(foo,collapse=',')
    } else {
      txt[i]=paste(paste(foo,collapse=','),'!',comment[i])
    }
  }

  if(!dir.exists(dir)){dir.create(dir,recursive=T)}
  file=file.path(dir,fname)
  
  write.table(matrix(txt, ncol = 1, nrow = length(txt)), file = file,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

list2string<-function(x){
  #^******************************************************************************
  #^* PURPOSE: transform a list into a string vector
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 11/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [list] x, list with n elements
  #^* OUT
  #^*    1. [string vector] vector of size n of string-transformed list elements 
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  txt=c()
  if(length(x)>0){
    for (i in 1:length(x)){
      txt=c(txt,paste(x[[i]],collapse=','))
    }
  }
  return(txt)
}

addQuotes<-function(txt){
  #^******************************************************************************
  #^* PURPOSE: add double quotes to a string or to each element of a string vector
  #^******************************************************************************
  #^* PROGRAMMERS: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 12/10/2018
  #^******************************************************************************
  #^* IN
  #^*    1. [string vector] txt
  #^* OUT
  #^*    1. [string vector] double-quoted string vector 
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* TO DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  n=length(txt)
  out=txt
  for(i in 1:n){
    out[i]=paste('"',txt[i],'"',sep='')
  }
  return(out)
}
