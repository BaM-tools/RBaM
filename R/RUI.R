#***************************************************************************----
# R User Interface: main functions  ----

#*******************************************************************************
#' Run BaM
#'
#' Run BaM.exe
#'
#' @param workspace Character, directory where config and result files are stored.
#' @param mod model object, the model to be calibrated
#' @param data dataset object, calibration data
#' @param remnant list of remnantErrorModel objects.
#'     WARNING: make sure you use a list of length mod$nY (even if mod$nY=1!)
#' @param mcmc mcmcOptions object, MCMC simulation number and options
#' @param cook mcmcCooking object, properties of MCMC cooking (burn and slice)
#' @param summary mcmcSummary object, properties of MCMC summary
#' @param residuals residualOptions object, properties of residual analysis
#' @param pred list of prediction objects, properties of prediction experiments
#' @param doCalib Logical, do Calibration? (mcmc+cooking+summary+residuals)
#' @param doPred Logical, do Prediction?
#' @param na.value numeric, value used for NAs when writing the dataset in BaM format
#' @param run Logical, run BaM? if FALSE, just write config files.
#' @param preClean Logical, start by cleaning up workspace?
#'  Be careful, this will delete all files in the workspace, including old results!
#' @param dir.exe Character, directory where BaM executable stands.
#' @param name.exe Character, name of the executable without extension ('BaM' by default).
#' @param predMaster_fname Character, name of configuration file pointing to all prediction experiments.
#' @param stout Character string, standard output (see ?system2).
#'     In particular, stout="" (default) shows BaM messages in the console,
#'     stout=NULL discards BaM messages, stout='log.txt' saves BaM messages in file "log.txt".
#' @return Nothing: just write config files and runs the executable.
#' @examples
#' # Fitting a rating curve - see https://github.com/BaM-tools/RBaM
#' workspace=tempdir()
#' D=dataset(X=SauzeGaugings['H'],Y=SauzeGaugings['Q'],Yu=SauzeGaugings['uQ'],data.dir=workspace)
#' # Parameters of the low flow section control: activation stage k, coefficient a and exponent c
#' k1=parameter(name='k1',init=-0.5,prior.dist='Uniform',prior.par=c(-1.5,0))
#' a1=parameter(name='a1',init=50,prior.dist='LogNormal',prior.par=c(log(50),1))
#' c1=parameter(name='c1',init=1.5,prior.dist='Gaussian',prior.par=c(1.5,0.05))
#' # Parameters of the high flow channel control: activation stage k, coefficient a and exponent c
#' k2=parameter(name='k2',init=1,prior.dist='Gaussian',prior.par=c(1,1))
#' a2=parameter(name='a2',init=100,prior.dist='LogNormal',prior.par=c(log(100),1))
#' c2=parameter(name='c2',init=1.67,prior.dist='Gaussian',prior.par=c(1.67,0.05))
#' # Define control matrix: columns are controls, rows are stage ranges.
#' controlMatrix=rbind(c(1,0),c(0,1))
#' # Stitch it all together into a model object
#' M=model(ID='BaRatin',
#'         nX=1,nY=1, # number of input/output variables
#'         par=list(k1,a1,c1,k2,a2,c2), # list of model parameters
#'         xtra=xtraModelInfo(object=controlMatrix)) # use xtraModelInfo() to pass the control matrix
#' # Call BaM to write configuration files. To actually run BaM, use run=TRUE,
#' # but BaM executable needs to be downloaded first (use downloadBaM())
#' BaM(workspace=workspace,mod=M,data=D,run=FALSE)
#' @export
#' @importFrom utils write.table
BaM <- function(workspace,mod,data,
                remnant=rep(list(remnantErrorModel()),mod$nY),
                mcmc=mcmcOptions(),cook=mcmcCooking(),summary=mcmcSummary(),
                residuals=residualOptions(),pred=NULL,
                doCalib=TRUE,doPred=FALSE,na.value=-9999,
                run=TRUE,preClean=FALSE,
                dir.exe=.BAM_PATH,name.exe='BaM',
                predMaster_fname="Config_Pred_Master.txt",
                stout=""
){
  #oooooooooooooooooooooooooooooooooooooooooo
  # Preliminaries
  if(preClean){file.remove(list.files(workspace,full.names=TRUE))}
  # check length(remnantErrorModel)==mod$nY

  #oooooooooooooooooooooooooooooooooooooooooo
  # Write config files
  quickWrite(toString(mod),workspace,mod$fname)
  npar=length(mod$par)
  if(npar>0){
    for (i in 1:npar){
      p=mod$par[[i]]
      if(is.parameter_VAR(p)){
        writeConfig.parameter_VAR(workspace,paste0('Config_',p$name,'_VAR.txt'),p)
      }
    }
  }
  writeConfig.xtra(workspace,mod)
  quickWrite(toString(data),workspace,data$fname)
  quickWrite(toString(mcmc),workspace,mcmc$fname)
  quickWrite(toString(cook),workspace,cook$fname)
  quickWrite(toString(summary),workspace,summary$fname)
  quickWrite(toString(residuals),workspace,residuals$fname)
  for(i in 1:length(remnant)){
    quickWrite(toString(remnant[[i]]),workspace,remnant[[i]]$fname)
  }
  #oooooooooooooooooooooooooooooooooooooooooo
  # Predictions
  if(!is.null(pred)){
    # Individual prediction experiments
    if(is.prediction(pred)){ # a single prediction experiment
      npred=1
      quickWrite(toString(pred),workspace,pred$fname)
      writePredInputs(pred)
      parSamples=pred$parSamples
    } else { # a list of several prediction experiments
      npred=length(pred)
      parSamples=pred[[1]]$parSamples
      for(i in 1:npred){
        # Verify that all prediction experiments use the same parSamples
        # (possibly the MCMC-generated one when parSamples==NULL)
        if(!identical(pred[[i]]$parSamples,parSamples)){
          stop("All prediction experiments should use the same `parSamples` (possibly the MCMC-generated one when `parSamples`==NULL)",call.=FALSE)
        }
        quickWrite(toString(pred[[i]]),workspace,pred[[i]]$fname)
        writePredInputs(pred[[i]])
      }
    }
    # Master prediction file
    expFiles=getNames(pred,name='fname')
    if(any(duplicated(expFiles))){
      warning('Duplicated prediction experiments.')
    }
    val <- vector(mode='list',length=npred+1)
    val[[1]] <- npred
    for(i in 1:npred){val[[i+1]] <- expFiles[i]}
    comment <- c('Number of prediction experiments',
                 paste0('Config file for experiment ',1:npred))
    txt <- toString_engine(val,comment)
    quickWrite(txt=txt,dir=workspace,fname=predMaster_fname)
  }
  runO <- runOptions(doMCMC=doCalib,doSummary=doCalib,
                  doResiduals=doCalib,doPrediction=doPred)
  quickWrite(toString(runO),workspace,runO$fname)

  #oooooooooooooooooooooooooooooooooooooooooo
  # Write data file
  foo=data$data
  foo[is.na(foo)] <- na.value
  utils::write.table(foo,sep='\t',quote=FALSE,
                     file=data$data.file,row.names=FALSE)

  #oooooooooooooooooooooooooooooooooooooooooo
  # General controller
  rem.txt <- c()
  for(i in 1:length(remnant)){rem.txt <- c(rem.txt,remnant[[i]]$fname)}
  val <- list(paste0(workspace,.Platform$file.sep),
              runO$fname,mod$fname,mod$xtra$fname,
              data$fname,rem.txt,mcmc$fname,cook$fname,
              summary$fname,residuals$fname,predMaster_fname)
  comment <- c('Workspace','Config file: run options','Config file: model',
               'Config file: xtra model information','Config file: Data',
               'Config file: Remnant sigma (as many files as there are output variables separated by commas)',
               'Config file: MCMC','Config file: cooking of MCMC samples',
               'Config file: summary of MCMC samples','Config file: residual analysis',
               'Config file: prediction experiments (master file)')
  txt <- toString_engine(val,comment)
  quickWrite(txt=txt,dir=workspace,fname="Config_BaM.txt")

  #oooooooooooooooooooooooooooooooooooooooooo
  # Run exe

  # If a prediction provides its own parSamples, need to backup-overwrite-restore cooked MCMC file
  if(!is.null(pred)){
    if(!is.null(parSamples)){
      # backup
      invisible(file.copy(from=file.path(workspace,cook$result.fname),
                          to=file.path(workspace,paste0('BACKUP_',cook$result.fname))))
      # overwrite
      utils::write.table(parSamples,file.path(workspace,cook$result.fname),row.names=FALSE)
    }
  }

  res=0
  if(run){
    ok=foundBaM(exedir=dir.exe,exename=name.exe)
    if(!ok){
      message(paste0('BaM executable was not found in folder: ',dir.exe,
                     '. Call function downloadBaM("destination/folder/on/your/computer") to download it.'))
      return()
    }
    res=try(runExe(exedir=dir.exe,exename=name.exe,workspace=workspace,stout=stout))
  }
  if(res!=0){stop('BaM executable crashed with error code: ',res,call.=FALSE)}

  # If a prediction provides its own parSamples, need to cleanup
  if(!is.null(pred)){
    if(!is.null(parSamples)){
      # restore
      invisible(file.copy(to=file.path(workspace,cook$result.fname),
                          from=file.path(workspace,paste0('BACKUP_',cook$result.fname)),
                          overwrite=TRUE))
      # cleanup
      invisible(file.remove(file.path(workspace,paste0('BACKUP_',cook$result.fname))))
    }
  }
}

#*******************************************************************************
#' Run Model
#'
#' Perform a single run of a model, using the initial values specified for the
#' model's parameters.
#'
#' @param workspace Character, directory where config and result files are stored.
#'     workspace = tempdir() is recommended.
#' @param mod model object, the model to be run.
#' @param X data frame, containing the inputs of the model
#' @param na.value numeric, value used by BaM to denote impossible runs, that will be changed to NA in RBaM.
#' @param run Logical, run the model? if FALSE, just write config files and returns NULL.
#' @param preClean Logical, start by cleaning up workspace?
#'  Be careful, this will delete all files in the workspace, including old results!
#' @param dir.exe Character, directory where BaM executable stands.
#' @param name.exe Character, name of the executable without extension ('BaM' by default).
#' @param stout Character string, standard output (see ?system2).
#'     In particular, stout="" (default) shows BaM messages in the console,
#'     stout=NULL discards BaM messages, stout='log.txt' saves BaM messages in file "log.txt".
#' @param Inputs_fname Character, name of configuration file used to specify the format of X.
#' @param X_fname Character, name of file containing a copy of X.
#' @param Y_fname Character, name of file where simulations are written.
#' @return A data frame containing the outputs simulated by the model.
#' @examples
#' # Rating curve model - see https://github.com/BaM-tools/RBaM
#' # Parameters of the low flow section control: activation stage k, coefficient a and exponent c
#' k1=parameter(name='k1',init=-0.5)
#' a1=parameter(name='a1',init=50)
#' c1=parameter(name='c1',init=1.5)
#' # Parameters of the high flow channel control: activation stage k, coefficient a and exponent c
#' k2=parameter(name='k2',init=1)
#' a2=parameter(name='a2',init=100)
#' c2=parameter(name='c2',init=1.67)
#' # Define control matrix: columns are controls, rows are stage ranges.
#' controlMatrix=rbind(c(1,0),c(0,1))
#' # Stitch it all together into a model object
#' M=model(ID='BaRatin',
#'         nX=1,nY=1, # number of input/output variables
#'         par=list(k1,a1,c1,k2,a2,c2), # list of model parameters
#'         xtra=xtraModelInfo(object=controlMatrix)) # use xtraModelInfo() to pass the control matrix
#' # Define the model input
#' X=data.frame(stage=seq(-1,7,0.1))
#' # Write the config files used to run the model. To actually run it,
#' # use run=TRUE, but BaM executable needs to be downloaded first (use downloadBaM())
#' runModel(workspace=tempdir(),mod=M,X=X,run=FALSE)
#' @export
runModel <- function(workspace,mod,X,
                     na.value=-666.666,
                     run=TRUE,preClean=FALSE,
                     dir.exe=.BAM_PATH,name.exe='BaM',
                     stout="",
                     Inputs_fname='Config_Inputs.txt',
                     X_fname='X.txt',Y_fname='Y.txt'){
  #oooooooooooooooooooooooooooooooooooooooooo
  # Preliminaries
  if(preClean){file.remove(list.files(workspace,full.names=TRUE))}
  # check length(remnantErrorModel)==mod$nY

  #oooooooooooooooooooooooooooooooooooooooooo
  # Write config files
  quickWrite(toString(mod),workspace,mod$fname)
  writeConfig.xtra(workspace,mod)
  Xf=file.path(workspace,X_fname)
  # - ADD HEADERS
  utils::write.table(X,sep='\t',quote=FALSE,file=Xf,col.names=FALSE,row.names=FALSE)
  value=list(normalizePath(Xf),0,nrow(X),ncol(X))
  # XXX
  comment=c(
    'Full path to data file',
    'number of header lines',
    'Nobs, number of rows in data file (excluding header lines)',
    'number of columns in data file'
  )
  txt<-toString_engine(value,comment)
  quickWrite(txt,workspace,Inputs_fname)

  #oooooooooooooooooooooooooooooooooooooooooo
  # General controller
  val <- list(paste0(normalizePath(workspace),.Platform$file.sep),
              mod$fname,mod$xtra$fname,Inputs_fname)
  comment <- c('Workspace','Config file: model',
               'Config file: xtra model information',
               'Config file: Input Data')
  txt <- toString_engine(val,comment)
  quickWrite(txt=txt,dir=workspace,fname="Config_BaM.txt")

  #oooooooooooooooooooooooooooooooooooooooooo
  # Run exe
  res=0
  if(run){
    ok=foundBaM(exedir=dir.exe,exename=name.exe)
    if(!ok){
      message(paste0('BaM executable was not found in folder: ',dir.exe,
                     '. Call function downloadBaM("destination/folder/on/your/computer") to download it.'))
      return()
    }
    arg=paste('--config',
              addQuotes(file.path(workspace,'Config_BaM.txt')),
              '--onerun',
              addQuotes(Y_fname))
    res=try(runExe(exedir=dir.exe,exename=name.exe,workspace=workspace,
                   arguments=arg,stout=stout))
    if(res!=0){stop('BaM executable crashed with error code: ',res,call.=FALSE)}
    Y=read.table(file=file.path(workspace,Y_fname),header=TRUE)
    Y[Y==na.value]=NA
  } else {Y=NULL}
  return(Y)
}


#*******************************************************************************
#' MCMC Reader
#'
#' Read raw MCMC samples, return cooked (burnt & sliced) ones
#'
#' @param file Character, full path to MCMC file.
#' @param burnFactor Numeric, burn factor. 0.1 means the first 10% iterations
#'    are discarded.
#' @param slimFactor Integer, slim factor. 10 means that only one iteration
#'    every 10 is kept.
#' @param sep Character, separator used in MCMC file.
#' @param reportFile Character, full path to pdf report file, not created if NULL
#' @param panelPerCol Integer, max number of panels per column
#' @param panelHeight Numeric, height of each panel
#' @param panelWidth Numeric, width of each panel
#' @return A data frame containing the cooked mcmc samples.
#' @examples
#'# Create Monte Carlo samples and write them to file
#' n=4000
#' sim=data.frame(p1=rnorm(n),p2=rlnorm(n),p3=runif(n))
#' workspace=tempdir()
#' write.table(sim,file=file.path(workspace,'MCMC.txt'),row.names=FALSE)
#' # Read file, burn the first half and keep every other row
#' M=readMCMC(file=file.path(workspace,'MCMC.txt'),burnFactor=0.5,slimFactor=2)
#' dim(M)
#' @export
#' @importFrom utils read.table
#' @importFrom grDevices pdf dev.off
#' @importFrom gridExtra grid.arrange
readMCMC <- function(file='Results_Cooking.txt',burnFactor=0,slimFactor=1,sep='',
                     reportFile=NULL,
                     panelPerCol=10,panelHeight=3,panelWidth=23/panelPerCol){
  # read file
  raw <- utils::read.table(file,header=TRUE,sep=sep)
  n <- NROW(raw);p <- NCOL(raw)
  keep <- seq(max(floor(n*burnFactor),1),n,slimFactor)
  cooked <- raw[keep,]
  if(!is.null(reportFile)){
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      ncol <- ifelse(p>=panelPerCol,panelPerCol,p)
      grDevices::pdf(file=reportFile,width=ncol*panelWidth,
          height=ceiling(p/panelPerCol)*panelHeight,useDingbats=FALSE)
      gList <- vector("list",p)
      colors <- rep('black',p)
      for(i in 1:p){
        gList[[i]] <- tracePlot(sim=raw[,i],ylab=names(raw)[i],
                                keep=keep,col=colors[i])
      }
      gridExtra::grid.arrange(grobs=gList,ncol=ncol)
      grDevices::dev.off()
    } else {
      warning(paste('Package gridExtra is required for reporting.',
              'Report file is not created'))
    }
  }
  return(cooked)
}

#***************************************************************************----
# Plotting functions  ----

#*******************************************************************************
#' MCMC reporting
#'
#' 2DO (adapt from STooDs): Generate pdf report files summarizing mcmc samples
#'

#*******************************************************************************
#' tracePlot
#'
#' returns a trace plot ggplot (or a list thereof if several columns in sim)
#'
#' @param sim vector or matrix or data frame, MCMC simulations
#' @param ylab Character, label of y-axis to be used if sim has no names
#' @param keep Integer vector, indices of samples to be kept in cooked MCMC sample
#' @param col Color
#' @param psize Numeric, point size
#' @return A ggplot (or a list thereof if several columns in sim)
#' @examples
#' # Create Monte Carlo samples
#' n=1000
#' sim=data.frame(p1=rnorm(n),p2=rlnorm(n),p3=runif(n))
#' # create trace plot for each component
#' figures=tracePlot(sim)
#' @export
#' @import ggplot2
#' @importFrom rlang .data
tracePlot <- function(sim,ylab='values',keep=NULL,col='black',psize=0.5){
  p <- NCOL(sim)
  g <- vector("list",p)
  yl <- names(sim)
  if(is.null(yl)) {yl <- rep(ylab,p)}
  for(i in 1:p){
    DF <- data.frame(x=1:NROW(sim),y=as.data.frame(sim)[,i])
    if(is.null(keep)){lcol <- col} else {lcol <- 'lightgray'}
    g[[i]] <- ggplot()+
      scale_x_continuous('Iteration')+
      scale_y_continuous(yl[i])+
      geom_line(data=DF,aes(x=.data$x,y=.data$y),col=lcol)
    if(!is.null(keep)){
      g[[i]] <- g[[i]] + geom_point(data=DF[keep,],aes(x=.data$x,y=.data$y),col=col,size=psize) +
        geom_vline(xintercept=keep[1],col='red')
    }
    g[[i]] <- g[[i]] + theme_bw()
  }
  if(p==1){return(g[[1]])} else {return(g)}
}

#*******************************************************************************
#' densityPlot
#'
#' returns a histogram+density ggplot (or a list thereof if several columns in sim)
#'
#' @param sim vector or matrix or data frame, MCMC simulations
#' @param xlab Character, label of x-axis to be used if sim has no names
#' @param col Color
#' @return A ggplot (or a list thereof if several columns in sim)
#' @examples
#' # Create Monte Carlo samples
#' n=1000
#' sim=data.frame(p1=rnorm(n),p2=rlnorm(n),p3=runif(n))
#' # create density plot for each component
#' figures=densityPlot(sim)
#' @export
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom stats density
densityPlot <- function(sim,xlab='values',col='black'){
  p <- NCOL(sim)
  g <- vector("list",p)
  xl <- names(sim)
  if(is.null(xl)) {xl <- rep(xlab,p)}
  for(i in 1:p){
    DF <- data.frame(val=as.data.frame(sim)[,i])
    g[[i]] <- ggplot(DF,aes(.data$val))+
      scale_x_continuous(xl[i])+
      scale_y_continuous('posterior pdf')+
      geom_histogram(aes(y=after_stat(density)),fill=NA,col='black',
                     binwidth=(max(DF$val)-min(DF$val))/30)+
      geom_density(fill=col,alpha=0.5,col=NA)

    g[[i]] <- g[[i]] + theme_bw()
  }
  if(p==1){return(g[[1]])} else {return(g)}
}

#*******************************************************************************
#' violinPlot
#'
#' returns a violinplot ggplot
#'
#' @param sim vector or matrix or data frame, MCMC simulations
#' @param ylab Character, label of y-axis
#' @param col Color
#' @return A ggplot
#' @examples
#' # Create Monte Carlo samples
#' n=1000
#' sim=data.frame(p1=rnorm(n),p2=rlnorm(n),p3=runif(n))
#' # create violin plot comparing all components
#' figure=violinPlot(sim)
#' @export
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom stats reorder
violinPlot <- function(sim,ylab='values',col='black'){
  if (requireNamespace("tidyr",quietly=TRUE)) {
    DF=tidyr::gather(as.data.frame(sim))
    DF$key2 <- stats::reorder(DF$key, 1:NROW(DF)) # avoids violins being re-ordered alphabetically
    g <- ggplot(DF,aes(x=.data$key2,y=.data$value))+
      geom_violin(fill=col)+
      scale_x_discrete('Index',labels=1:NCOL(sim))+
      scale_y_continuous(ylab)
    g <- g + theme_bw()
    return(g)
  } else {
    warning('Package tidyr is required to create this plot.')
  }
}

#***************************************************************************----
# Catalogue ----

#*******************************************************************************
#' Get parameter names
#'
#' Get parameter names for a distribution d
#'
#' @param d Character (possibly vector), distribution (possibly distributions)
#' @return A character vector with parameter names.
#' @examples
#' parnames <- getParNames('GEV')
#' npar <- length(getParNames('Gumbel'))
#' @export
getParNames<-function(d){
  names=c()
  for(i in 1:length(d)){
    name=switch(d[i],
                'Gaussian'=c('mean','sd'),
                'Uniform'=c('lowBound','highBound'),
                'Triangle'=c('peak','lowBound','highBound'),
                'LogNormal'=c('meanlog','sdlog'),
                'LogNormal3'=c('threshold','meanlogexcess','sdlogexcess'),
                'Exponential'=c('threshold','scale'),
                'GPD'=c('threshold','scale','shape'),
                'Gumbel'=c('location','scale'),
                'GEV'=c('location','scale','shape'),
                'GEV_min'=c('location','scale','shape'),
                'Inverse_Chi2'=c('dof','scale'),
                'PearsonIII'=c('location','scale','shape'),
                'Geometric'=c('prob'),
                'Poisson'=c('rate'),
                'Bernoulli'=c('prob'),
                'Binomial'=c('prob','ntrials'),
                'NegBinomial'=c('prob','nfails'),
                'FlatPrior'=c(),
                'FlatPrior+'=c(),
                'FlatPrior-'=c(),
                'FIX'=c(),
                'VAR'=c(),
                NA)
    names=c(names,name)
  }
  return(names)
}

#***************************************************************************----
# Misc. ----

#*******************************************************************************
#' Get object names
#'
#' getNames from an object or a list of objects having a $name field (e.g. parameters)
#' @param loo List Of Objects
#' @param name character, string denoting the name field
#' @return A character vector containing names
#' @examples
#' pars <- list(parameter(name='par1',init=0),
#'              parameter(name='par2',init=0),
#'              parameter(name='Third parameter',init=0))
#' getNames(pars)
#' @export
getNames<-function(loo,name='name'){
  if(is.null(loo)) {return(NULL)}
  if(!is.null(loo[[name]])){return(loo[[name]])}
  n=length(loo)
  if(n==0){return(NULL)}
  txt=vector("character",n)
  for(i in 1:n){
    foo=tryCatch(loo[[i]][[name]],error=function(e){NaN})
    if(is.null(foo)){txt[i]=NaN} else {txt[i]=foo}
  }
  return(txt)
}
