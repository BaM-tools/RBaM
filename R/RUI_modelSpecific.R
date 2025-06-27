#***************************************************************************----
# R User Interface: model-specific functions to be updated for every new model ----

#*******************************************************************************
#' Xtra Model Information
#'
#' Handle Xtra Model Information
#' In particular, write Model Xtra configuration file, but could also run a
#' script to pre-load data, format stuff, etc.
#' @param workspace Character, directory where config and result files are stored.
#' @param mod model object, the model to be calibrated
#' @return Nothing: just write config files
#' @examples
#' # BaRatin model for a single-control rating curve Y=a(X-b)^c
#' workspace=tempdir()
#' mod <- model(ID='BaRatin',nX=1,nY=1,
#'              par=list(parameter('a',10,prior.dist='LogNormal',prior.par=c(log(10),0.1)),
#'                       parameter('b',-1,prior.dist='Gaussian',prior.par=c(-1,1)),
#'                       parameter('c',5/3,prior.dist='Gaussian',prior.par=c(5/3,0.05))),
#'              xtra=xtraModelInfo(object=matrix(1,nrow=1,ncol=1)))
#' writeConfig.xtra(workspace,mod)
#' @noRd
#' @keywords internal
#' @importFrom utils write.table
writeConfig.xtra<-function(workspace,mod){
  ID=mod$ID
  x=mod$xtra$object
  fname=mod$xtra$fname
  # Models requiring no xtra information
  if(ID %in% c('Linear','SGD','SFDTidal','SFDTidal_Sw_correction','SFDTidal2','SFDTidalJones','SFDTidal4',
               'SFDTidal_Qmec0','SFDTidal_Qmec','SFDTidal_Qmec2','TidalODE','TidalRemenieras',
               'SuspendedLoad','Recession_h','AlgaeBiomass')){
    # Do nothing
  }
  # Models requiring only to write the list in mod$xtra$object into the config file
  if(ID %in% c('SFD','SWOT','Vegetation','DynamicVegetation',
               'Segmentation','Sediment','Mixture',
               'Orthorectification','Tidal')){
    txt<-toString_engine(x,NULL)
    quickWrite(txt,workspace,fname)
  }
  # BaRatin: write control matrix into the config file
  if(ID=='BaRatin'){
    val=list()
    for(i in 1:NCOL(x)){val=c(val,list(x[i,]))}
    txt<-toString_engine(val,NULL)
    quickWrite(txt,workspace,fname)
  }
  # BaRatinBAC: write control matrix + hmax into the config file
  if(ID=='BaRatinBAC'){
    cmat=x[[1]];hmax=x[[2]]
    val=list()
    for(i in 1:NCOL(cmat)){val=c(val,list(cmat[i,]))}
    val=c(val,hmax)
    txt<-toString_engine(val,NULL)
    quickWrite(txt,workspace,fname)
  }
  # General section hydraulic control: write h-A-w matrix into the config file
  if(ID=='HydraulicControl_section'){
    val=list()
    for(i in 1:NROW(x)){val=c(val,list(x[i,]))}
    txt<-toString_engine(val,NULL)
    quickWrite(txt,workspace,fname)
  }
  # GR4J: write path to GR4J setup file
  if(ID=='GR4J'){
    if(is.null(x)){
      f=file.path(path.package('RBaM'),'extdata','GR4J_setupFile.txt')
    } else {f=x}
    txt<-toString_engine(f,NULL)
    quickWrite(txt,workspace,fname)
  }
  # TextFile: write config file
  if(ID=='TextFile'){
    # get all parameter names
    pars=getNames(mod$par)
    # get inputs
    inputs=x$inputs
    # Verify that all variables used in formulas are either parameters or inputs
    for(i in 1:length(x$formulas)){
      checkFormula(f=x$formulas[i],namespace=c(pars,inputs))
    }
    # Build configuration file
    val=list(length(inputs),inputs,length(pars),pars,length(x$formulas))
    comments=c('number of input variables',
               'list of input variables, comma-separated ',
               'number of parameters',
               'list of parameters, comma-separated ',
               'number of output variables')
    for(i in 1:length(x$formulas)){
      val=c(val,x$formulas[i])
      comments=c(comments,paste('Formula',i))
    }
    txt<-toString_engine(val,comments,addQuote=FALSE)
    quickWrite(txt,workspace,fname)
  }
  # MAGE: xtra$object is a named list, xtra$object = list(exeFile=...,version=...,mageDir=...,repFile=...,zKmin=...,zFileKmin=...,doExpKmin=...,zKmoy=...,zFileKmoy=...,doExpKmoy=...), with components:
  # exeFile [string]: full path to MAGE executable
  # version [string]: MAGE version
  # mageDir [string]: full path to MAGE project directory
  # repFile [string]: Name of the .REP file (located in MAGE project directory - file name only, not full path)
  # zKmin [dataframe]: [n*p] design matrix for computing Kmin. The [n*1] values passed to MAGE through the .RUG file are computed by the matrix product zKmin*theta , where theta is the [p*1] parameter vector seen and estimated by BaM
  # zFileKmin [string]: File (full path) where zKmin will be written
  # doExpKmin [logical]: if TRUE, use an exponential transformation to ensure positivity, i.e. Kmin=exp(Z*theta)
  # zKmoy, zFileKmoy, doExpKmoy: same as the 3 lines above, but for Kmoy instead of Kmin
  if(ID=='MAGE'){
    val=list(x$exeFile,x$version,x$mageDir,x$repFile,
             x$zFileKmin,NCOL(x$zKmin),x$doExpKmin,
             x$zFileKmoy,NCOL(x$zKmoy),x$doExpKmoy)
    comments=c('Mage executable (full path)',
               'MAGE version',
               'MAGE Project directory (full path)',
               'REP file (located in MAGE project directory - file name only, not full path)',
               'Z file (full path) containing covariates for the regression Kmin(x)=a1Z1(x)+...+apZp(x). Leave empty for no regression',
               'number of columns p in Z file',
               'apply exponential transformation to computed Kmins to ensure positivity?',
               'Z file (full path) containing covariates for the regression Kmoy(x)=a1Z1(x)+...+apZp(x). Leave empty for no regression',
               'number of columns p in Z file',
               'apply exponential transformation to computed Kmoys to ensure positivity?')
    txt<-toString_engine(val,comments)
    quickWrite(txt,workspace,fname)
    # Write zKmin and zKmoy data frames to files
    utils::write.table(x$zKmin,file=x$zFileKmin,row.names=FALSE)
    utils::write.table(x$zKmoy,file=x$zFileKmoy,row.names=FALSE)
  }
  # MAGE_TEMP: xtra$object is a named list, xtra$object = list(exeFile=...,version=...,mageDir=...,repFile=...,zKmin=...,zFileKmin=...,doExpKmin=...,zKmoy=...,zFileKmoy=...,doExpKmoy=...,mage_extraire_file=...,mage_extraire_args=...), with components:
  # exeFile [string]: full path to MAGE executable
  # version [string]: MAGE version
  # mageDir [string]: full path to MAGE project directory
  # repFile [string]: Name of the .REP file (located in MAGE project directory - file name only, not full path)
  # zKmin [dataframe]: [n*p] design matrix for computing Kmin. The [n*1] values passed to MAGE through the .RUG file are computed by the matrix product zKmin*theta , where theta is the [p*1] parameter vector seen and estimated by BaM
  # zFileKmin [string]: File (full path) where zKmin will be written
  # doExpKmin [logical]: if TRUE, use an exponential transformation to ensure positivity, i.e. Kmin=exp(Z*theta)
  # zKmoy, zFileKmoy, doExpKmoy: same as the 3 lines above, but for Kmoy instead of Kmin
  # mage_extraire_file [string]: full path to mage_extraire exe
  # mage_extraire_args [string vector]: command line arguments of mage_extraire for each extracted series
  if(ID=='MAGE_TEMP'){
    val=list(x$exeFile,x$version,x$mageDir,x$repFile,
             x$zFileKmin,NCOL(x$zKmin),x$doExpKmin,
             x$zFileKmoy,NCOL(x$zKmoy),x$doExpKmoy,
             x$mage_extraire_file,length(x$mage_extraire_args),x$mage_extraire_args)
    comments=c('Mage executable (full path)',
               'MAGE version',
               'MAGE Project directory (full path)',
               'REP file (located in MAGE project directory - file name only, not full path)',
               'Z file (full path) containing covariates for the regression Kmin(x)=a1Z1(x)+...+apZp(x). Leave empty for no regression',
               'number of columns p in Z file',
               'apply exponential transformation to computed Kmins to ensure positivity?',
               'Z file (full path) containing covariates for the regression Kmoy(x)=a1Z1(x)+...+apZp(x). Leave empty for no regression',
               'number of columns p in Z file',
               'apply exponential transformation to computed Kmoys to ensure positivity?',
               'full path to mage_extraire exe',
               'number of extracted series',
               'arguments of mage_extraire for each extracted series (size = number of extracted series above)')
    txt<-toString_engine(val,comments)
    quickWrite(txt,workspace,fname)
    # Write zKmin and zKmoy data frames to files
    utils::write.table(x$zKmin,file=x$zFileKmin,row.names=FALSE)
    utils::write.table(x$zKmoy,file=x$zFileKmoy,row.names=FALSE)
  }
}

#*******************************************************************************
#' BaM catalogue
#'
#' Distributions and models available in BaM
#'
#' @param printOnly Logical, should the catalogue be returned or only printed?
#' @return If \code{printOnly==FALSE}, a list with the following fields:
#' \describe{
#'   \item{distributions}{available univariate distributions.}
#'   \item{models}{available models.}
#' }
#' @examples
#' catalogue <- getCatalogue()
#' getCatalogue(printOnly=TRUE)
#' @export
getCatalogue<-function(printOnly=FALSE){
  # available distributions
  dist=c('Gaussian','Uniform','Triangle','LogNormal','LogNormal3',
         'Exponential','GPD','Gumbel','GEV','GEV_min','Inverse_Chi2','PearsonIII',
         'Geometric','Poisson','Bernoulli','Binomial','NegBinomial',
         'FlatPrior','FlatPrior+','FlatPrior-','FIX','VAR')
  models=c('TextFile',
           'BaRatin','BaRatinBAC','SFD','SGD','SWOT',
           'Vegetation','AlgaeBiomass','DynamicVegetation',
           'Recession_h','Segmentation',
           'Sediment','SuspendedLoad',
           'Linear','Mixture','Orthorectification','GR4J',
           'Tidal','SFDTidal','SFDTidal2','SFDTidalJones','SFDTidal4',
           'SFDTidal_Qmec0','SFDTidal_Qmec','SFDTidal_Qmec2',
           'TidalODE','TidalRemenieras','SFDTidal_Sw_correction',
           'MAGE','MAGE_TEMP','HydraulicControl_section')
  if(printOnly){
    message('DISTRIBUTIONS:')
    print(dist)
    message('MODELS:')
    print(models)
  } else{
    return(list(distributions=dist,models=models))
  }
}
