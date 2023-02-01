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
writeConfig.xtra<-function(workspace,mod){
  ID=mod$ID
  x=mod$xtra$object
  fname=mod$xtra$fname
  # Models requiring no xtra information
  if(ID %in% c('Linear','SGD','SFDTidal','SFDTidal_Sw_correction','SFDTidal2','SFDTidalJones','SFDTidal4',
               'TidalODE','TidalRemenieras','SuspendedLoad','Recession_h','AlgaeBiomass')){
    # Do nothing
  }
  # Models requiring only to write the list in mod$xtra$object into the config file
  if(ID %in% c('SFD','SWOT','Vegetation','DynamicVegetation',
               'Segmentation','Sediment','Mixture',
               'Orthorectification','Tidal','MAGE')){
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
           'TidalODE','TidalRemenieras','SFDTidal_Sw_correction','MAGE')
  if(printOnly){
    message('DISTRIBUTIONS:')
    print(dist)
    message('MODELS:')
    print(models)
  } else{
    return(list(distributions=dist,models=models))
  }
}
