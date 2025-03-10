#***************************************************************************----
# Constructor ----
#' prediction object constructor.
#'
#' Creates a new instance of a 'prediction' object
#'
#' @param X data frame or list of dataframes / matrices, representing the
#'     values taken by the input variables.
#'     \itemize{
#'       \item If X is a dataframe, then each column is interpreted as one input variable, and consequently
#'       inputs are not replicated (=> no input uncertainty).
#'       \item If X is a list, then each element of the list is a matrix associated with one input variable, and
#'       the columns of this matrix are replications (=> input uncertainty is propagated). All matrices
#'       in the list should have the same number of rows, columns are recycled if needed.
#'     }
#' @param spagFiles Character vector (size nY, the number of output variables). Name of the files containing the spaghettis
#'     for each output variable. NOTE: provide file names only, not full paths.
#'     Using a '.spag' extension is a good practice.
#' @param data.dir Character, directory where a copies of the dataset X will be
#'     written if required (1 file per variable in X). Default is the current working directory, but you
#'     may prefer to use the BaM workspace.
#' @param data.fnames Character, data file names.
#' @param fname Character, configuration file name.
#' @param doParametric Logical, propagate parametric uncertainty?
#'     If FALSE, maxpost parameters are used.
#' @param doStructural Logical, propagate structural uncertainty for each output variable? (size nY)
#' @param transposeSpag Logical. If FALSE, spaghettis are written horizontally (row-wise), otherwise
#'     they will be transposed so that each spaghetti is a column.
#' @param priorNsim Integer, number of samples from the prior distribution for 'prior prediction' experiments.
#'     If negative or NULL (default), posterior samples are used.
#' @param envFiles Character vector (size nY, the number of output variables). Name of the files containing the
#'     envelops (e.g. prediction intervals) computed from the spaghettis for each output variable.
#'     By default, same name as spaghetti files but with a '.env' extension.
#'     If NULL, envelops are not computed.
#' @param consoleProgress Logical, print progress in BaM.exe console?
#' @param spagFiles_state Character vector (size nState, the number of state variables),
#'     same as spagFiles but for states rather than outputs. If NULL, states are not predicted.
#'     Note that only parametric uncertainty is propagated for state variables since they are not observed.
#'     Consequently, structural uncertainty = 0 and total uncertainty = parametric uncertainty.
#' @param transposeSpag_state Logical. Same as transposeSpag, but for states rather than outputs.
#' @param envFiles_state Character vector (size nState, the number of state variables),
#'     same as envFiles, but for states rather than outputs.
#' @param parSamples data frame, parameter samples that will replace the MCMC-generated one for this prediction.
#' @return An object of class 'prediction'.
#' @examples
#' #--------------
#' # Example using the twoPopulations dataset, containing 101 values for
#' # 3 input variables (time t, temperature at site 1 T1, temperature at site 2 T2)
#' # and 2 output variables (population at site 1 P1, population at site 2 P2).
#' pred=prediction(X=twoPopulations[,1:3],spagFiles=c('P1.spag','P2.spag'))
#' #--------------
#' # Alternative example showing how to propagate uncertainty in some of
#' # the input variables (here, temperatures T1 and T2)
#' # Create 100 noisy replicates for T1, representing uncertainty
#' T1rep=matrix(rnorm(101*100,mean=twoPopulations$T1,sd=0.1),nrow=101,ncol=100)
#' # Same for T2
#' T2rep=matrix(rnorm(101*100,mean=twoPopulations$T2,sd=0.1),nrow=101,ncol=100)
#' # Create prediction object
#' pred=prediction(X=list(twoPopulations$t,T1rep,T2rep),spagFiles=c('P1.spag','P2.spag'))
#' @export
prediction<-function(X,spagFiles,
                     data.dir=getwd(),data.fnames=paste0('X',1:length(X),'.pred'),
                     fname=paste0('Config_Pred_',paste0(sample(c(letters,LETTERS,0:9),6),collapse=''),'.txt'),
                     doParametric=FALSE,doStructural=rep(FALSE,length(spagFiles)),transposeSpag=TRUE,priorNsim=NULL,
                     envFiles=paste0(tools::file_path_sans_ext(spagFiles),'.env'),
                     consoleProgress=TRUE,spagFiles_state=NULL,transposeSpag_state=TRUE,
                     envFiles_state=switch(is.null(spagFiles_state)+1,
                                          paste0(tools::file_path_sans_ext(spagFiles_state),'.env'),
                                          NULL), # dirty trick because ifelse cannot return NULL - see https://stackoverflow.com/questions/45814810/returning-null-using-ifelse-function
                     parSamples=NULL){
  o<-new_prediction(X,spagFiles,data.dir,data.fnames,fname,doParametric,doStructural,transposeSpag,priorNsim,
                    envFiles,consoleProgress,spagFiles_state,transposeSpag_state,envFiles_state,parSamples)
  return(validate_prediction(o))
}

#***************************************************************************----
# toString function ----
#' prediction to string
#'
#' Convert an object of class 'prediction' into a ready-to-write vector of string
#'
#' @param x prediction object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' pred=prediction(X=twoPopulations[,1:3],spagFiles=c('P1.spag','P2.spag'))
#' toString(pred)
#' @export
toString.prediction<-function(x,...){
  Nobs=NROW(x$X[[1]])
  if(is.data.frame(x$X)){
    Nspag=rep(1,length(x$X))
  } else {
    Nspag=sapply(x$X,NCOL)
  }
  nY=length(x$spagFiles)
  nState=length(x$spagFiles_state)
  if(is.null(x$priorNsim)){Nsim=-1} else {Nsim=x$priorNsim}
  doEnv=rep(!is.null(x$envFiles),nY)
  if(is.null(x$spagFiles_state)){doState=FALSE} else {doState=rep(TRUE,nState)}
  if(is.null(x$envFiles_state)){doEnv_state=FALSE} else {doEnv_state=rep(TRUE,nState)}
  value=list(x$data.files,Nobs,Nspag,x$doParametric,x$doStructural,
             Nsim,x$spagFiles,rep(x$transposeSpag,nY),doEnv,x$envFiles,
             x$consoleProgress,doState,x$spagFiles_state,
             rep(x$transposeSpag_state,nState),
             doEnv_state,x$envFiles_state)
  comment=c(
    'Files containing spaghettis for each input variable (size nX)',
    'Nobs, number of observations per spaghetti (common to all files!)',
    'Nspag, number of spaghettis for each input variable (size nX)',
    'Propagate parametric uncertainty?',
    'Propagate remnant uncertainty for each output variable? (size nY)',
    'Nsim[prior]. If <=0: posterior sampling (nsim is given by mcmc sample); if >0: sample nsim replicates from prior distribution',
    'Files containing spaghettis for each output variable (size nY)',
    'Post-processing: transpose spag file (so that each column is a spaghetti)? (size nY)',
    'Post-processing: create envelops? (size nY)',
    'Post-processing: name of envelop files (size nY)',
    'Print progress in console during computations?',
    'Do state prediction? (size nState)',
    ' Files containing spaghettis for each state variable (size nState)',
    'Post-processing: transpose spag file (so that each column is a spaghetti)? (size nState)',
    'Post-processing: create envelops? (size nState)',
    'Post-processing: name of envelop files (size nState)'
  )
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# writePredInputs function ----
#' Write prediction inputs
#'
#' Write input data of the prediction into files
#'
#' @param o prediction object
#' @return nothing - just write to files.
#' @examples
#' temp=tempdir()
#' pred=prediction(X=twoPopulations[,1:3],spagFiles=c('P1.spag','P2.spag'),
#'                 data.dir=temp)
#' writePredInputs(pred)
#' @export
#' @importFrom utils write.table
writePredInputs<-function(o){
  for(i in 1:length(o$data.files)){
    utils::write.table(o$X[[i]],sep='\t',quote=FALSE,
                       file=o$data.files[i],
                       row.names=FALSE,col.names=FALSE)
  }
}

#***************************************************************************----
# is function ----
#' prediction tester
#'
#' Is an object of class 'prediction'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'prediction', FALSE otherwise.
#' @keywords internal
is.prediction<-function(o){
  return(class(o)=='prediction')
}

#***************************************************************************----
# internal constructor ----
new_prediction<-function(X,spagFiles,data.dir,data.fname,fname,doParametric,doStructural,
                         transposeSpag,priorNsim,envFiles,consoleProgress,
                         spagFiles_state,transposeSpag_state,envFiles_state,parSamples){
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # basic checks
  stopifnot(is.data.frame(X) | is.list(X))
  stopifnot(is.character(spagFiles))
  stopifnot(is.character(data.dir))
  stopifnot(is.character(data.fname))
  stopifnot(is.character(fname))
  stopifnot(is.logical(doParametric))
  stopifnot(is.logical(doStructural))
  stopifnot(is.logical(transposeSpag))
  if(!is.null(priorNsim)) stopifnot(is.numeric(priorNsim))
  if(!is.null(envFiles)) stopifnot(is.character(envFiles))
  stopifnot(is.logical(consoleProgress))
  if(!is.null(spagFiles_state)) stopifnot(is.character(spagFiles_state))
  stopifnot(is.logical(transposeSpag_state))
  if(!is.null(envFiles_state)) stopifnot(is.character(envFiles_state))
  if(!is.null(parSamples)) stopifnot(is.data.frame(parSamples))
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # data files
  data.files=file.path(R.utils::getAbsolutePath(data.dir),data.fname)
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assemble prediction object
  o <- list(fname=fname,data.files=data.files,
            X=X,spagFiles=spagFiles,doParametric=doParametric,doStructural=doStructural,
            transposeSpag=transposeSpag,priorNsim=priorNsim,envFiles=envFiles,
            consoleProgress=consoleProgress,spagFiles_state=spagFiles_state,
            transposeSpag_state=transposeSpag_state,envFiles_state=envFiles_state,
            parSamples=parSamples)
  class(o) <- 'prediction'
  return(o)
}

#***************************************************************************----
# validator ----
validate_prediction<-function(x){
  if(!is.data.frame(x$X)){
    # each element of the list should have the same number of rows
    p=length(x$X)
    if(p>0){
      n=NROW(x$X[[1]])
      for(i in 1:p){
        if(NROW(x$X[[i]])!=n){
          stop("All input variables in X should have the same number of observations (rows)",call.=FALSE)
        }
      }
    }
  }
  return(x)
}

