# This file contains various private utilities that are NOT meant to be exported

#*******************************************************************************
#' Write value-comment pairs into string vector
#' @param val List, object/list of values to be printed
#' @param comment Character vector, associated comments (same length as val)
#' @param addQuote Logical, ass double quotes to strings?
#' @return A character vector, ready to be printed of written to file
#' @keywords internal
toString_engine<-function(val,comment,addQuote=TRUE){
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
      # collapse foo
      foo=paste(foo,collapse=',')
      # add trailing white spaces
      nw=max(58-nchar(foo),0)
      ws=paste0(rep(' ',nw),collapse='')
      foo=paste0(foo,ws)
      # add comment
      txt[i]=paste(foo,'!',comment[i])
    }
  }
  return(txt)
}

#*******************************************************************************
#' Add double quotes to a string or to each element of a string vector
#' @param txt Character vector
#' @return The double-quoted character vector
#' @keywords internal
addQuotes<-function(txt){
  n=length(txt)
  out=txt
  for(i in 1:n){out[i]=paste0('"',txt[i],'"')}
  return(out)
}

#*******************************************************************************
#' Write a character vector (typically output of toString() to file)
#' @param txt Character vector, text to be written
#' @param dir Character, directory where file is written. The directory is
#'   created if needed. dir should end with a path seperator (not checked here).
#' @param fname Character, file name
#' @return Nothing
#' @keywords internal
#' @importFrom utils write.table
quickWrite <- function(txt,dir,fname){
  if(!dir.exists(dir)){dir.create(dir,recursive=TRUE)}
  file=file.path(dir,fname)
  utils::write.table(matrix(txt, ncol = 1, nrow = length(txt)), file = file,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

#*******************************************************************************
#' Check a formula make sense within a given namespace
#' @param f Character string, formula
#' @param namespace Character vector, namespace (i.e. names of known variables)
#' @return A list with the following fields:
#' \describe{
#'   \item{ok}{Does the formula make sense?}
#'   \item{mess}{message}
#' }
#' @keywords internal
checkFormula<-function(f,namespace){
  # initialise
  ok=TRUE;mess='';unknown=c()
  # Get variables used in the formula
  vars <- all.vars(parse(text=f))
  # Check all variables are in the namespace
  n <- length(vars)
  for(i in 1:n){
    if(!vars[i]%in%namespace){unknown=c(unknown,vars[i])}
  }
  # build result list
  ok=(length(unknown)==0)
  if(!ok){
    mess=paste0('unknown variable used in formula: ',
                paste0(unknown,collapse=','))
  }
  return(list(ok=ok,mess=mess))
}

#*******************************************************************************
#' Run BaM executable
#' @param exedir Character string, directory of the executable
#' @param exename Character string, name of the executable
#' @param workspace Character string, full path to workspace
#' @param arguments Character string, arguments to be passed to BaM exe
#' @param stout Character string, standard output (see ?system2).
#' In particular, stout="" (default) shows BaM messages in the console,
#' stout=NULL discards BaM messages,
#' stout='log.txt' saves BaM messages in file "log.txt".
#' @return an error code (0 for success)
#' @keywords internal
runExe<-function(exedir,exename,workspace,arguments=NULL,stout=""){
  saveWD <- getwd() # remember current working dir
  on.exit(setwd(saveWD)) # make sure it will be restored even if the function crashes
  setwd(exedir) # need to set working dir to the exe dir
  os=Sys.info()['sysname'] # determine OS
  cmd=ifelse(os=='Windows',
             paste0(exename,'.exe'), # Windows command
             paste0('./',exename) # Linux command
  )
  configFile=file.path(workspace,'Config_BaM.txt')
  if(is.null(args)){
    arg=paste('--config', addQuotes(configFile))
  } else {
    arg=arguments
  }
  res=system2(cmd, arg, stdout = stout,input=" ") # run exe
  setwd(saveWD) # move back to initial working directory
  return(res)
}

#*******************************************************************************
#' Was BaM executable found?
#' @param exedir directory where the executable should be searched
#' @param exename name of the executable
#' @return a logical, TRUE if the exe was found, false otherwise
#' @keywords internal
foundBaM <- function(exedir,exename){
  out=file.exists(file.path(exedir,exename)) |
    file.exists(file.path(exedir,paste0(exename,'.exe')))
}
