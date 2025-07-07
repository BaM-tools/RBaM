.onLoad <- function(libname, pkgname) {
  dir.config=tools::R_user_dir(package="RBaM",which="config")
  fname=file.path(dir.config,'pathToBaM.txt')
  if(file.exists(fname)){
    dir.exe=readLines(fname,n=1)
  } else {
    dir.exe=NULL
  }
  assign('.BAM_PATH',dir.exe, envir = parent.env(environment()))
}

.onAttach <- function(libname, pkgname) {
  ok=TRUE
  exedir=.BAM_PATH
  if(is.null(exedir)){
    ok=FALSE
    mess=paste0('Path to BaM executable has not been defined yet.',
                '\nIf BaM executable is already installed on your computer, use ',
                'setPathToBaM("folder/containing/BaM/on/your/computer") ',
                'to define its location.',
                '\nOtherwise use downloadBaM("destination/folder/on/your/computer") ',
                'to download it now.')
  } else {
    ok=foundBaM(exedir=exedir,exename='BaM')
    mess=paste0('BaM executable was not found in the folder: ',exedir,
                '\nTo update BaM location, use setPathToBaM("folder/containing/BaM/on/your/computer").',
                '\nTo re-download BaM executable in this folder, use downloadBaM("',exedir,'")')
  }
  if(!ok){
    packageStartupMessage('Welcome to RBaM!')
    packageStartupMessage(mess)
  }
}
