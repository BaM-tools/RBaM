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
    RBaMdir=find.package("RBaM")[1]
    mess=paste0('Path to BaM executable has not been defined yet.',
                '\n\nIf BaM executable is already installed on your computer, use ',
                'setPathToBaM(folder_containing_BaM_on_your_computer) ',
                'to define its location.',
                '\n\nOtherwise use downloadBaM(destination_folder_on_your_computer) ',
                'to download it now.',
                '\nFor instance, to download it into RBaM installation folder, type:',
                '\ndownloadBaM("',file.path(RBaMdir,'bin'),'")')
  } else {
    ok=foundBaM(exedir=exedir,exename='BaM')
    mess=paste0('BaM executable was not found in the folder: ',exedir,
                '\nTo update BaM location, use setPathToBaM(folder_containing_BaM_on_your_computer).',
                '\nTo re-download BaM executable in this folder, use downloadBaM("',exedir,'")')
  }
  if(!ok){
    packageStartupMessage('~~~~~~~~~~~~~~~~')
    packageStartupMessage('Welcome to RBaM!')
    packageStartupMessage('~~~~~~~~~~~~~~~~')
    packageStartupMessage(mess)
  }
}
