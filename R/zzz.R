.onAttach <- function(libname, pkgname) {
  exedir=file.path(find.package('RBaM'),'bin')
  ok=foundBaM(exedir=exedir,exename='BaM')
  if(!ok){
    mess=paste0('BaM executable was not found in your RBaM directory: ',exedir)
    packageStartupMessage('Welcome to RBaM!')
    packageStartupMessage(mess)
    packageStartupMessage('Use downloadBaM() to download it now.')
  }
}
