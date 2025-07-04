# This file contains various public utilities that are meant to be exported

#*******************************************************************************
#' Bathymetry interpreter
#'
#' Compute Area A(h), width w(h) and wet perimeter P(h) from a bathymetry profile (a,z).
#'
#' @param bathy data frame, 2 columns containing abscissa (increasing values) and stage.
#' @param hgrid numeric vector, grid of h values where A, w and P are computed.
#'              By default 1000 values in the range of bathymetry's z.
#' @param segmentLength numeric, segment length for bathymetry subsampling.
#'              By default 1/1000 of the total bathymetry's perimeter.
#' @return A 4-column dataframe containing h, A(h), w(h) and P(h)
#' @examples
#' bathy=data.frame(a=c(0,0,0,1,2,2,4,6,8),h=c(3,2,0,-0.5,0,2,2.0001,2.3,3))
#' plot(bathy,type='l')
#' df=getAwPfromBathy(bathy)
#' plot(df$h,df$A,type='l')
#' plot(df$h,df$w,type='l')
#' plot(df$h,df$P,type='l')
#' @export
getAwPfromBathy <- function(bathy,
                            hgrid=seq(min(bathy[,2]),max(bathy[,2]),diff(range(bathy[,2]))/1000),
                            segmentLength=sum(sqrt(apply(apply(bathy,2,diff)^2,1,sum)))/1000){
  if(NCOL(bathy)<2) {stop("bathy dataframe should have at least two columns",call.=FALSE)}
  if(any(diff(bathy[,1])<0)) {stop("abscissa in first column of bathy should be increasing",call.=FALSE)}

  # Full perimeter of bathy
  bathyP=sum(sqrt(apply(apply(bathy,2,diff)^2,1,sum)))
  # Estimate number of points of resampled bathy
  n=ceiling(bathyP/segmentLength)+NROW(bathy)
  # initialize
  segments=data.frame(x=rep(NA,n),y=rep(NA,n))
  k=0
  # resample bathy
  for(i in 2:NROW(bathy)){
    dir=as.numeric(bathy[i,]-bathy[i-1,]) # direction of current bathy segment
    le=sqrt(sum(dir^2)) # length of current bathy segment
    le2=seq(0,le,segmentLength) # cumulative length of resampled segments
    m=length(le2) # number of points in resampled segments
    segments[(k+1):(k+m),1]=bathy[i-1,1]+le2*dir[1]/le
    segments[(k+1):(k+m),2]=bathy[i-1,2]+le2*dir[2]/le
    k=k+m
  }
  # add last bathy point and remove NA and duplicates
  segments[k+1,]=bathy[NROW(bathy),]
  segments=segments[!is.na(segments$x),]
  segments=segments[!duplicated(segments),]
  # Compute horizontal, vertical and euclidean length of each small segments, to the right and to the left
  segments$lhleft=c(0,diff(segments$x))/2
  segments$lhright=c(diff(segments$x),0)/2
  segments$lvleft=c(0,diff(segments$y))/2
  segments$lvright=c(diff(segments$y),0)/2
  segments$lleft=sqrt(segments$lhleft^2+segments$lvleft^2)
  segments$lright=sqrt(segments$lhright^2+segments$lvright^2)

  # Create functions for Ah, wh and Ph (to be applied to hgrid)
  fA=function(h){ # sum rectangle areas
    foo=segments[segments$y<h,]
    return(sum((h-foo$y)*(foo$lhright+foo$lhleft)))
  }
  fw=function(h){ # sum horizontal lengths
    foo=segments[segments$y<h,]
    return(sum(foo$lhleft)+sum(foo$lhright))
  }
  fP=function(h){ # sum segment lengths
    foo=segments[segments$y<h,]
    return(sum(foo$lleft)+sum(foo$lright))
  }

  # create final dataframe
  out=data.frame(h=hgrid,A=sapply(hgrid,fA),w=sapply(hgrid,fw),P=sapply(hgrid,fP))
  return(out)
}


#*******************************************************************************
#' BaM downloader
#'
#' Download BaM executable
#'
#' @param destFolder character string, folder where BaM executable will be downloaded.
#' @param url character string, the url from which BaM should be downloaded.
#'     When NULL, the url is determined automatically by using GitHub API to determine
#'     the latest release and the file corresponding to the OS.
#' @param os character string, operating system, e.g. 'Linux', 'Windows' or 'Darwin'.
#' @param quiet logical, if TRUE, suppress status messages.
#' @param ... arguments passed to function `download.file`
#' @return nothing - just download the file.
#' @examples
#'   try(downloadBaM(destFolder=tempdir()))
#' @export
#' @importFrom rjson fromJSON
#' @importFrom utils download.file unzip
downloadBaM <- function(destFolder,url=NULL,os=Sys.info()['sysname'],
                        quiet=FALSE,...){
  if(is.null(os)){stop('Unrecognized OS',call.=FALSE)}
  if(is.null(url)){
    url0='https://api.github.com/repos/BaM-tools/BaM/releases/latest'
    if(!quiet){
      message('----------------')
      message('Determining latest version of BaM exe...')
      message(' ')
    }
    foo=tryCatch(readLines(url0,warn=FALSE),error=function(e){NULL})
    if(is.null(foo)){
      mess=paste0('Could not download GitHub API file at url: ',url0,
                  '. Please check you Internet connection, ',
                  'or try providing the url of the BaM exe to download through the `url=` argument')
      stop(mess,call.=FALSE)
    }
    if(length(foo)>1){foo=paste(foo,collapse='')}
    js=rjson::fromJSON(foo)
    tag=js$tag_name
    allAssets=sapply(js$assets,function(x){x$browser_download})
    k=grep(os,allAssets)
    if(length(k)==0){
      mess=paste0('Could not find a BaM exe for your OS in the latest BaM release. ',
                  'Try providing the url of the BaM exe to download through the `url=` argument')
      stop(mess,call.=FALSE)
    }
    if(length(k)>1){
      mess=paste0('Several BaM exe files match your OS in the latest BaM release. ',
                  'Try providing the url of the BaM exe to download through the `url=` argument')
      stop(mess,call.=FALSE)
    }
    url=allAssets[k]
  }
  if(!quiet){
    message('----------------')
    message('Downloading zip file from: ', url)
    message(' ')
  }
  filename <- basename(url)
  tdir=tempdir()
  ok=utils::download.file(url,destfile=file.path(tdir,filename),...)
  if(ok != 0){
    mess=paste0('Unable to download BaM exe zip file at url: ',url,'...')
    stop(mess,call.=FALSE)
  }
  if(!quiet){
    message('----------------')
    message('Unzipping...')
    message(' ')
  }
  contents=unzip(zipfile=file.path(tdir,filename),list=TRUE)
  ok=unzip(zipfile=file.path(tdir,filename),exdir=destFolder)
  if(os!='Windows'){
    Sys.chmod(file.path(destFolder,contents$Name),mode="0777",use_umask = TRUE)
  }
  if(is.null(ok)){
    mess=paste0('Unable to extract BaM exe from zip file')
    stop(mess,call.=FALSE)
  } else {
    # save destFolder in config file
    setPathToBaM(destFolder,quiet=TRUE)
  }
  if(!quiet){
    message('----------------')
    message('BaM executable was successfully downloaded in folder: ',destFolder)
    message('Please restart a new R session before further using RBaM.')
    message(' ')
  }
}

#*******************************************************************************
#' Path to BaM
#'
#' Set path to BaM executable
#'
#' @param dir.exe character string, folder where BaM executable is located.
#' @param quiet logical, if TRUE, suppress status messages.
#' @return nothing - just write a config file.
#' @examples
#'   setPathToBaM(dir.exe=tempdir())
#' @export
#' @importFrom tools R_user_dir
setPathToBaM <- function(dir.exe,quiet=FALSE){
  # Determine folder where config file can be written in accordance with CRAN rules
  dir.config=tools::R_user_dir(package="RBaM",which="config")
  if(!dir.exists(dir.config)){dir.create(dir.config,recursive=TRUE)}
  # Write config file
  fname=file.path(dir.config,'pathToBaM.txt')
  writeLines(text=dir.exe,con=fname)
  if(!quiet){
    message('----------------')
    message('Path to BaM executable has been set.')
    message('Please restart a new R session for the change to take effect.')
  }
}
