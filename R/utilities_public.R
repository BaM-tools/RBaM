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
