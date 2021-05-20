require(ggplot2);require(reshape2);require(GGally)

ReadModel<-function(ModelFile='Config_Model.txt'){
  #------------------------------------------------------------
  # Read information on the fitted model in Config_Model.txt
  #------------------------------------------------------------
  # read model info
  k=0;ID=qscan(ModelFile,k)[1]
  k=k+1;nX=as.numeric(qscan(ModelFile,k)[1])
  k=k+1;nY=as.numeric(qscan(ModelFile,k)[1])
  k=k+1;nPar=as.numeric(qscan(ModelFile,k)[1])
  # parameter blocs
  par=list();m=0
  for(i in 1:nPar){
    k=k+1;name=qscan(ModelFile,k)[1]
    k=k+1;init=as.numeric(qscan(ModelFile,k)[1])
    k=k+1;dist=qscan(ModelFile,k)[1]
    k=k+1;foo=qscan(ModelFile,k,sep='?')
    if(dist=='Gaussian' | dist=='Uniform'){
      ppar=as.numeric(strsplit(gsub(pattern=",",replacement=" ",foo),split=" ")[[1]][1:2])
    } else {
      ppar=c(NA,NA)
    }        
    if(dist!='FIX'){
      m=m+1;par[[m]]=list(name=name,init=init,prior=list(dist=dist,par=ppar))
    }
  }
  return(list(ID=ID,nX=nX,nY=nY,nPar=nPar,par=par))
}

MCMCplot<-function(MCMCfile='Results_MCMC_Cooked.txt',
                   doPar=T,doLogPost=F,doDPar=F,
                   type="trace", # "histogram","density","scatterplot"
                   xlab='',ylab='',
                   ncol=3,prior=NULL,
                   burn=0,slim=1){
  #------------------------------------------------------------
  # Plot MCMC samples
  #------------------------------------------------------------
  # read MCMC simulation, burn and slim if required
  X=read.table(MCMCfile,header=T)
  X=X[seq(burn+1,nrow(X),slim),]
  # determine logpot column
  lpcol=which(names(X)=='LogPost')
  # determine which columns should be kept
  keep=c()
  if(doPar){keep=c(keep,1:(lpcol-1))}
  if(doLogPost){keep=c(keep,lpcol)}
  if(doDPar){if(ncol(X)>lpcol){keep=c(keep,(lpcol+1):ncol(X))}}
  X=X[,keep]
  if(type=="scatterplot"){
    m=ggpairs(X,diag=list(continuous='density'),
              lower=list(continuous='points'),upper=list(continuous='cor'),axisLabels='show')
  } else {
    X=cbind(X,indx=1:nrow(X))
    Xm=melt(X,id.vars='indx')  
    m=ggplot(Xm)
    m=m+switch(type,
               trace=geom_line(aes(x=indx,y=value,colour=variable)),
               histogram=geom_histogram(aes(value,fill=variable,..density..)),
               density=geom_density(aes(value,fill=variable),colour=NA))
    m=m+facet_wrap(~variable,scales='free',ncol=ncol)+
    xlab(xlab)+ylab(ylab)+theme(legend.position="none")
    
    # Plot priors if required
    if(doPar & (type=='density' | type=='histogram') & !is.null(prior)){ 
      priorDF=data.frame()
      for(i in 1:length(prior)){
        grid=seq(min(X[,i]),max(X[,i]),length.out=100)
        if(prior[[i]]$prior$dist=='Gaussian'){
          y=dnorm(grid,mean=prior[[i]]$prior$par[1],sd=prior[[i]]$prior$par[2])
        }
        if(prior[[i]]$prior$dist=='Uniform'){
          y=dunif(grid,min=prior[[i]]$prior$par[1],max=prior[[i]]$prior$par[2])
        }
        if(prior[[i]]$prior$dist=='LogNormal'){
          y=dlnorm(grid,meanlog=prior[[i]]$prior$par[1],sdlog=prior[[i]]$prior$par[2])
        }
        if(prior[[i]]$prior$dist=='Gaussian' | prior[[i]]$prior$dist=='Uniform' | prior[[i]]$prior$dist=='LogNormal'){
          priorDF=rbind(priorDF,data.frame(variable=prior[[i]]$name,x=grid,y=y))
        }
      }
      m=m+geom_line(aes(x=x,y=y),data=priorDF,colour="black")
    }   
   }
  return(m)
}

Residualplot<-function(ResFile='Results_Residuals.txt',
                       nX,nY,
                       xlab=c('Observed','Simulated','Simulated','Observed X'),
                       ylab=c('Simulated','Residuals','Standardized Residuals','Observed / simulated Y')){
  #------------------------------------------------------------
  # Perform residual analysis
  #------------------------------------------------------------
  R=read.table(ResFile,header=T)
  #------------------------------------------------------------
  # Yobs/Yunbiased vs. Ysim and Ysim vs. residuals
  DF=data.frame()
  for(i in 1:nY){
    DF=rbind(DF,
             data.frame(variable=i,
               Yobs=R[[paste('Y',i,'_obs',sep='')]],
               Yunbiased=R[[paste('Y',i,'_unbiased',sep='')]],
               Ysim=R[[paste('Y',i,'_sim',sep='')]],
               Residual=R[[paste('Y',i,'_res',sep='')]],
               StandardizedResidual=R[[paste('Y',i,'_stdres',sep='')]]))
  }
  # Obs vs. sim plot
  m1=ggplot(DF)+
    geom_point(aes(x=Yobs,y=Ysim),colour='black',size=5)+
    facet_wrap(~variable,scales='free')+
    theme(legend.position="none")
  if( any(DF$Yobs!=DF$Yunbiased) ){
    m1=m1+geom_point(aes(x=Yunbiased,y=Ysim),size=3,colour='red')
    xl='Observed (black) / Unbiased (red)'
  } else {xl=xlab[1]}
  m1=m1+xlab(xl)+ylab(ylab[1])+geom_abline(intercept=0,slope=1)
  # sim vs. res plot
  m2=ggplot(DF)+
    geom_point(aes(x=Ysim,y=Residual),colour='black',size=5)+
    facet_wrap(~variable,scales='free')+
    theme(legend.position="none")+geom_abline(intercept=0,slope=0)+
    xlab(xlab[2])+ylab(ylab[2])
  # sim vs. standardized residuals plot
  m3=ggplot(DF)+
    geom_point(aes(x=Ysim,y=StandardizedResidual),colour='black',size=5)+
    facet_wrap(~variable,scales='free')+
    theme(legend.position="none")+geom_abline(intercept=0,slope=0)+
    xlab(xlab[3])+ylab(ylab[3])
  #------------------------------------------------------------
  # Xobs vs. Yobs/Ysim
  DF=data.frame()
  for(i in 1:nX){
    for(j in 1:nY){
      DF=rbind(DF,
               data.frame(Xvariable=i,Yvariable=j,
                          Xobs=R[[paste('X',i,'_obs',sep='')]],
                          Yobs=R[[paste('Y',j,'_obs',sep='')]],
                          Ysim=R[[paste('Y',j,'_sim',sep='')]]))
    }
  }
  m4=ggplot(DF)+
    geom_point(aes(x=Xobs,y=Yobs),colour='black',size=5)+
    geom_line(aes(x=Xobs,y=Ysim),colour='black')+
    facet_wrap(Yvariable~Xvariable,scales='free',ncol=nX)+
    theme(legend.position="none")+
    xlab(xlab[4])+ylab(ylab[4])
  #------------------------------------------------------------
  return(list(m1=m1,m2=m2,m3=m3,m4=m4))
  
}
                   
EnvelopLayer<-function(file,Xfile=NULL,color='red',alpha=1,sep=F){
  #------------------------------------------------------------
  # Return the ggplot layer from an envelop file 
  # WARNING: just a layer! need to call ggplot() before printing 
  #------------------------------------------------------------
  E=read.table(file,header=T)
  n=nrow(E)
  if(is.null(Xfile)){X=matrix(1:n,n,1)} else {X=read.table(Xfile,header=F)}
  w=sort(X[,1],index.return=T)
  if(!sep){ # uncertainty bands
    x=c(X[w$ix[1:n],1],X[w$ix[n:1],1])
    y=c(E[w$ix[1:n],2],E[w$ix[n:1],3])
    DF=data.frame(x=x,y=y)
    m=geom_polygon(aes(x=x,y=y),data=DF,fill=color,alpha=alpha,colour=NA)
  } else { # separated uncertainty bars
    x=X[w$ix[1:n],1]
    lo=E[w$ix[1:n],2]
    hi=E[w$ix[1:n],3]
    DF=data.frame(x=x,lo=lo,hi=hi)
    m=geom_crossbar(aes(x=x,y=lo,ymin=lo,ymax=hi),data=DF,fill=color,alpha=alpha,colour=NA)
  }
  return(m)
}

SpaghettiLayer<-function(file,Xfile=NULL,color='black',size=1,sep=F){
  #------------------------------------------------------------
  # Return the ggplot layer from a spaghetti file 
  # WARNING: just a layer! need to call ggplot() before printing 
  #------------------------------------------------------------
  E=read.table(file,header=F)
  n=nrow(E);p=ncol(E)
  if(is.null(Xfile)){X=matrix(1:n,n,1)} else {X=read.table(Xfile,header=F)}
  for(i in 1:p){
    DF=data.frame(x=X[,1],y=E[,i])
    if(!sep) { # line
      mi=geom_line(aes(x=x,y=y),data=DF,colour=color,size=size)
    } else { # separated points
      mi=geom_point(aes(x=x,y=y),data=DF,colour=color,size=size)
    }
    if(i==1){m=mi} else {m=m+mi}
  }
  return(m)
}

qscan<-function(f,k,sep=' '){
  # quick scan - just a convenience wrapper
  return(scan(f,what='character',skip=k,nlines=1,quiet=TRUE,sep=sep))
}

Calendar2Days<-function(fileIN,fileOUT,format="%d/%m/%Y %H:%M:%S"){
  # read input file as text
  w=readLines(fileIN)
  # convert to date format
  z=as.POSIXct(w,format=format)
  # convert to seconds
  ww=unclass(z)
  # convert to days
  d=(ww-ww[1])/(60*60*24)
  # write result
  write(d,file=fileOUT,ncolumns=1)
}
  
  
  