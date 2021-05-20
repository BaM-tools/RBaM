m.x=800;s.x=150
s.e=50
a=0.6
n=50
x=rnorm(n,m.x,s.x)
y.true=a*x+rnorm(n,0,s.e)

ll.standard<-function(theta,y,x,s.e){
  y.sim=theta*x
  ll=sum(dnorm(y,y.sim,s.e,log=T))
}

ll.nu<-function(theta,ro,y,x,m.y,s.y){
  y.sim=theta*x
  z=(1/s.y^2)*( (y-m.y)^2 - 2*ro*(y-m.y)*(y.sim-m.y) + (y.sim-m.y)^2)
  ll=-1*z/(1-ro^2)-log(2*pi*s.y^2*sqrt(1-ro^2))
  return(sum(ll))
}

X11();par(mfrow=c(1,4))
plot(x,y.true)
theta.grid=seq(0.1,0.9,,100)
ro.grid=seq(0.01,0.99,,100)
ll1=matrix(NA,length(theta.grid),1)
ll2=matrix(NA,length(theta.grid),length(ro.grid))

for(i in 1:length(theta.grid)){
  ll1[i]=ll.standard(theta.grid[i],y.true,x,s.e)
  for(j in 1:length(ro.grid)){
    ll2[i,j]=ll.nu(theta.grid[i],ro.grid[j],y.true,x,mean(y.true),sd(y.true))
  }
}
plot(theta.grid,exp(ll1-max(ll1)),type='l')
matplot(theta.grid,exp(ll2-max(ll2)),type='l')
matplot(ro.grid,t(exp(ll2-max(ll2))),type='l')

