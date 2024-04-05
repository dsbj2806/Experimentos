  
plot.ic1=function(r1,r2,mreal,var,m=1){
  x=factor(c(rep(1,r1),rep(2,r2)))
  x11()
  if(m>16) m=16
  if(m==1) par(mfrow=c(1,2))
  if(m%in% c(2,4,6,8)) par(mfrow=c(2,m))
  if(m%in% c(3,5,7)) par(mfrow=c(2,m+1))
  #if(m>=9 & m<=) par(mfrow=c(4,8))
  
  for(i in 1:m){
    y1=rnorm(r1,mreal[1],sqrt(var))
    y2=rnorm(r2,mreal[2],sqrt(var))
    y=c(y1,y2)
    mod=lm(y~x)
    cmr=anova(mod)[2,3]
    q1=qt(0.975,r1-1)
    q2=qt(0.975,r2-1)
    q=qt(0.975,r1+r2-2)
    ee1=sqrt(cmr/r1)
    ee2=sqrt(cmr/r2)
    ee=sqrt(cmr/r1+cmr/r2)
    ic1=mean(y1)+c(-1,1)*q1*ee1
    ic2=mean(y2)+c(-1,1)*q2*ee2
    ic=abs(mean(y1)-mean(y2))+c(-1,1)*q*ee
    
    
    plot(0,0,type="n",xaxt="n",xlab="",ylab="",
         xlim=c(0.5,2.5),ylim=c(mean(mreal)-3*sqrt(var),mean(mreal)+3*sqrt(var)))
    points(rep(1,r1),y1,col=4,pch=18)
    k1=1
    if(max(c(ic1[1],ic2[1]))<min(c(ic1[2],ic2[2]))) k1=2
    segments(1,ic1[1],1,ic1[2],lwd=2.5,col=k1)
    points(rep(2,r2),y2,col=4,pch=18)
    segments(2,ic2[1],2,ic2[2],lwd=2.5,col=k1)
    
    plot(0,0,type="n",xaxt="n",xlab="",ylab="",
         xlim=c(0.5,1.5),ylim=c(-3*sqrt(var),3*sqrt(var)))
    k2=1
    if(prod(ic)<0) k2=2
    segments(1,ic[1],1,ic[2],lwd=2.5,col=k2)
    abline(h=abs(diff(mreal)),col=4,lty=2)
    abline(h=0,col=2,lty=2)
  }
}


plot.dist=function(mreal,var,ini=min(c(mreal[1]-3*sqrt(var[1]),mreal[2]-3*sqrt(var[2]))),
                   fin=max(c(mreal[1]+3*sqrt(var[1]),mreal[2]+3*sqrt(var[2])))){
  var1=var[1]; var2=var[2]
  alt=max(c(dnorm(0,0,sqrt(var1)),dnorm(0,0,sqrt(var2))))
  curve(dnorm(x,mreal[1],sqrt(var1)),ini,fin,col=2,ylab="",xlab="",ylim=c(0,alt))
  curve(dnorm(x,mreal[2],sqrt(var2)),add=T,col=4)
  abline(v=mreal,col=c(2,4))
  segments(mreal[1],0,mreal[2],0,lty=2,col=1)
  text(mean(mreal),dnorm(0,0,sqrt(var1))*0.1,paste("d = ",round(abs(diff(mreal)),2),sep=""))
}


plot.muestra=function(mreal,var,r1,r2){
  par(mfrow=c(1,2))
  var1=var[1]; var2=var[2]
  y1=rnorm(r1,mreal[1],sqrt(var1))
  y2=rnorm(r2,mreal[2],sqrt(var2))
  res=c(y1-mean(y1),y2-mean(y2))
  alt=c(max(c(density(y1)$y,density(y2)$y,density(c(y1,y2))$y)))
  a=min(c(density(y1)$x,density(y2)$x))
  b=max(c(density(y1)$x,density(y2)$x))
  plot(density(y1),col=2,xlim=c(a,b),main="",xlab="",ylab="",ylim=c(0,alt))
  points(y1,rep(0,r1),pch=18,col=2)
  lines(density(y2),col=4)
  points(y2,rep(0,r2),pch=18,col=4)
  lines(density(c(y1,y2)))
  legend("topleft",c("condicional 1","condicional 2","total"),cex=0.5,col=c(2,4,1),lty=1,bty="n")
  plot(density(res),col=2,main="",xlab="residual",ylab="",ylim=c(0,alt))
  
}


plot.ic2=function(r1,r2,mreal,var1,var2,m){
  mreal=c(min(mreal),max(mreal))
  par(mfrow=c(1,1))
  var=(var1+var2)/2
  x=factor(c(rep(1,r1),rep(2,r2)))
  plot(0,0,type="n",xaxt="n",xlab="",ylab="",
       xlim=c(0.5,m+0.5),ylim=c(abs(diff(mreal))-3*sqrt(var),abs(diff(mreal))+3*sqrt(var)))
  
  tot=0
  ancho=c()
  for(j in 1:m){
    y1=rnorm(r1,mreal[1],sqrt(var1))
    y2=rnorm(r2,mreal[2],sqrt(var2))
    y=c(y1,y2)
   
    mod=lm(y~x)
    cmr=anova(mod)[2,3]
    q=qt(0.975,r1+r2-2)
    ee=sqrt(cmr/r1+cmr/r2)
    ic=mean(y2)-mean(y1)+c(-1,1)*q*ee
    ancho[j]=2*q*ee
    
    k2=1
    if(between(diff(mreal),ic[1],ic[2])) k2=2  else (tot=tot+1)
    segments(j,ic[1],j,ic[2],lwd=2.5,col=k2)
    }
    
  abline(h=abs(diff(mreal)),col=4,lty=2)
  text(m/2,abs(diff(mreal))-3*sqrt(var),paste(round(tot/m*100,1),"% no contienen la verdadera diferencia - Ancho promedio IC:",round(mean(ancho),1),sep=""))
}




pot1=function(r,mreal,var,m){
  x=mu=sigma=c()
  n=sum(r)
  k=length(r)
  for(i in 1:k){
    x=c(x,rep(i,r[i]))
    mu=c(mu,rep(mreal[i],r[i]))
    sigma=c(sigma,rep(var[i],r[i]))
  }
  x=factor(x)
  p=c()
  for(j in 1:m){
    y=rnorm(n,mu,sqrt(sigma))
    mod=lm(y~x)
    p[j]=anova(mod)[1,5]
  }
  pot=mean(p<0.05)
  pot
}


