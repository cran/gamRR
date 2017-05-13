
gamRR=function(fit,ref,est,data,plot=TRUE,ylim=NULL){

  ref=data.frame(t(ref))
  rrref=predict(fit,type="response",newdata=ref)

  ndata=matrix(rep(0,nrow(data)*length(names(ref))),ncol=length(names(ref)))
  ndata=data.frame(ndata)
  names(ndata)=names(ref)
  ndata[,match(est,names(ndata))]=data[,match(est,names(data))]
  ndata[,-match(est,names(ref))]=ref[,-match(est,names(ref))]

  rr=predict(fit,type="response",newdata=ndata)

  rr=as.numeric(rr)/as.numeric(rrref)

  i=1
  ndata=matrix(rep(0,nrow(data)*length(names(ref))),ncol=length(names(ref)))
  ndata=data.frame(ndata)
  names(ndata)=names(ref)
  ndata[,match(est,names(ndata))]=data[,match(est,names(data))]
  ndata[,-match(est,names(ref))]=data[i,match(names(ndata),names(data))][,-match(est,names(ref))]
  rrn=predict(fit,type="response",newdata=ndata)/as.numeric(rrref)

  for(i in 2:nrow(data)){
    ndata=matrix(rep(0,nrow(data)*length(names(ref))),ncol=length(names(ref)))
    ndata=data.frame(ndata)
    names(ndata)=names(ref)
    ndata[,match(est,names(ndata))]=data[,match(est,names(data))]
    ndata[,-match(est,names(ref))]=data[i,match(names(ndata),names(data))][,-match(est,names(ref))]
    rrn=cbind(rrn,predict(fit,type="response",newdata=ndata)/as.numeric(rrref))
  }

  se=apply(rrn,1,FUN="sd")/sqrt(nrow(data)-1)
  u=rr+1.96*se
  l=rr-1.96*se
  xy=data.frame(x=data[,est],rr=rr,u=u,l=l)
  xy=xy[order(xy$x),]

  if(plot){
    if(is.null(ylim)){ylim=c(min(xy$l),max(xy$u))}

    plot(xy[,c("x","rr")],type="l",ylim=ylim,ylab="RR",xlab=est)
    lines(xy[,c("x","u")],lty=2)
    lines(xy[,c("x","l")],lty=2)
    abline(h=1,col="grey")
  }

  return(xy)
}
