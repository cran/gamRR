gamRR=function(
  fit,
  ref,
  est,
  data,
  n.points=10,
  plot=TRUE,
  ylim=NULL){
  
  ref=data.frame(t(ref))
  form=as.character(fit$formula)
  x.list=strsplit(form[3],"\\+")[[1]]
  x.list=gsub(" ","",x.list)
  x.list=sapply(strsplit(x.list,"\\,"),"[",1)#for s(x,k=n)
  x.list=gsub("s\\(","",x.list)
  x.list=gsub("as.factor\\(","",x.list)
  x.list=gsub("factor\\(","",x.list)
  x.list=gsub("offset\\(","",x.list)
  x.list=gsub("log\\(","",x.list)
  x.list=gsub("\\)","",x.list)
  if(length(names(ref))!=length(x.list)){
    stop("The number of variables in the 'ref' argument is not equal to those in the model!")
  }
  if(any(!(names(ref) %in% x.list))){stop("Some variables in the 'ref' argument are not in the model!")}

  for(i in 1:length(x.list)){data=data[!is.na(data[,x.list[i]]),]}
  
  rrref=predict(fit,type="response",newdata=ref)

  ndata=matrix(rep(0,nrow(data)*length(names(ref))),ncol=length(names(ref)))
  ndata=data.frame(ndata)
  names(ndata)=names(ref)
  ndata[,match(est,names(ndata))]=data[,match(est,names(data))]
  ndata[,-match(est,names(ref))]=ref[,-match(est,names(ref))]

  rr=predict(fit,type="response",newdata=ndata)
  rr=as.numeric(rr)/as.numeric(rrref)

  ref_no_est=names(ref)[-match(est,names(ref))]
  i=1
  ndata=matrix(rep(0,nrow(data)*length(names(ref))),ncol=length(names(ref)))
  ndata=data.frame(ndata)
  names(ndata)=names(ref)
  ndata[,match(est,names(ndata))]=data[,match(est,names(data))]
  for(j in 1:length(ref_no_est)){ndata[,ref_no_est[j]]=data[i,ref_no_est[j]]}
  
  rrn=predict(fit,type="response",newdata=ndata)/as.numeric(rrref)

  for(i in 2:nrow(data)){
    ndata=matrix(rep(0,nrow(data)*length(names(ref))),ncol=length(names(ref)))
    ndata=data.frame(ndata)
    names(ndata)=names(ref)
    ndata[,match(est,names(ndata))]=data[,match(est,names(data))]
    for(j in 1:length(ref_no_est)){ndata[,ref_no_est[j]]=data[i,ref_no_est[j]]}
    rrn=cbind(rrn,predict(fit,type="response",newdata=ndata)/as.numeric(rrref))
  }

  se=apply(rrn,1,FUN="sd")/sqrt(nrow(data)-1)
  u=rr+1.96*se
  l=rr-1.96*se
  xy=data.frame(x=data[,est],rr=rr,u=u,l=l)
  xy=xy[order(xy$x),]

  rangE=range(data[,est])
  est.seq=seq(from=rangE[1],to=rangE[2],length.out=n.points)
  seq.ind=which(abs(est.seq-as.numeric(ref[est]))==min(abs(est.seq-as.numeric(ref[est]))))
  est.seq[seq.ind]=as.numeric(ref[est])

  nxy=matrix(rep(0,n.points*4),ncol=4)
  nxy=data.frame(nxy)
  names(nxy)=c("x","rr","u","l")
  for(i in 1:n.points){
    ind=which(abs(xy$x-est.seq[i])==min(abs(xy$x-est.seq[i])))
    nxy[i,]=xy[ind,]
  }
  nxy[seq.ind,2:4]=1

  if(plot){
    if(is.null(ylim)){ylim=c(min(xy$l),max(xy$u))}
    plot(spline(nxy$x,nxy$rr,xmax=as.numeric(ref[,est])),type="l",xlim=c(min(nxy$x),max(nxy$x)),ylim=ylim,xlab=est,ylab="RR")
    lines(spline(nxy$x,nxy$l,xmax=as.numeric(ref[,est])),lty=2)
    lines(spline(nxy$x,nxy$u,xmax=as.numeric(ref[,est])),lty=2)
    lines(spline(nxy$x,nxy$rr,xmin=as.numeric(ref[,est])),lty=1)
    lines(spline(nxy$x,nxy$l,xmin=as.numeric(ref[,est])),lty=2)
    lines(spline(nxy$x,nxy$u,xmin=as.numeric(ref[,est])),lty=2)
  }

  return(nxy)
}
