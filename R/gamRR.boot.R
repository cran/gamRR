gamRR.boot=function(
  fit,
  ref,
  est,
  data,
  n.points=10,
  n.boot=50,
  plot=TRUE,
  ylim=NULL){

  ref=data.frame(t(ref))
  form=as.character(fit$formula)
  x.list=strsplit(form[3],"\\+")[[1]]
  x.list=gsub(" ","",x.list)
  x.list=gsub("s\\(","",x.list)
  x.list=gsub("as.factor\\(","",x.list)
  x.list=gsub("factor\\(","",x.list)
  x.list=gsub("\\)","",x.list)
  if(length(names(ref))!=length(x.list)){
    stop("The number of variables in the 'ref' argument is not equal to those in the model!")
  }
  if(any(!(names(ref) %in% x.list))){
    stop("Some variables in the 'ref' argument are not in the model!")
  }

  rrref=predict(fit,type="response",newdata=ref)

  rangE=range(data[,est])
  est.seq=seq(from=rangE[1],to=rangE[2],length.out=n.points)
  seq.ind=which(abs(est.seq-as.numeric(ref[est]))==min(abs(est.seq-as.numeric(ref[est]))))
  est.seq[seq.ind]=as.numeric(ref[est])

  ndata=matrix(rep(0,n.points*length(names(ref))),ncol=length(names(ref)))
  ndata=data.frame(ndata)
  names(ndata)=names(ref)
  ndata[,est]=est.seq
  ndata[,-match(est,names(ref))]=ref[,-match(est,names(ref))]

  dzc.boot=function(data.boot,i){
    fit.boot=gam(fit$formula,family=fit$family,method=fit$method,
                 data=data.boot[i,])
    c(predict(fit.boot,type="response",newdata=ndata)/as.numeric(rrref))
  }
  rr.boot=boot(data=data,statistic=dzc.boot,R=n.boot,stype="i")
  t=data.frame(t(rr.boot$t))
  rr=apply(t,1,FUN="mean")
  rr.se=apply(t,1,FUN="sd")
  rr.l=rr-1.96*rr.se
  rr.u=rr+1.96*rr.se

  rr[seq.ind]=1
  rr.l[seq.ind]=1
  rr.u[seq.ind]=1

  if(is.null(ylim)){ylim=c(min(rr.l),max(rr.u))}

  if(plot){
    plot(spline(est.seq,rr,xmax=as.numeric(ref[,est])),
         type="l",xlim=c(min(est.seq),max(est.seq)),ylim=ylim,xlab=est,ylab="RR")
    lines(spline(est.seq,rr.l,xmax=as.numeric(ref[,est])),lty=2)
    lines(spline(est.seq,rr.u,xmax=as.numeric(ref[,est])),lty=2)
    lines(spline(est.seq,rr,xmin=as.numeric(ref[,est])),lty=1)
    lines(spline(est.seq,rr.l,xmin=as.numeric(ref[,est])),lty=2)
    lines(spline(est.seq,rr.u,xmin=as.numeric(ref[,est])),lty=2)
  }

  xy=data.frame(x=est.seq,rr=rr,u=rr.u,l=rr.l)

  return(xy)
}
