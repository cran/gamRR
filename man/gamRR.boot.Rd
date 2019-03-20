\name{gamRR.boot}
\alias{gamRR.boot}
\title{Calculate the RR for the GAM by using the bootstrap method}
\description{
To calculate the relative risk (RR) for the generalized additive model by using the bootstrap method
}
\usage{
gamRR.boot(fit,ref,est,data,n.points,n.boot,plot,ylim)
}
\arguments{
  \item{fit}{an object of gam()}
  \item{ref}{a vector of the independent variables at referenced level, please note that the names of the variables in 'ref' should be matched to those in the model}
  \item{est}{character, to indicate which numeric variable should be calculated the RR, please note that the name of the variable in 'est' should be matched to which in the model}
  \item{data}{the name of the data in the gam()}
  \item{n.points}{integer, the number of points of 'est' to be estimated, the default is 10}
  \item{n.boot}{integer, the number of times for resampling, the default is 50}
  \item{plot}{logic, to indicate whehter to plot the rr, the default is TRUE}
  \item{ylim}{a vector of tow numeric number determinging the range of y axis}
}
\value{
  \item{data frame}{a data frame including variables of 'x', 'rr', 'u', and 'l'}
  \item{x}{the value of 'est' variable}
  \item{rr}{the RR coresponding to  'est' variable}
  \item{u}{the 95 percent upper limit of the 'rr'}
  \item{l}{the 95 percent lower limit of the 'rr'}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Wangjian Zhang<wzhang27@albany.edu>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

Update description:

version 0.2.0: 1. checking procedure for the arguments was added. The function will stop if the number of variables in the 'ref' argument was not equal to those in the model or some variables in the 'ref' argument were not in the model.

version 0.3.0: 1. gamRR.boot() function was added.

version 0.4.0: 1. the plot styles of gamRR() and gamRR.boot() were united. 2. the independent variable with factor() or as.factor() was allowed.

version 0.5.0: 1. fix the error "object 'nxy' not found" in gamRR().

version 0.6.0: 1. fix the error if there were missing data. 2. fix the warnings of 'replace' in 'data.frame'. 3. the independent variable with offset() or log() was allowed.

more functions will be included in 'gamRR' package!
}
\seealso{
\code{\link{gamRR}}
}
\examples{
#require("mgcv")
#dat <- gamSim(1,100,dist="poisson",scale=.25)
#fit <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,dat,method="REML")
#plot(fit,select=2)

#gamRR.boot(
# fit=fit,
# ref=c(x0=dat$x0[1],x1=dat$x1[1],x2=dat$x2[1],x3=dat$x3[1]),
# est="x1",
# data=dat,
# n.points=10,
# n.boot=10,
# plot=TRUE,
# ylim=NULL)
}
