\name{gamRR}
\alias{gamRR}
\title{Calculate the RR for the GAM}
\description{
To calculate the relative risk (RR) for the generalized additive model
}
\usage{
gamRR(fit,ref,est,data,plot,ylim)
}
\arguments{
  \item{fit}{an object of gam()}
  \item{ref}{a vector of the independent variables at referenced level, please note that the names of the variables in 'ref' should be matched to those in the model}
  \item{est}{character, to indicate which numeric variable should be calculated the RR, please note that the name of the variable in 'est' should be matched to which in the model}
  \item{data}{the name of the data in the gam()}
  \item{plot}{logic, to indicate whehter to plot the rr}
  \item{ylim}{a vector of tow numeric number}
}
\value{
  \item{data frame}{a data frame including variables of 'x', 'rr', 'u', and 'l'}
  \item{x}{the value of 'est' variable}
  \item{rr}{the RR coresponding to  'est' variable}
  \item{u}{the 95 percent upper limit of the 'rr'}
  \item{l}{the 95 percent lower limit of the 'rr'}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

Update description:

version 0.2.0: 1. checking procedure for the arguments was added. The function will stop if the number of variables in the 'ref' argument was not equal to those in the model or some variables in the 'ref' argument were not in the model.

more functions will be included in 'gamRR' package!
}
\examples{
require("mgcv")
dat <- gamSim(1,100,dist="poisson",scale=.25)
fit <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,dat,method="REML")
plot(fit,select=2)

gamRR(fit=fit,
  ref=c(x0=dat$x0[1],x1=dat$x1[1],x2=dat$x2[1],x3=dat$x3[1]),
  est="x2",
  data=dat,
  plot=TRUE)
}
