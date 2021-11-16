\name{landpred-package}
\alias{landpred-package}
\alias{landpred}
\docType{package}
\title{Landmark Prediction of a Survival Outcome
}
\description{This package includes functions for landmark prediction of a survival outcome incorporating covariate and short-term event information. For more information about landmark prediction please see: Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term outcome information to predict long-term survival with discrete markers. Biometrical Journal 53.2 (2011): 294-307.}
\details{
\tabular{ll}{
Package: \tab landpred\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
License: \tab GPL \cr
}
%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Layla Parast
}
\references{
%~~ Literature or other references for background information ~~
Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term outcome information to predict long-term survival with discrete markers. Biometrical Journal 53.2 (2011): 294-307.
}
\keyword{ survival }
\examples{
%~~ simple examples of the most important functions ~~
data(data_example_landpred)
t0=2
tau = 8

####Landmark prediction with no covariate or short term information
Prob.Null(t0=t0,tau=tau,data=data_example_landpred)
out = Prob.Null(t0=t0,tau=tau,data=data_example_landpred)
out$Prob
out$data

newdata = matrix(c(1,1,3,0,4,1,10,1,11,0), ncol = 2, byrow=TRUE)
out = Prob.Null(t0=t0,tau=tau,data=data_example_landpred,newdata=newdata)
out$Prob
out$newdata

#Landmark prediction with covariate information only
Prob.Covariate(t0=t0,tau=tau,data=data_example_landpred)
out = Prob.Covariate(t0=t0,tau=tau,data=data_example_landpred)
out$Prob
out$data

newdata = matrix(c(1,1,1, 3,0,1, 4,1,1, 10,1,0, 11,0,1), ncol = 3, byrow=TRUE)
out = Prob.Covariate(t0=t0,tau=tau,data=data_example_landpred,newdata=newdata)
out$Prob
out$newdata

#Landmark prediction with covariate information and short term event information
#note: computationally intensive commands below
#Prob.Covariate.ShortEvent(t0=t0,tau=tau,data=data_example_landpred)
#out = Prob.Covariate.ShortEvent(t0=t0,tau=tau,data=data_example_landpred)
#out$data
#data.plot = out$data
#plot(data.plot$XS[data.plot$Z ==1], data.plot$Probability[data.plot$Z ==1], 
#pch = 20, xlim = c(0,t0))
#points(data.plot$XS[data.plot$Z ==0], data.plot$Probability[data.plot$Z ==0], 
#pch = 20, col = 2)

newdata = matrix(c(1,1,0.5,1,0,
3,0,1,1,1,
4,1,1.5,1,0,
10,1,5,1,0,
11,0,11,0,1), ncol = 5, byrow=TRUE)
#note: computationally intensive command below
#out=Prob.Covariate.ShortEvent(t0=t0,tau=tau,data=data_example_landpred,newdata=newdata)
#out$newdata

}