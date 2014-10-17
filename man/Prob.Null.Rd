\name{Prob.Null}
\alias{Prob.Null}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Estimates P(TL <t0+tau | TL > t0).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the probability that an individual has the event of interest before t0 + tau given the event has not yet occurred and the individual is still at risk at time t0; this estimated probability does not incorporate any information about the covariate or short term event information.
}
\usage{
Prob.Null(t0, tau, data, weight = NULL, newdata=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t0}{
%%     ~~Describe \code{t0} here~~
the landmark time.
}
  \item{tau}{
%%     ~~Describe \code{tau} here~~
the residual survival time for which probabilities are calculated. Specifically, this function estimates the probability that the an individual has the event of interest before t0 + tau given the event has not yet occurred and the individual is still at risk at time t0.
}
  \item{data}{
%%     ~~Describe \code{data} here~~
n by k matrix, where k >=2. A data matrix where the first column is XL = min(TL, C) where TL is the time of the long term event, C is the censoring time, and the second column is DL =1*(TL<C). These are the data used to calculate the estimated probability.
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
 an optional weight to be incorporated in all estimation.
}
  \item{newdata}{
an optional n by k matrix, where k >=2. A data matrix where the first column is XL = min(TL, C) where TL is the time of the long term event, C is the censoring time, and the second column is DL =1*(TL<C). Predicted probabilities are estimated for these data.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{Prob}{Estimated probability that the an individual has the event of interest before t0 + tau given the event has not yet occurred and the individual is still at risk at time t0; this estimated probability does not incorporate any information about the covariate or short term event information.}
\item{data}{the data matrix with an additional column with the estimated individual probabilities; note that the predicted probability is NA if TL <t0 since it is only defined for individuals with TL> t0}
\item{newdata}{the newdata matrix with an additional column with the estimated individual probabilities; note that the predicted probability is NA if TL <t0 since it is only defined for individuals with TL> t0; if newdata is not supplied then this returns NULL}
}
\references{Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term outcome information to predict long-term survival with discrete markers. Biometrical Journal 53.2 (2011): 294-307.
}
\author{
%%  ~~who you are~~
Layla Parast
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(data_example_landpred)
t0=2
tau = 8
Prob.Null(t0=t0,tau=tau,data=data_example_landpred)

out = Prob.Null(t0=t0,tau=tau,data=data_example_landpred)
out$Prob
out$data

newdata = matrix(c(1,1,3,0,4,1,10,1,11,0), ncol = 2, byrow=TRUE)
out = Prob.Null(t0=t0,tau=tau,data=data_example_landpred,newdata=newdata)
out$Prob
out$newdata

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ prediction }
\keyword{ survival }% __ONLY ONE__ keyword per line
