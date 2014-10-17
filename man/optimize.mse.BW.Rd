\name{optimize.mse.BW}
\alias{optimize.mse.BW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates initial optimal bandwidth.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates initial optimal bandwidth with respect to mean squared error using K-fold cross-validation.
}
\usage{
optimize.mse.BW(data, t0,tau,h.grid=seq(.01,2,length=50), folds=3, reps=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	 \item{data}{
%%     ~~Describe \code{data} here~~
n by 5 matrix. A data matrix where the first column is XL = min(TL, C) where TL is the time of the long term event, C is the censoring time, and the second column is DL =1*(TL<C), the third column is XS = min(TS, C) where TS is the time of the short term event, C is the censoring time, the fourth column is DS =1*(TS<C), and the fifth column is the covariate. These are the data used to calculate the estimated probability.
}
  \item{t0}{
%%     ~~Describe \code{t0} here~~
the landmark time.
}
  \item{tau}{
%%     ~~Describe \code{tau} here~~
the residual survival time of interest.}
  \item{h.grid}{
The grid of possible bandwidths that the user would like the function to search through. Default is h.grid = seq(.01,2,length=50).}
  \item{folds}{
Number of folds wanted for K-fold cross-validation. Default is 3.}
  \item{reps}{
Number of repitions wanted of K-fold cross-validation. Default is 2.}
 }
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{h}{Selected bandwidth.}
}
\references{Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term outcome information to predict long-term survival with discrete markers. Biometrical Journal 53.2 (2011): 294-307.
}
\author{
%%  ~~who you are~~
Layla Parast
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bandwidth}
