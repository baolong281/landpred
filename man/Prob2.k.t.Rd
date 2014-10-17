\name{Prob2.k.t}
\alias{Prob2.k.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Estimates P(TL <t0+tau | TL > t0, Z, TS==ts).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the probability that the an individual has the event of interest before t0 + tau given the discrete covariate, given the short term event occurred before t0 and occurred at time ts, and given the long term event has not yet occurred and the individual is still at risk at time t0.This function is called by Prob.Covariate.ShortEvent;  this function should not be called on its own.

}
\usage{
Prob2.k.t(t, t0, tau, data.use, bandwidth, covariate.value, weight = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
%%     ~~Describe \code{t} here~~
time of the short term event, ts, on the log scale.
}
  \item{t0}{
%%     ~~Describe \code{t0} here~~
the landmark time.
}
  \item{tau}{
%%     ~~Describe \code{tau} here~~
the residual survival time for which probabilities are calculated. Specifically, this function estimates the probability that the an individual has the event of interest before t0 + tau given the event has not yet occurred and the individual is still at risk at time t0.
}
  \item{data.use}{
%%     ~~Describe \code{data.use} here~~
n by 5 matrix. A data matrix where the first column is XL = min(TL, C) where TL is the time of the long term event, C is the censoring time, and the second column is DL =1*(TL<C), the third column is log(XS) = log(min(TS, C)) where TS is the time of the short term event, C is the censoring time, the fourth column is DS =1*(TS<C), and the fifth column is the covariate. 

}
  \item{bandwidth}{
%%     ~~Describe \code{bandwidth} here~~
bandwidth to be used.
}
  \item{covariate.value}{
%%     ~~Describe \code{covariate.value} here~~
covariate value at which to calculate probability.
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
 an optional weight to be incorporated in all estimation.

}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns estimated probabilities for each ts value (parameter t) at the specified covariate value; returns NA if ts>t0.
}
\references{
%% ~put references to the literature/web site here ~
Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term outcome information to predict long-term survival with discrete markers. Biometrical Journal 53.2 (2011): 294-307.
}
\author{
%%  ~~who you are~~
Layla Parast
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ prediction }
\keyword{ survival }% __ONLY ONE__ keyword per line
