\name{prob2.single}
\alias{prob2.single}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Estimates P(TL <t0+tau | TL > t0, Z, TS==ts) for a single t.

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Helper function for Prob2.k.t; should not be called directly.
}
\usage{
prob2.single(K, W2i, Xi.long, tau, Di.short, Xi.short, Zi, t0, covariate.value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{K}{
%%     ~~Describe \code{t.K} here~~
Kernel matrix.
}
  \item{W2i}{
%%     ~~Describe \code{W2i} here~~
inverse probability of censoring weights.
}
  \item{Xi.long}{
%%     ~~Describe \code{Xi.long} here~~
XL = min(TL, C) where TL is the time of the long term event, C is the censoring time.
}
  \item{tau}{
%%     ~~Describe \code{tau} here~~
the residual survival time for which probabilities are calculated. Specifically, this function estimates the probability that the an individual has the event of interest before t0 + tau given the event has not yet occurred and the individual is still at risk at time t0.
}
  \item{Di.short}{
%%     ~~Describe \code{Di.short} here~~
DS =1*(TS<C), where TS is the time of the short term event, C is the censoring time.
}
  \item{Xi.short}{
%%     ~~Describe \code{Xi.short} here~~
log(XS) = log(min(TS, C)) where TS is the time of the short term event, C is the censoring time.
}
  \item{Zi}{
%%     ~~Describe \code{Zi} here~~
covariate vector.
}
  \item{t0}{
%%     ~~Describe \code{t0} here~~
landmark time.
}
  \item{covariate.value}{
%%     ~~Describe \code{covariate.value} here~~
specific covariate at which to estimate the conditional probability.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns estimated probability for values corresponding to the kernel matrix at the specified covariate value;
}
\author{
%%  ~~who you are~~
Layla Parast
}
\keyword{ survival }
\keyword{ kernel }% __ONLY ONE__ keyword per line
