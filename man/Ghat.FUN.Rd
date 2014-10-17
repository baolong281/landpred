\name{Ghat.FUN}
\alias{Ghat.FUN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates the Kaplan Meier survival probability for censoring
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates the survival probability for censoring i.e. P(C > tt) where C is censoring; used in inverse probability of censoring weights (IPCW). This function is called by Wi.FUN;  this function should not be called on its own.


}
\usage{
Ghat.FUN(tt, data, type = "fl", weight.given)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tt}{
%%     ~~Describe \code{tt} here~~
the time (or vector of times) at which the survival probability should be estimated.
}
  \item{data}{
%%     ~~Describe \code{data} here~~
n by k matrix, where k>=2. A data matrix where the first column is XL = min(TL, C) where TL is the time of the long term event, C is the censoring time, and the second column is DL =1*(TL<C)}
  \item{type}{
%%     ~~Describe \code{type} here~~
type sent to survfit function, default is "fl".
}
  \item{weight.given}{
%%     ~~Describe \code{weight.given} here~~
a weight to be used in estimation.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
survival probability for censoring at time tt
}
\author{
%%  ~~who you are~~
Layla Parast
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ survival }
\keyword{ IPCW }% __ONLY ONE__ keyword per line
