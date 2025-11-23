# Estimates P(TL \<t0+tau \| TL \> t0, Z, min(TS, t0), I(TS\<=t0)), i.e. given discrete covariate and TS information.

This function calculates the probability that the an individual has the
event of interest before t0 + tau given the discrete covariate, given
short term event information, and given the event has not yet occurred
and the individual is still at risk at time t0.

## Usage

``` r
Prob.Covariate.ShortEvent(t0, tau, data, weight = NULL, bandwidth = NULL, newdata=NULL)
```

## Arguments

- t0:

  the landmark time.

- tau:

  the residual survival time for which probabilities are calculated.
  Specifically, this function estimates the probability that the an
  individual has the event of interest before t0 + tau given the event
  has not yet occurred and the individual is still at risk at time t0.

- data:

  n by 5 matrix. A data matrix where the first column is XL = min(TL, C)
  where TL is the time of the long term event, C is the censoring time,
  and the second column is DL =1\*(TL\<C), the third column is XS =
  min(TS, C) where TS is the time of the short term event, C is the
  censoring time, the fourth column is DS =1\*(TS\<C), and the fifth
  column is the covariate. These are the data used to calculate the
  estimated probability.

- weight:

  a weight to be incorporated in all estimation.

- bandwidth:

  an optional bandwidth to be used in kernel smoothing; is not provided
  then function calculates an appropriate bandwidth using bw.nrd and
  then undersmoothing with c = .10 (See reference)

- newdata:

  an optional n by 5 matrix where the first column is XL = min(TL, C)
  where TL is the time of the long term event, C is the censoring time,
  and the second column is DL =1\*(TL\<C), the third column is XS =
  min(TS, C) where TS is the time of the short term event, C is the
  censoring time, the fourth column is DS =1\*(TS\<C), and the fifth
  column is the covariate. Predicted probabilities are estimated for
  these data.

## Value

- data:

  the data matrix with an additional column with the estimated
  individual probabilities; note that the predicted probability is NA if
  TL \<t0 since it is only defined for individuals with TL\> t0

- newdata:

  the newdata matrix with an additional column with the estimated
  individual probabilities; note that the predicted probability is NA if
  TL \<t0 since it is only defined for individuals with TL\> t0; if
  newdata is not supplied then this returns NULL

## References

Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term
outcome information to predict long-term survival with discrete markers.
Biometrical Journal 53.2 (2011): 294-307.

## Author

Layla Parast

## Examples

``` r
data(data_example_landpred)
t0=2
tau = 8
#note: computationally intensive command below
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
#out = Prob.Covariate.ShortEvent(t0=t0,tau=tau,data=data_example_landpred,newdata=newdata)
#out$newdata
```
