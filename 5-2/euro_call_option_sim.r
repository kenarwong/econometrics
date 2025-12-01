rm(list=ls())

lnS0 = log(17.2) # starting value
beta = 1 # estimated parameter
nobs = 3 # 3 months to maturity
rf = 0.01 # annual risk-free rate
K = 19 # strike price
std = 0.088871 # annual standard deviation of the error term

# number of simulations
# increase simulations for more accuracy (slower),
# but will approach Black-Scholes value
n.repeat = 5000

payoff = rep(0, n.repeat)

set.seed(123)

for (i in 1:n.repeat){

  # initialize 
  epsilon = rnorm(nobs, mean = 0, sd = std)
  lnS = rep(0, nobs)
  lnS[0] = lnS0
  lnS[1] = rf + lnS0 + epsilon[1]

  # simulate the log prices over the next nobs periods
  for (t in 2:nobs){
    lnS[t] = rf + beta * lnS[t-1] + epsilon[t]
  }
  St = exp(lnS)

  # call option payoff
  # for put option, use: payoff[i] = max(K - St[nobs], 0)
  payoff[i] = max(St[nobs] - K, 0)
}

callprice = (1/(1 + rf/4)^3) * mean(payoff)
print(callprice)

print(head(payoff, 20))
