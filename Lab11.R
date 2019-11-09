x <- c(12.8, 10.5, 13.2, 13.0, 7.0, 11.0, 13.4, 13.2, 9.5, 11.0, 10.9, 4.6, 5.8, 3.2, 9.8, 0.2, 11.2,
      7.2, 14.7, 5.9, 9.7, 17.6, 8.5, 6.8, 7.2, 12.2, 16.7, 10.4, 14.2, 5.7)
mu0 = 8
omega0 = 2
alpah0 = 5
beta0= 1
tau0 = 1/omega0
xbar = mean(x)
n = length(x)
tau <- 1/var(x)
mu <- 0

gmean <- function(tau) {
  return(((n*tau*xbar)+(mu0*tau0))/(n*tau+tau0))
}
gvar <- function(tau) {
  return(1/((n*tau)+tau0))
}
g <- function(x, tau) {
  return(dnorm(x, gmean(tau), gvar(tau)))
}
h <- function(x, mu) {
  return(df(x, (alpah0+n/2), (beta0+((x-mu)^2/2))))
}

muVal <- rep(NA, 500)
tauVal <- rep(NA, 500)
for (i in 1:500) {
  muSamples <- rep(NA, 1000)
  for (j in 1:1000) {
    muSamples[j] <- g(j, tau)
  }
  mu <- mean(muSamples)
  muVal[i] <- mu
  tauSamples <- rep(NA, 1000)
  for (k in 1:1000) {
    tauSamples[k] <- h(k, mu)
  }
  tau <- mean(tauSamples)
  tauVal[i] <- tau
}