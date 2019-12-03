num.samples <- 2000
sample.size <- 2000
lambda <- 1
q <- function(x) {
  if (x>5) {
    return(5/(x^2))
  }
  return(0)
}

samples <- list()
for (i in 1:num.samples) {
  exp_samples <- rep(NA, sample.size)
  U <- runif(sample.size, 0, 1)
  for (j in 1:sample.size) {
    exp_samples[j] = -log(1-U[j]/lambda)
  }
  samples[[i]] <- exp_samples
}


tests <- rep(NA, num.samples)
for (i in 1:num.samples) {
  ksresults <- ks.test(samples[[i]], "pexp")
  tests[i] <- ksresults$p.value
}


mean(tests)
min(tests)
max(tests)

hist(samples[[1]])