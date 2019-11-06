num.samples <- 2000
sample.size <- 2000
#Q1
lambda <- 1
samples <- list()
for (i in 1:num.samples) {
  exp_samples <- rep(NA, sample.size)
  U <- runif(sample.size, 0, 1)
  for (j in 1:sample.size) {
    exp_samples[j] = -log(1-U[j]/lambda)
  }
  samples[[i]] <- exp_samples
}

pdfs <- list()
for (i in 1:length(samples)) {
  pdfs[[i]] <- dexp(samples[[i]], rate=lambda)
}

hist(exp_samples[[1]])
plot(x=samples[[1]],y=pdfs[[1]], type="p")

tests <- rep(NA, num.samples)
for (i in 1:num.samples) {
  ksresults <- ks.test(samples[[i]], "pexp")
  tests[i] <- ksresults$p.value
}

mean(tests)
min(tests)
max(tests)

#Q2
k <- 5
lambda <- 3
gamma_samples <- list()
for (i in 1:num.samples) {
  sum <- rep(0, k)
  index <- sample(1:num.samples, k)
  for (j in 1:k) {
    sum <- sum + samples[[index[j]]]
  }
  gamma_samples[[i]] <- sum
}

pdfs <- list()
for (i in 1:length(gamma_samples)) {
  pdfs[[i]] <- dgamma(gamma_samples[[i]], shape = k, rate = lambda)
}

hist(gamma_samples[[1]])
plot(gamma_samples[[1]], pdfs[[1]], type = "p")

tests <- rep(NA, num.samples)
for (i in 1:num.samples) {
  ksresults <- ks.test(gamma_samples[[i]], "pgamma", k, lambda)
  tests[i] <- ksresults$p.value
}

mean(tests)
min(tests)
max(tests)