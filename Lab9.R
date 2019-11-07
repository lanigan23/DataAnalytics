num.samples <- 2000
sample.size <- 2000
f <- function(x) {
  return(dgamma(x, 5.5, 3))
}
g <- function(x) {
  return(dgamma(x, 5, 2))
}
M <- function(x) {
  return(max(f(x)/g(x))) 
}
h <- function(M, g) {
  return(M*g)
}
samples <- rep(NA, num.samples)
x <- rep(NA, num.samples)
u <- rep(NA, num.samples)
index <- 1
for (i in 1:num.samples) {
  rv <- runif(1, 0, 10)
  x[i] <- g(rv)
  m <- M(rv)
  u[i] <- runif(1, 0, h(m, x[i]))
  if (u[i] <= f(x[i])) {
    samples[index] <- x[i]
    index <- index + 1
  }
}
hist(samples)
plot(samples, f(samples))
ks.test(samples, "pgamma", 5.5 ,3)