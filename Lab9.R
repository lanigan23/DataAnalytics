num.samples <- 6
f <- function(x) {
  return(dgamma(x, 5.5, 3))
}
g <- function(x) {
  return(dgamma(x, 5, 2))
}
M <- function(f, g) {
  return(max(f/g)) 
}
h <- function(M, g) {
  return(M*g)
}
samples <- rep(NA, 2000)
x <- rep(NA, num.samples)
u <- rep(NA, num.samples)
index <- 1
i <- 0.01
m <- 0
while (i < num.samples) {
  diff <- M(f(i), g(i))
  if (m < diff) {
    m <- diff
  }
  i <- i + 0.01
}

while (i < 2000) {
  x <- runif(1, 0, 6)
  xi <- g(x)
  ui <- runif(1, 0, h(m, xi))
  if (ui < f(x)){
    samples[index] <- ui
    index <- index + 1
  }
  i <- i + 1
}

ks.test(samples, "pgamma", 5.5, 3)