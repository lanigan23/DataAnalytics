num.samples <- 2000
U <- runif(num.samples)
lambda <- 1
X <- -log(1-U)/lambda #sample
h <- hist(X)
xlines <- seq (min(h$breaks), max(h$breaks), length.out = 100)
ylines <- dnorm(xlines, mean = mean(X), sd=sd(X))
ylines <- ylines * diff(h$mids[1:2]*length(X))
lines(xlines, ylines, lwd=2)
ks.test(X, "pexp")

X2 <- rgamma(num.samples, shape=5, scale = 3)
h2 <- hist(X2)
xlines <- seq (min(h2$breaks), max(h2$breaks), length.out = 100)
ylines <- dnorm(xlines, mean = mean(X2), sd=sd(X2))
ylines <- ylines * diff(h2$mids[1:2]*length(X2))
lines(xlines, ylines, lwd=2)
ks.test(X2, "gamma")