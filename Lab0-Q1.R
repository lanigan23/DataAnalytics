i <- 0
count <- 0
sampleSize <- 10000000
while (i < sampleSize) {
  x1 <- sample(1:sampleSize, 1)
  x2 <- sample(1:sampleSize, 1)
  if (coprime(x1, x2)) {
    count = count + 1
  }
  i = i + 1
}
result = count/sampleSize
print(result)