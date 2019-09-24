i <- 0
count <- 0
sampleSize <- 1000000
while (i < sampleSize) {
  x3 <- c(sample(1:60, 1), sample(1:60, 1))
  if (diff(x3) < 20) {
    count = count + 1 
  }
  i = i + 1
}
result = count/sampleSize
print(result)