Data <- readxl::read_xlsx("~/Documents/DataAnalytics/Project Data.xlsx")
attach(Data)
summary(Data)

#Imputation using mean
ImputeMean <- function(Data){
  Data$X1[is.na(Data$X1)] <- mean(Data$X1[!is.na(Data$X1)])
  Data$X2[is.na(Data$X2)] <- mean(Data$X2[!is.na(Data$X2)])
  Data$X3[is.na(Data$X3)] <- mean(Data$X3[!is.na(Data$X3)])
  Data$X5[is.na(Data$X5)] <- mean(Data$X5[!is.na(Data$X5)])
  Data$X6[is.na(Data$X6)] <- mean(Data$X6[!is.na(Data$X6)])
  Data$X7[is.na(Data$X7)] <- mean(Data$X7[!is.na(Data$X7)])
  Data$Y1[is.na(Data$Y1)] <- mean(Data$Y1[!is.na(Data$Y1)])
  Data$Y2[is.na(Data$Y2)] <- mean(Data$Y2[!is.na(Data$Y2)])
  Data$Y3[is.na(Data$Y3)] <- mean(Data$Y3[!is.na(Data$Y3)])
  Data$Y5[is.na(Data$Y5)] <- mean(Data$Y5[!is.na(Data$Y5)])
  Data$Y6[is.na(Data$Y6)] <- mean(Data$Y6[!is.na(Data$Y6)])
  Data$Y7[is.na(Data$Y7)] <- mean(Data$Y7[!is.na(Data$Y7)])
  return(Data)
}

#Imputation using median
ImputeMedian <- function(Data){
  Data$X1[is.na(Data$X1)] <- median(Data$X1[!is.na(Data$X1)])
  Data$X2[is.na(Data$X2)] <- median(Data$X2[!is.na(Data$X2)])
  Data$X3[is.na(Data$X3)] <- median(Data$X3[!is.na(Data$X3)])
  Data$X5[is.na(Data$X5)] <- median(Data$X5[!is.na(Data$X5)])
  Data$X6[is.na(Data$X6)] <- median(Data$X6[!is.na(Data$X6)])
  Data$X7[is.na(Data$X7)] <- median(Data$X7[!is.na(Data$X7)])
  Data$Y1[is.na(Data$Y1)] <- median(Data$Y1[!is.na(Data$Y1)])
  Data$Y2[is.na(Data$Y2)] <- median(Data$Y2[!is.na(Data$Y2)])
  Data$Y3[is.na(Data$Y3)] <- median(Data$Y3[!is.na(Data$Y3)])
  Data$Y5[is.na(Data$Y5)] <- median(Data$Y5[!is.na(Data$Y5)])
  Data$Y6[is.na(Data$Y6)] <- median(Data$Y6[!is.na(Data$Y6)])
  Data$Y7[is.na(Data$Y7)] <- median(Data$Y7[!is.na(Data$Y7)])
  return(Data)
}

#Imputation using regression
ImputeRegression <- function(Data){
  Data <- Data[,-c(1:2)]
  Data$I <- missing(Data$X1)
  model <- lm(Data$X1 ~ Group)
  #print(summary(model))
  for (i in 1:nrow(Data)) {
    if(Data$I[i]==0) {
      Data$X1[i] = 256.79 + 65.98 * Group[i]
    }
  }
  return(Data)
}
missing <- function(u) {
  x <- dim(length(u))
  x[which(is.na(u))] <- 0
  x[which(!is.na(u))] <- 1
  return(x)
}

xs <- grep("X", colnames(Data), value = T)
ys <- grep("Y", colnames(Data), value = T)
pred <- c(xs, ys)
Data1 <- Data2 <- Data3 <- Data
Data1 <- ImputeMean(Data1)
Data1
Data2 <- ImputeMedian(Data2)
Data2
Data3 <- ImputeRegression(Data3)
Data3
