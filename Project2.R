Data <- readxl::read_xlsx("~/Documents/DataAnalytics/Project Data.xlsx")
#attach(Data)
summary(Data)

#Imputation using mean
ImputeMean <- function(Data){
  predictors <- names(Data)
  predictors <- predictors[-(1:3)]
  for (i in predictors) {
    Data[[i]][is.na(Data[[i]])] <- mean(Data[[i]][!is.na(Data[[i]])])
  }
  return(Data)
}

#Imputation using median
ImputeMedian <- function(Data){
  predictors <- names(Data)
  predictors <- predictors[-(1:3)]
  for (i in predictors) {
    Data[[i]][is.na(Data[[i]])] <- median(Data[[i]][!is.na(Data[[i]])])
  }
  return(Data)
}

#Imputation using regression
ImputeRegression <- function(Data){
  predictors <- names(Data)
  predictors <- predictors[-(1:3)]
  for (i in predictors) {
    Data$I <- missing(Data[[i]])
    model <- lm(Data[[i]] ~ Group)
    for (j in 1:nrow(Data)) {
      if (Data$I[j]==0) {
        Data[[i]][j] = model$coefficients[[1]] + model$coefficients[[2]] * Group[j]
      }
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

Data1 <- Data2 <- Data3 <- Data
Data1 <- ImputeMean(Data1)
summary(Data1)
Data2 <- ImputeMedian(Data2)
summary(Data2)
Data3 <- ImputeRegression(Data3)
Data3
summary(Data3)