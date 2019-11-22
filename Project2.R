Data <- readxl::read_xlsx("~/Documents/DataAnalytics/Project Data.xlsx")
attach(Data)
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

#Imputation using K-Nearest Neighbour
ImputeKnn <- function(Data) {
  d <- kNN(Data, k=10)
  return(d[-(18:34)])
}

#Imputation using Multiple Imputation by Chained Equations
ImputeMice <- function(Data) {
  d <- mice(Data, m=5, method = "pmm")
  summary(d)
  d <- complete(d)
  return(d)
}

Data1 <- Data2 <- Data3 <- Data4 <- Data5 <- Data
Data1 <- ImputeMean(Data1)
summary(Data1)
Data2 <- ImputeMedian(Data2)
summary(Data2)
Data3 <- ImputeRegression(Data3)
summary(Data3)
md.pattern(Data4)
Data4 <- ImputeMice(Data4)
summary(Data4)
Data5 <- ImputeKnn(Data5) 
summary(Data5)
DataGroup1 <- subset(Data5, Group==0)
DataGroup2 <- subset(Data5, Group==1)
i<-0
av1<-av2<-av3<-av4<-av5<-av6<-av7<-av8<-av9<- 0
l1<-l2<-l3<-l4<-l5<-l6<-l7<-l8<-l9<-list(0,0,0,0,0,0,0,0,0,0)
while (i<200) {
  model <- rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data5, parms=list(split="information"), method = "class")
  opt <- which.min(model$cptable[,"xerror"])
  av1 <- av1 + min(model$cptable[,"xerror"])
  tmp <- l1[[opt]]
  l1[[opt]] <- tmp + 1
  
  model2 <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data5, parms = list(split="information"), method = "class")
  opt <- which.min(model2$cptable[,"xerror"])
  av2 <- av2 + min(model2$cptable[,"xerror"])
  tmp <- l2[[opt]]
  l2[[opt]] <- tmp + 1
  
  model3 <-rpart(Response~X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data5, parms = list(split="information"), method="class")
  opt <- which.min(model3$cptable[,"xerror"])
  av3 <- av3 + min(model3$cptable[,"xerror"])
  tmp <- l3[[opt]]
  l3[[opt]] <- tmp + 1
  
  model4 <- rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=DataGroup1, parms=list(split="information"), method = "class")
  opt <- which.min(model4$cptable[,"xerror"])
  av4 <- av4 + min(model4$cptable[,"xerror"])
  tmp <- l4[[opt]]
  l4[[opt]] <- tmp + 1
  
  model5 <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=DataGroup1, parms = list(split="information"), method = "class")
  opt <- which.min(model5$cptable[,"xerror"])
  av5 <- av5 + min(model5$cptable[,"xerror"])
  tmp <- l5[[opt]]
  l5[[opt]] <- tmp + 1
  
  model6 <-rpart(Response~X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=DataGroup1,parms = list(split="information"), method="class")
  opt <- which.min(model6$cptable[,"xerror"])
  av6 <- av6 + min(model6$cptable[,"xerror"])
  tmp <- l6[[opt]]
  l6[[opt]] <- tmp + 1
  
  model7 <- rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=DataGroup2,parms=list(split="information"), method = "class")
  opt <- which.min(model7$cptable[,"xerror"])
  av7 <- av7 + min(model7$cptable[,"xerror"])
  tmp <- l7[[opt]]
  l7[[opt]] <- tmp + 1
  
  model8 <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=DataGroup2, parms = list(split="information"), method = "class")
  opt <- which.min(model8$cptable[,"xerror"])
  av8 <- av8 + min(model8$cptable[,"xerror"])
  tmp <- l8[[opt]]
  l8[[opt]] <- tmp + 1
  
  model9 <-rpart(Response~X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=DataGroup2, parms = list(split="information"), method="class")
  opt <- which.min(model9$cptable[,"xerror"])
  av9 <- av9 + min(model9$cptable[,"xerror"])
  tmp <- l9[[opt]]
  l9[[opt]] <- tmp + 1
  i=i+1
}

av1 <- av1/200
print(av1)
cp <- model$cptable[which.max(l1),"CP"]
model_prune <- prune(model, cp=cp)
pdf(file="~/Documents/DataAnalytics/models2.pdf", height = 10, width = 13)
plot(as.party(model_prune))

av2 <- av2/200
print(av2)
cp <- model2$cptable[which.max(l2),"CP"]
model2_prune <- prune(model2, cp=cp)
plot(as.party(model2_prune))

av3 <- av3/200
print(av3)
cp <- model3$cptable[which.max(l3),"CP"]
model3_prune <- prune(model3, cp=cp)
plot(as.party(model3_prune))

av4 <- av4/200
print(av4)
cp <- model4$cptable[which.max(l4),"CP"]
model4_prune <- prune(model4, cp=cp)
plot(as.party(model4))

av5 <- av5/200
print(av5)
cp <- model5$cptable[which.max(l5),"CP"]
model5_prune <- prune(model5, cp=cp)
plot(as.party(model5))

av6 <- av6/200
print(av6)
cp <- model6$cptable[which.max(l6),"CP"]
model6_prune <- prune(model6, cp=cp)
plot(as.party(model6))

av7 <- av7/200
print(av7)
cp <- model7$cptable[which.max(l7),"CP"]
model7_prune <- prune(model7, cp=cp)
plot(as.party(model7))

av8 <- av8/200
print(av8)
cp <- model8$cptable[which.max(l8),"CP"]
model8_prune <- prune(model8, cp=cp)
plot(as.party(model8))

av9 <- av9/200
print(av9)
cp <- model9$cptable[which.max(l9),"CP"]
model9_prune <- prune(model9, cp=cp)
plot(as.party(model9))
dev.off()