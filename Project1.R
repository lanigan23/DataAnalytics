Data <- readxl::read_xlsx("~/Documents/DataAnalytics/Project Data.xlsx")
str(Data)
model <- rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data, minsplit = 10, minbucket = 1, parms=list(split="information"), method = "class")
summary(model)
print(model)
plot(as.party(model))

model2 <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data, minsplit = 10, minbucket = 1, parms = list(split="information"), method = "class")
summary(model2)
print(model)
plot(as.party(model2))

model3 <-rpart(Response~X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data, minsplit = 10, minbucket = 1, parms = list(split="information"), method="class")
summary(model3)
print(model3)
plot(as.party(model3))

Data1 <- Data
Data1$Group = 0

model4 <- rpart(Response~Group+X1+X2+X3+X4+X5+X6+X7, data=Data1, minsplit = 10, minbucket = 1, parms=list(split="information"), method = "class")
summary(model4)
print(model4)
plot(as.party(model4))

model5 <- rpart(Response~Group+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data1, minsplit = 10, minbucket = 1, parms = list(split="information"), method = "class")
summary(model5)
print(model5)
plot(as.party(model5))

model6 <-rpart(Response~Group+X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data1, minsplit = 10, minbucket = 1, parms = list(split="information"), method="class")
summary(model6)
print(model6)
plot(as.party(model6))

Data2 <- Data
Data2$Group = 1
str(Data2)

model7 <- rpart(Response~Group+X1+X2+X3+X4+X5+X6+X7, data=Data2, minsplit = 10, minbucket = 1, parms=list(split="information"), method = "class")
summary(model7)
print(model7)
plot(as.party(model7))

model8 <- rpart(Response~Group+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data2, minsplit = 10, minbucket = 1, parms = list(split="information"), method = "class")
summary(model8)
print(model8)
plot(as.party(model8))

model9 <-rpart(Response~Group+X1+X2+X3+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data2, minsplit = 10, minbucket = 1, parms = list(split="information"), method="class")
summary(model9)
print(model9)
plot(as.party(model9))