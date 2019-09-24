Data <- read.csv("DT-Credit.csv", header=TRUE, sep=";")
str(Data)
attach(Data)
DT_Model <- rpart(RESPONSE~.,data=Data, control=rpart.control(minsplit = 60, minbucket = 30, maxdepth = 4))
plot(as.party(DT_Model))
print(DT_Model)

Target=ifelse(RESPONSE==1,'Y','N')
Data <- data.frame(Data, Target)
Data1=Data[,-32]
DT_Model1 <- rpart(Target~., data=Data1, control=rpart.control(minsplit = 60, minbucket = 30, maxdepth = 8))
plot(as.party(DT_Model1))
print(DT_Model1)

DT_Model2<-rpart(Target~.,data=Data1, control=rpart.control(minsplit = 60, minbucket = 30, maxdepth = 8))
plot(as.party(DT_Model2))
print(DT_Model2$cptable)
opt <- which.min(DT_Model2$cptable[,"xerror"])

cp<- DT_Model2$cptable[opt,"CP"]
DT_Model_pruned<-prune(DT_Model2, cp=cp)
plot(as.party(DT_Model_pruned))