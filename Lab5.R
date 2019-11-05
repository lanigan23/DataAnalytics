rm(list=ls())
setwd("~/Documents/DataAnalytics")
Data <- read.csv("DI.csv", header=TRUE, sep=";")
str(Data)
attach(Data)
mean(Post_BP)
median(Post_BP)
Data$Post_BP[is.na(Data$Post_BP)]
mean(Data$Post_BP[!is.na(Data$Post_BP)])
median(Data$Post_BP[!is.na(Data$Post_BP)])
Data1 <- Data
Data1$Post_BP[is.na(Data1$Post_BP)] <- mean(Data1$Post_BP[!is.na(Data1$Post_BP)])
Data1$Post_BP
Data2 <- Data
Data2$Post_BP[is.na(Data2$Post_BP)] <- median(Data2$Post_BP[!is.na(Data2$Post_BP)])
Data2$Post_BP
Data <- Data[, -1]
cor(Data)
cor(Data, use="complete.obs")
symnum(cor(Data, use="complete.obs"))
Ind_Function <- function(u)
{
  x <- dim(length(u))
  x[which(is.na(u))] <- 0
  x[which(!is.na(u))] <- 1
  return(x)
}
Data$I <- Ind_Function(Data$Post_BP)
Model <- lm(Post_BP ~ Pre_BP)
summary(Model)
for (i in 1:nrow(Data))
{
  if (Data$I[i] == 0)
  { 
    Data$Post_BP[i] = 78.26482 + 0.46276 * Pre_BP[i]
  }
}
Data$Post_BP