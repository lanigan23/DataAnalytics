Data <- read.csv("~/Documents/DataAnalytics/PCAData.csv", header=TRUE, sep=";")

install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)

Data <- Data[complete.cases(Data),]
Data_corr <- cor(Data)
Data_corr

factors_data <- fa(r=Data_corr, nfactors=6)
factors_data

fit <- princomp(Data, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit, type="lines")
biplot(fit)