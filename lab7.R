rm(list=ls())
setwd("~/Documents/DataAnalytics/")
Data = read.csv("siswave3v4impute3.csv", header=TRUE, sep=",")
attach(Data)
head(Data)
glm.sign <- glm(I(rearn>0) ~ men + age60m + white + 
  immig + educ_r + ssi + welfare + charity, data=Data, family = binomial(link = logit))
glm.sign

lm.ifpos.sqrt <- lm (I(sqrt(rearn)) ~ men + age60m + white + 
  immig + educ_r + ssi + welfare + charity, data=Data, subset=rearn>0)
lm.ifpos.sqrt

pred.sign <- rbinom(n, 1, predict(glm.sign, data, type = "response"))
pred.pos.sqrt <- rnorm(n, predict(lm.ifpos.sqrt, Data), sigma.hat(lm.ifpos.sqrt))