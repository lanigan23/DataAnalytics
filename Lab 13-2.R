# Copy the data file to the clipboard and then type the following instruction in R: 
data <- read.csv("~/Documents/DataAnalytics/LEData(1).csv", header=TRUE, sep=",")
summary(data) # a summary of the data set 
# assign names to environmental variables 
x <- data[,6] 
y <- data[,7] 
z <- data[,8] 
# assign name to matrix of species abundance data 
abcde <- data[,1:5] 
abcde 
# Computing Step 1 
# graphs of environmental variables and species abundances 
hist(x) # histogram of Depth data 
plot(x,y) # scatter plot of Pollution vs Depth 
boxplot(x~data[,9]) # box plot 
# graph of standardized values of three environmental variables 
xst <- (x-mean(x))/sd(x) # standardize x 
yst <- (y-mean(y))/sd(y) # standardize y 
zst <- (z-mean(z))/sd(z) # standardize y 
plot(xst,yst) # plot standardized variables 
abline(lm(yst~xst),col="red") # add trend line (linear regression) 
# all pairwise scatterplots of xst,yst,zst, 
xyz.st<-cbind(xst,yst,zst) # assign name to matrix of standardized environmental data 
xyz.st 
pairs(xyz.st) 
pairs(xyz.st,upper.panel=panel.smooth) # add trend lines 
# all pairwise scatterplots of abcde 
pairs(abcde) 
# DISTANCES BETWEEN SAMPLES 
# Computing Step 2 
# calculate Euclidean distances based on environmental data (xst,yst) 
Dxyz<- dist(cbind(xst,yst,zst)) # distance matrix based on Depth (xst),Pollution (yst),Temperature 
(zst) 
Dxyz 
as.matrix(Dxyz)[1:6,1:6] # inspect a subset of the distance matrix 



# Computing step 3 
# calculate Bray-Curtis distances based on species abundance data (counts abcde) 
BC<-matrix(rep(0,900),nrow=30) 
for(i in 1:30) { 
  for(j in 1:30) { 
    BC[i,j]<-100*sum(abs(abcde[i,]-abcde[j,]))/(sum(abcde[i,])+sum(abcde[j,])) 
  } 
} 
BC[1:6,1:6] # inspect a subset of the distance matrix 
# Bray-Curtis distances based on fourth root transformation of frequencies 
sqrt(sqrt(abcde)) 
abcdet<-sqrt(sqrt(abcde)) 
BCt<-matrix(rep(0,900),nrow=30) 
for(i in 1:30) { 
  for(j in 1:30) { 
    BCt[i,j]<-100*sum(abs(abcdet[i,]-abcdet[j,]))/(sum(abcdet[i,])+sum(abcdet[j,])) 
  } 
} 
BCt[1:6,1:6] # inspect a subset of the distance matrix 
# to get some idea about relationship between BC and BCt 
cor(BC[,1],BCt[,1]) 
cor(BC[,3],BCt[,3]) 
cor(BC[,5],BCt[,5]) 



# Computing step 4 
# calculations of row profiles and chi-square distances 



sum(abcde) 
apply(abcde,1,sum) # row sums (total in each sample) 
apply(abcde,2,sum) # column sums (total for each species) 
sum(apply(abcde,2,sum)) # grand total 
abcde/apply(abcde,1,sum) # row profiles 



rowpro<-abcde/apply(abcde,1,sum) # each row dedided by row-sum 
rowpro 
avepro<-apply(abcde,2,sum)/sum(abcde) # col-sum devided by total 
avepro 



D <- diag(1/sqrt(avepro)) 
D 
# calculate chi-square distances similar to d(x,y) = sum( (xi-yi)^2 / (xi+yi) ) / 2; 
chid<-dist(as.matrix(rowpro)%*%diag(1/sqrt(avepro))) 
chid 
# compare Bray-Curtis to chi-square 
plot(chid,as.dist(BC)) 
cor(chid,as.dist(BC)) 


# CLUSTER ANALYSIS 
# Computing step 5 cluster analyses of xy 
# clustering of weighted Euclidean distances based on x and y 



treeD<-rect.hclust(Dxyz) 
plot(treeD) 
cutree(treeD,4) 


plot(xst,yst,type="n") 
text(xst,yst,cutree(treeD,4)) # add labels of cluster affiliation (cutree) 



# Computing step 6 cluster analyses 
# cluster analysis of Bray-Curtis dissimilarities between sites 



treeBC<-hclust(as.dist(BC)) 
plot(treeBC) 
cutree(treeBC,3) 
plot(xst,yst,type="n") 
text(xst,yst,cutree(treeBC,3)) # add labels of cluster affiliation (cutree) 



# Computing step 7 cluster analyses 
# cluster analysis of chi-square distances between sites 



treechi<-hclust(chid) 
plot(treechi) 
cutree(treechi,3) 
plot(xst,yst,type="n") 
text(xst,yst,cutree(treechi,3)) # add labels of cluster affiliation (cutree) 
