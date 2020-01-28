#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L


# Raw Principal Components Analysis

#First perform Principal Components analysis (PCA) without using a ready-madeR function from a library

#Step 1: Data,  First we are going to make a dataset and plot it.
d=c(2.5,2.4,0.5,0.7,2.2,2.9,1.9,2.2,3.1,3.0,2.3,2.7,2,1.6,1,1.1,1.5,1.6,1.1,0.9)
data=matrix(d, ncol=2, byrow=T)
plot(data, xlab="x1", ylab="x2")

#Step 2, subtract the mean of each attribute (x1, x2) from the respective values (centering the data)using the functionscale
data_norm<-scale(data, scale=F)
plot(data_norm, xlab="x1", ylab="x2")

#Step 3: Calculate the covariance matrix
S=cov(data_norm)
print(S)
cat("\n")

#Step 4: Computer the eigenvectors of the covariance matrix, using the svd method Singular Value Decomposition
udv=svd(S)
print(udv)

#plot the eigenvectors
# asp=1 keep the aspect ratio to 1 so that the eigenvectors are displayed appropriatelly, perpendicular
plot(data_norm, asp=1, xlab="x1", ylab="x2")
arrows(0,0,udv$u[1,1],udv$u[2,1], lwd=1)
arrows(0,0,udv$u[1,2],udv$u[2,2], lwd=0.5)

#Step 5: Choosing components
#use a barplot to display the variance accounted for each component

barplot(udv$d)
print(cumsum(udv$d)/sum(udv$d)) #he 1st component accounts for more than 95% of the variance

#Step 6: Picking the 1st component
#transform the 2D dataset into a 1D dataset using just the 1st Principal Component (PC)
data_new=t(udv$u[,1,drop=FALSE])%*%t(data_norm)
print(data_new)
plot(data_new,data_new,asp=1,xlab="x1", ylab="x2")




# PCA using an R function
# will use the "prcomp" function that performs PCA in one step
library(class)
library(stats)

data=iris
# split into training and validation datasets
set.seed(1)
ind<=sample(2,nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData=data[ind==1,]
validationData=data[ind==2,]

# keep only instances that do not have missing values.
trainData=trainData[complete.cases(trainData),]
validationData=validationData[complete.cases(validationData),]

trainDataX=trainData[,-ncol(trainData)]
logTrainDataX=log(trainDataX)
train.pca=prcomp(logTrainDataX, center=TRUE, scale.=TRUE)
sumpca = summary(train.pca)
print(sumpca)

trainDataY=trainData$Species
validationDataX=validationData[,-ncol(trainData)]
# Let's also transform the validation data
logValidationDataX=log(validationDataX)
validation.pca<-predict(train.pca, newdata=logValidationDataX)
validationDataY<-validationData$Species

# no pca prediction
prediction=knn(trainDataX, validationDataX, trainDataY, k=3)

# So let's predict using only the 7 principal components
prediction_pca=knn(train.pca$x[,1:2], validation.pca[,1:2], trainDataY, k=3)

cat("Confusion matrix:\n")
xtab=table(prediction, validationData$Species)
print(xtab)

cat("\nEvaluation:\n\n")
accuracy=sum(prediction==validationData$Species)/length(validationData$Species)
cat(paste("Accuracy:\t",format(accuracy, digits=2),"\n",sep=""))

cat("Confusion matrix PCA:\n")
xtab=table(prediction_pca, validationData$Species)
print(xtab)

cat("\nEvaluation PCA:\n\n")
accuracy=sum(prediction_pca==validationData$Species)/length(validationData$Species)
cat(paste("Accuracy PCA:\t",format(accuracy, digits=2),"\n",sep=""))

#PCA ploting by using just the first two components
plot(train.pca$x[trainData$Species=='setosa',1:2], col="blue", ylim=c(-3,3),xlim=c(-3,3), asp=1)
points(train.pca$x[trainData$Species=='versicolor',1:2], pch=3, col="red")
points(train.pca$x[trainData$Species=='virginica',1:2], pch=4, col="green")
