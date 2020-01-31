#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
rm(list=ls()) #fir clearing env
cat("\014") # for clearing console or use Ctrl+L

#import class library of kNN model 
library(class)
#load data
knndata = read.csv("../data/knndata.txt")

print(knndata)
#partition data
X_train = knndata[,c("X1", "X2")]
Y_train = knndata[,c("Y")]

print(X_train)
print(Y_train)

#plot data
plot(X_train, col = Y_train, pch = c("A","B")[Y_train])

#create knn model
test_instance = c(0.7, 0.4)
knn_res1 = knn(X_train, test_instance, Y_train, k=1,prob=TRUE)
print(knn_res1)
knn_res5 = knn(X_train, test_instance, Y_train, k=5,prob=TRUE)
print(knn_res5)

test_instance2 = c(0.7, 0.6)
knn_res = knn(X_train, test_instance2, Y_train, k=1, prob=TRUE)
print(knn_res)

#apply k-NN in the breast cancer Wisconsin dataset.

#load the dataset as a dataframe
rm(list=ls())
# download the dataset
fileURL<-"http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
#download.file(fileURL, destfile="../data/breast-cancer-wisconsin.data") # uncomment to download
 
data = read.table("../data/breast-cancer-wisconsin.data", na.strings = "?", sep = ",")
#remove the id Column(first)
data = data[, -1]

# put names in the columns (attributes)
names(data)<-c("ClumpThickness","UniformityCellSize","UniformityCellShape","MarginalAdhesion",
               "SingleEpithelialCellSize","BareNuclei","BlandChromatin","NormalNucleoli","Mitoses","Class")

# make the class a factor 2->benign, 4->malignnant
data$Class<-factor(data$Class, levels=c(2,4), labels=c("benign","malignant"))

# set the seed
set.seed(1)
#partiotion data
ind = sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData = data[ind==1,]
validationData = data[ind==2,]

#?knn #uncomment for manual
trainData = trainData[complete.cases(trainData),]
validationData = validationData[complete.cases(validationData),]
trainDataX = trainData[,-ncol(trainData)]
trainDataY = trainData$Class
validationDataX = validationData[,-ncol(trainData)]
validationDataY = validationData$Class

prediction=knn(trainDataX, validationDataX, trainDataY, k=1)
print(prediction)

#evalution on the validation dataset
xtab=table(prediction, validationData$Class)
cat("Confusion matrix:\n")
print(xtab)

accuracy=sum(prediction==validationData$Class)/length(validationData$Class)
precision=xtab[1,1]/sum(xtab[,1])
recall=xtab[1,1]/sum(xtab[1,])
f1_score=2*(precision*recall)/(precision+recall)
cat("\nEvaluation:\n\n")
cat(paste("Accuracy:\t",format(accuracy, digits=2),"\n",sep=""))
cat(paste("Precision:\t",format(precision, digits=2),"\n",sep=""))
cat(paste("Recall:\t\t",format(recall, digits=2),"\n",sep=""))
cat(paste("F-measure:\t",format(f1_score, digits=2),"\n",sep=""))

