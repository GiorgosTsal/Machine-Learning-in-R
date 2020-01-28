#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L

data=iris
# split into training and validation datasets
set.seed(1)
ind<-sample(2,nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData<-data[ind==1,]
validationData<-data[ind==2,]

# keep only instances that do not have missing values.
trainData<-trainData[complete.cases(trainData),]
validationData<-validationData[complete.cases(validationData),]

# Correlation Feature Selection (CFS)

#The CFS method is found in theFSelectorpackage and to use it we call thecfsmethod
#beacusa Fselector is a java based lib u need to install RJava
#Guide there:https://github.com/hannarud/r-best-practices/wiki/Installing-RJava-(Ubuntu)

library(FSelector)
subset<-cfs(Species~., trainData)
f<-as.simple.formula(subset,"Species")
print(f)

library(e1071)
model<-naiveBayes(Species~., data=trainData, laplace=1)
simpler_model<-naiveBayes(f, data=trainData, laplace=1)

pred<-predict(model, validationData)
simpler_pred<-predict(simpler_model, validationData)

library(MLmetrics)
train_pred<-predict(model, trainData)
train_simpler_pred<-predict(simpler_model, trainData)

print(paste("Accuracy in training all attributes",Accuracy(train_pred, trainData$Species), sep="- "))

print(paste("Accuracy in training CFS attributes",Accuracy(train_simpler_pred, trainData$Species), sep="- "))

print(paste("Accuracy in validation all attributes",Accuracy(pred, validationData$Species), sep="- "))

print(paste("Accuracy in validation CFS attributes",Accuracy(simpler_pred, validationData$Species), sep="- "))

#apply forward search and decision trees as a model in the breast cancer Wisconsin dataset.

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

library(FSelector)
library(rpart)

evaluator<-function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(trainData))
  results=sapply(1:k,function(i) {
    test.idx<-(splits>=(i-1)/k)&(splits<i/k)
    train.idx<-!test.idx
    test<-trainData[test.idx, , drop=FALSE]
    train<-trainData[train.idx, , drop=FALSE]
    tree<-rpart(as.simple.formula(subset,"Class"), train)
    error.rate=sum(test$Class!=predict(tree, test, type="c"))/nrow(test)
    return(1-error.rate)
})
  print(subset)
  print(mean(results))
  return(mean(results))
}

subset<-forward.search(names(trainData)[-10], evaluator)

#After the search we get the following formula, where 3 out of the 9 variables were kept.

f = as.simple.formula(subset,"Class")
print(f)


#RecurrentFeatureElimination(RFE): method to select attributes


#because simple install.packages("caret") not working-neither everything else i tried i installed in my machine with:

#sudo apt-get update
#sudo apt-get install r-cran-caret

# ensure the results are repeatable
# set.seed(1)
# # load the required libraries
# 
# 
# #library(caret) <=this shit not working i give up 
# library(mlbench)
# 
# # load the data
# data(PimaIndiansDiabetes)
# data=PimaIndiansDiabetes
# 
# # split into training and validation datasets
# set.seed(1)
# ind<-sample(2,nrow(data), replace=TRUE, prob=c(0.7,0.3))
# trainData<-data[ind==1,]
# print(trainData)
# validationData<-data[ind==2,]
# trainData<-trainData[complete.cases(trainData),]
# validationData<-validationData[complete.cases(validationData),]