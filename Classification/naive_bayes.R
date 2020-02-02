#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L
rm(list=ls()) #fir clearing env
#install.packages("e1071")
library(e1071)

traffic=read.csv("../data/traffic.txt")

model <- naiveBayes(HighTraffic ~ ., data = traffic)

print(model)


trvalue1<-data.frame(Weather=factor("Hot",levels(traffic$Weather)),  Day=factor("Vacation",levels(traffic$Day)))
trvalue2<-data.frame(Weather=factor("Hot",levels(traffic$Weather)),  Day=factor("Weekend",levels(traffic$Day)))
print('========')
print(predict(model, trvalue1, type="raw"))
print('========')
print('--------')
print(predict(model, trvalue2, type="raw"))
print('--------')

#same using laplace smoothing
modelLapl <- naiveBayes(HighTraffic ~ ., data = traffic, laplace = 1)

print(modelLapl)

pred1 = predict(modelLapl, trvalue2)
print(pred1)
pred2 = predict(modelLapl, trvalue2, type="raw")
print(pred2)


#Second part
#apply the Naive Bayes classifier on the dataset HouseVotes84
#install.packages("mlbench")
library(e1071)
library(MLmetrics)
library(ROCR)

data(HouseVotes84, package="mlbench")
votes=na.omit(HouseVotes84)
print(votes)
#Partition Data to train and test
trainingdata=votes[1:180,]
testingdata=votes[181:232,]

#build and train the naive bayes model
modelTr = naiveBayes(Class ~ ., trainingdata)

# All rows excluding first row
xtest=testingdata[,-1]
# Just First Column with All rows
ytest=testingdata[,1]

y_pred = predict(modelTr, xtest)
print(y_pred)
y_pred_prb = predict(modelTr, xtest, type="raw")
print(y_pred_prb)

#evaluate model 
cm = ConfusionMatrix(y_pred, ytest)
pre = Precision(ytest, y_pred, "democrat")
rec = Recall(ytest, y_pred)

print(rec)
print(cm)
print(pre)

#Plotting the ROC curve
# initially requires computing TPR and FRP

pred_obj=prediction(y_pred_prb[,1], ytest, label.ordering=c("republican","democrat"))
ROCcurve = performance(pred_obj,"tpr","fpr")

#plotting the roc
plot(ROCcurve, col="blue")
abline(0,1, col="grey")

#AUC metric, get the AUC Value
auc_ROCR = performance(pred_obj, measure = "auc")
auc_ROCR = auc_ROCR@y.values[[1]]
print(auc_ROCR)