#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

library(rpart)
library(rpart.plot)
weather=read.csv("../data/weather.txt")

#To see the split for the variable Outlook, execute the following command
model<-rpart(Play~Outlook, method="class", data=weather, minsplit=1)
rpart.plot(model, extra = 104, nn = TRUE)

absfreq=table(weather[,c(1,4)])
freq=prop.table(absfreq,1)
freqSum=rowSums(prop.table(absfreq))

#calculate the Gini index for Sunny and Rainy
GINI_Sunny=1-freq["Sunny","No"]^2-freq["Sunny","Yes"]^2
GINI_Rainy=1-freq["Rainy","No"]^2-freq["Rainy","Yes"]^2

#The total Gini for Outlook is computed by
GINI_Outlook=freqSum["Sunny"]*GINI_Sunny+freqSum["Rainy"]*GINI_Rainy

#compute the total entropy of thedataset
freq=prop.table(table(weather[,c(4)]))
Entropy_All=-freq["No"]*log(freq["No"])-freq["Yes"]*log(freq["Yes"])

#Then, for Outlook, we build the following frequency arrays
absfreq=table(weather[,c(1,4)])
freq=prop.table(absfreq,1)
freqSum=rowSums(prop.table(absfreq))

#calculate the entropy for Sunny and Rainy
Entropy_Sunny=-freq["Sunny","No"]*log(freq["Sunny","No"])-freq["Sunny","Yes"]*log(freq["Sunny","Yes"])
Entropy_Rainy=-freq["Rainy","No"]*log(freq["Rainy","No"])-freq["Rainy","Yes"]*log(freq["Rainy","Yes"])

#the tolat information for Outlook 
GAIN_Outlook=Entropy_All-freqSum["Sunny"]*Entropy_Sunny-freqSum["Rainy"]*Entropy_Rainy

# Building the Decision Tree
model<-rpart(Play~Outlook+Temperature+Humidity,
             method="class",
             data=weather,
             minsplit=1,
             minbucket=1,
             cp=-1)

rpart.plot(model, extra=104, nn=TRUE)

#Example on iris dataset
#we shall only keep the first 2 columns of the dataset anddrop the last 50 instances, 
#so that we now have a binary classification problem with two predictor features and one target class.

iris2=iris[,c(1,2,5)]
iris2$Species[c(101:150)]=iris2$Species[c(21:70)]
iris2$Species=factor(iris2$Species)

#split the dataset into training and testing data
trainingdata=iris2[c(1:40,51:90,101:140),]
testdata=iris2[c(41:50,91:100,141:150),]


#Build a decision tree using the training data (the minsplit parameter of rpart should be set to20, which is the default)
model<-rpart(Species~.,
             method="class",
             data=trainingdata,
             minsplit=20)

rpart.plot(model, extra=104, nn=TRUE)

xtest=testdata[,1:2]
ytest=testdata[,3]
pred=predict(model, xtest, type="class")

#Evaluation Metrics
library(MLmetrics)

cm=ConfusionMatrix(pred, ytest)
accuracy=Accuracy(pred, ytest)
precision=Precision(ytest, pred,'versicolor')
recall=Recall(ytest, pred,'versicolor')
f1=F1_Score(ytest, pred,'versicolor')
data.frame(precision, recall, f1)
