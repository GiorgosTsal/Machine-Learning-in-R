#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L


#load the dataset as a dataframe
rm(list=ls())
# download the dataset
fileURL<-"https://media.geeksforgeeks.org/wp-content/uploads/social.csv"
#download.file(fileURL, destfile="../data/social.csv") # uncomment to download


# Importing the dataset 
dataset = read.csv('../data/social.csv') 

# Taking columns 3-5 
dataset = dataset[3:5] 

# Encoding the target feature as factor 
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1)) 

# Splitting the dataset into the Training set and Test set 
#install.packages('caTools') 
library(caTools) 

set.seed(123) 
split = sample.split(dataset$Purchased, SplitRatio = 0.75) 

training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE) 

# Feature Scaling 
training_set[-3] = scale(training_set[-3]) 
test_set[-3] = scale(test_set[-3]) 


# Fitting SVM to the Training set 
#install.packages('e1071') 
library(e1071) 

classifier = svm(formula = Purchased ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear') 

# Predicting the Test set results 
y_pred = predict(classifier, newdata = test_set[-3]) 
print(y_pred)


# Making the Confusion Matrix 
cm = table(test_set[, 3], y_pred) 


# installing library ElemStatLearn 
#install.packages("../libs/ElemStatLearn_2015.6.26.2.tar.gz",repos=NULL, type="source") #install R package from source =>https://cmdlinetips.com/2012/05/how-to-install-a-r-package-locally-and-load-it-easily/
library(ElemStatLearn) 

# Plotting the training data set results 
set = training_set 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('Age', 'EstimatedSalary') 
y_grid = predict(classifier, newdata = grid_set) 

#If you get:
#Error in plot.new() : figure margins too large". To avoid such errors you can first check par("mar") output. You should be getting:
#[1] 5.1 4.1 4.1 2.1
#To change that write:
#par(mar=c(1,1,1,1)) #This should rectify the error. Or else you can change the values accordingly

plot(set[, -3], 
     main = 'SVM (Training set)', 
     xlab = 'Age', ylab = 'Estimated Salary', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3')) 

#Visualizing the Test set results

set = test_set 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('Age', 'EstimatedSalary') 
y_grid = predict(classifier, newdata = grid_set) 

plot(set[, -3], main = 'SVM (Test set)', 
     xlab = 'Age', ylab = 'Estimated Salary', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3')) 

