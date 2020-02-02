#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L
rm(list=ls()) #fir clearing env

#Q1
print("======Q1======")
dn = list(paste("Y", as.character(1949:1960), sep = ""), month.abb)
airmat = matrix(AirPassengers, 12, byrow= TRUE, dimnames = dn)
air = as.data.frame(t(airmat))


mean3 = mean(air[,3])
print(mean3)

#Q2
print("======Q2======")
#load data
weather = read.csv("../data/weather.txt")
library(rpart)
library(rpart.plot)

absfreq = table(weather[, c(1, 4)])
absfreq2 = table(weather[, c(2, 4)])
absfreq3 = table(weather[, c(3, 4)])
freq = prop.table(absfreq, 1)
freq2 = prop.table(absfreq2, 1)
freq3 = prop.table(absfreq3, 1)
freqSum = rowSums(prop.table(absfreq))
freqSum2 = rowSums(prop.table(absfreq2))
freqSum3 = rowSums(prop.table(absfreq3))

#We calculate the Gini index for Sunny and Rainy:
GINI_Sunny = 1 - freq["Sunny", "No"]^2 - freq["Sunny", "Yes"]^2
GINI_Rainy = 1 - freq["Rainy", "No"]^2 - freq["Rainy", "Yes"]^2
#The total Gini for Outlook is computed by:
GINI_Outlook = freqSum["Sunny"] * GINI_Sunny + freqSum["Rainy"] * GINI_Rainy
print(GINI_Outlook)


#We calculate the Gini index for Hot and Cool:
GINI_Hot = 1 - freq2["Hot", "No"]^2 - freq2["Hot", "Yes"]^2
GINI_Cool = 1 - freq2["Cool", "No"]^2 - freq2["Cool", "Yes"]^2
#The total Gini for Tempterature is computed by:
GINI_Temperature = freqSum2["Hot"] * GINI_Hot + freqSum2["Cool"] * GINI_Cool
print(GINI_Temperature)

#We calculate the Gini index for Low and High:
GINI_High = 1 - freq3["High", "No"]^2 - freq3["High", "Yes"]^2
GINI_Low = 1 - freq3["Low", "No"]^2 - freq3["Low", "Yes"]^2
#The total Gini for Humidity is computed by:
GINI_Humidity = freqSum3["High"] * GINI_High + freqSum3["Low"] * GINI_Low
print(GINI_Humidity)

#Q3
print("======Q3======")
freqall = prop.table(table(weather[, c(4)]))
Entropy_All = - freqall["No"] * log(freqall["No"]) - freqall["Yes"] * log(freqall["Yes"])

Entropy_Sunny = - freq["Sunny","No"] * log(freq["Sunny", "No"]) - freq["Sunny","Yes"] * log(freq["Sunny", "Yes"])
Entropy_Rainy = - freq["Rainy","No"] * log(freq["Rainy", "No"]) - freq["Rainy","Yes"] * log(freq["Rainy", "Yes"])

GAIN_Outlook = Entropy_All - freqSum["Sunny"] * Entropy_Sunny - freqSum["Rainy"] * Entropy_Rainy                                                                                
print(GAIN_Outlook)  

Entropy_Hot = - freq2["Hot","No"] * log(freq2["Hot", "No"]) - freq2["Hot","Yes"] * log(freq2["Hot", "Yes"])
Entropy_Cool = - freq2["Cool","No"] * log(freq2["Cool", "No"]) - freq2["Cool","Yes"] * log(freq2["Cool", "Yes"])

GAIN_Temperature = Entropy_All - freqSum2["Hot"] * Entropy_Hot - freqSum2["Cool"] * Entropy_Cool                                                                              
print(GAIN_Temperature) 

Entropy_High = - freq3["High","No"] * log(freq3["High", "No"]) - freq3["High","Yes"] * log(freq3["High", "Yes"])
Entropy_Low = - freq3["Low","No"] * log(freq3["Low", "No"]) - freq3["Low","Yes"] * log(freq3["Low", "Yes"])

GAIN_Humidity = Entropy_All - freqSum3["High"] * Entropy_High - freqSum3["Low"] * Entropy_Low                                                                              
print(GAIN_Humidity) 
                                                                                
#Q4
print("======Q4======")                                                                                
Pre = 133/(133+6)
Rec = 133/(133+15)
Fmeasure <- 2 * Pre * Rec / (Pre + Rec)
print(Fmeasure)

#Q5
rm(list=ls()) #fir clearing env
print("======Q5======") 
first = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
Alt = c("Yes","Yes","No","Yes","Yes","No","No","No","No","Yes","No","Yes")
Bar = c("Yes","Yes","No","Yes","Yes","No","No","No","No","Yes","No","Yes")
Fri = c("Yes","Yes","No","Yes","Yes","No","No","No","No","Yes","No","Yes")
Hungry = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
Patrons = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
Price = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
Rain = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
Res = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
Type = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
Est = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
WillWait = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")

#Q5
print("======Q5======")
v1=c(4.9, 3.0, 1.4, 0.2)
v2=c(5.6, 2.5, 3.9, 1.1)

euc.dist <- function(v1, v2) sqrt(sum((v1 - v2) ^ 2))
print(euc.dist(v1,v2))

#Q15 
print("=====Q15=====")

#clustering application on cdata.txt
# Import cluster library
library(cluster)
# Read data
qdata = read.csv("../data/question_data.txt")
# Plot data
plot(qdata)

#apply experimentaly k-means algo
#Perform k-Means clustering

kcenters = matrix(c(-4,10,0,0, 4, 10),ncol=2, byrow = TRUE)  
#kcenters = matrix(c(-2,0,2,0, 0, 10),ncol=2, byrow = TRUE)  
model = kmeans(qdata, centers = kcenters)
# Get final centers
centers = model$centers
print(centers)
# Get the distribution of data points among clusters
distr = model$cluster
print(distr)

# Compute cohesion
cohesion = model$tot.withinss
print(sprintf(cohesion, fmt = '%#.2f'))
# Compute separation
separation = model$betweenss
print(sprintf(separation, fmt = '%#.2f'))
# Compute silhouette
model_silhouette = silhouette(model$cluster, dist(qdata))
# Plot silhouette
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])

#Q19
print("=====Q19======")




#Q20
print("=====Q20======")
dcdata = read.csv("../data/dcdata.txt")
target=dcdata[,3]
dcdata = dcdata[,1:2]
plot(dcdata)

d = dist(dcdata)
# Perform clustering using single linkage
hc_single = hclust(d, method = "single")
# Plot the respective dendrogram
plot(hc_single)

# Perform clustering using complete linkage
hc_complete = hclust(d, method = "complete")
# Plot the respective dendrogram
plot(hc_complete)

#dbscan performance
dbmodel = dbscan(dcdata, eps = 0.75, minPts = 5)
dbmodel1 = dbscan(dcdata, eps = 1.00, minPts = 5)
dbmodel2 = dbscan(dcdata, eps = 1.25, minPts = 5)
dbmodel3 = dbscan(dcdata, eps = 1.50, minPts = 5)

#distr
clusters = dbmodel$cluster
clusters1 = dbmodel1$cluster
clusters2 = dbmodel2$cluster
clusters3 = dbmodel3$cluster

# Plot clusters
plot(dcdata, col=clusters+1, pch=3, main="DBSCAN(eps = 0.75, minPts = 5)")



# Plot clusters
plot(dcdata, col=clusters1+1, pch=3, main="DBSCAN(eps = 1.00, minPts = 5)")

# Plot clusters
plot(dcdata, col=clusters2+1, pch=3, main="DBSCAN(eps = 1.25, minPts = 5)")


# Plot clusters
plot(dcdata, col=clusters3+1, pch=3, main="DBSCAN(eps = 1.50, minPts = 5)")

# Construct k-Means model
modelkmeans = kmeans(dcdata, 2)
# Plot data
plot(dcdata, col = modelkmeans$cluster + 1)

# Form the 7 clusters
clusters = cutree(hc_single, k = 2)
# Calculate silhouette
model_silhouette_hier_sing = silhouette(clusters, d)
# Plot silhouette
plot(model_silhouette_hier_sing)
mean_silhouette_2 = mean(model_silhouette_hier_sing[, 3])
print(sprintf(mean_silhouette_2, fmt = '%#.3f'))

# Form the 7 clusters
clusters2 = cutree(hc_complete, k = 2)
# Calculate silhouette
model_silhouette_hc_complete = silhouette(clusters2, d)
# Plot silhouette
plot(model_silhouette_hc_complete)
mean_silhouette_3 = mean(model_silhouette_hc_complete[, 3])
print(sprintf(mean_silhouette_3, fmt = '%#.3f'))