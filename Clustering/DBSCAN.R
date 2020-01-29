#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L

# Plot library
library(dbscan)
# Read data
mdata = read.csv("../data/mdata.txt")
# Plot data
plot(mdata)

#apply experimentaly k-means algo
#Perform k-Means clustering
model = kmeans(mdata, 2)
# Get final centers
centers = model$centers

# Get the distribution of data points among clusters
distr = model$cluster
print(distr)

# Plot data using a different color for each cluster
plot(mdata, col = model$cluster, pch = 1)

# Display centers
points(model$centers, col =4,
       pch = "+", cex = 2)

#Compute the destance of every data point with its 10 nearest neighbours
knndist = kNNdist(mdata, k = 10)

# Plot distances
plot(sort(knndist), type = 'l', xlab = "Points sorted by distance",
     ylab = "10-NN distance")

#applying DBSCAN on the given dataset using eps = 0.4 and minPoints = 10
# Construct DBSCAN model
model = dbscan(mdata, eps = 0.4, minPts = 10)
# Plot distances
plot(mdata, col = model$cluster + 1, pch = ifelse(model$cluster, 1, 4))
