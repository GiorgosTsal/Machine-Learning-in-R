#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L

# Construct vector containing X-axis data
X = c(7, 3, 1, 5, 1, 7, 8, 5)

# Construct vector containing Y-axis data
Y = c(1, 4, 5, 8, 3, 8, 2, 9)

# Construct vector containing the data labels which correspond to the row names
rnames = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")
# Create the data frame
kdata = data.frame(X, Y, row.names = rnames)

# Plot the data
plot(kdata, pch = 8)
# Add the data labels
text(kdata, labels = row.names(kdata),
     pos = c(4, 2, 4, 4, 4, 4, 4, 4))

#Perform k-Means clustering
model = kmeans(kdata, centers = kdata[1:3,])
# Get final centers
centers = model$centers
print(centers)
# Get the distribution of data points among clusters
distr = model$cluster
print(distr)

#Calculate cohesion and seperation for previous clustering
# Compute WSS
cohesion = model$tot.withinss
print(cohesion)
# Compute BSS
separation = model$betweenss
print(separation)

# Plot data using a different color for each cluster
plot(kdata, col = model$cluster, pch = 15)
# Add data labels
text(kdata, labels = row.names(kdata),
     pos = c(4, 4, 4, 4, 4, 4, 2, 4))
# Display centers
points(model$centers, col = 1:length(model$centers),
       pch = "+", cex = 2)

#clustering application on cdata.txt
# Import cluster library
library(cluster)
# Read data
cdata = read.csv("../data/cdata.txt")
# The third column contains the cluster for each data point
target = cdata[, 3]
# The first two columns contain the data coordinates
cdata = cdata[, 1:2]
# Plot data
plot(cdata, col = target)

#In order to identify the optimal number of clusters for the given dataset, we will be based on
#the values of SSE metric. To that end, we will perform k-Means clustering setting the number of
#clusters from 1 to 10 and calculate SSE for each different clustering approach. 


# Initialize a vector to hold the SSE values
SSE <- (nrow(cdata) - 1) * sum(apply(cdata, 2, var))

# Calculate SSE values for the different clusterings
for (i in 2:10)
  SSE[i] <- kmeans(cdata, centers = i)$tot.withinss

plot(1:10, SSE, type="b", xlab="Number of Clusters", ylab="SSE")


#Perform k-Means clustering
model = kmeans(cdata, centers = 3)
# Get clusters' centroids
centers = model$centers
print(centers)
# Get the distribution of data points among clusters
distr = model$cluster
print(distr)

#Calculate cohesion and seperation for previous clustering
# Compute WSS
cohesion = model$tot.withinss
print(cohesion)
# Compute BSS
separation = model$betweenss
print(separation)


# Plot data using a different color for each cluster
plot(cdata, col = model$cluster, pch = 1)

# Display centers
points(model$centers, col =4,
       pch = "+", cex = 2)
# Compute Silhouette
model_silhouette = silhouette(model$cluster, dist(cdata))

# Plot Silhouette
plot(model_silhouette)
# Compute mean silhouette
mean(model_silhouette[,3])

#Heatmap construction
#In order to construct the heatmap, we first sort the data based on the cluster they belong

cdata_ord = cdata[order(model$cluster),]

heatmap(as.matrix(dist(cdata_ord)), Rowv = NA, Colv = NA,
        col = heat.colors(256), revC = TRUE)