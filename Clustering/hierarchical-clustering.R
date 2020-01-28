#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L

# Import libraries
library(cluster)
library(scatterplot3d)
# Read data from file
europe = read.csv("../data/europe.txt")

# Get distance matrix
d = dist(scale(europe))

# Perform hierarchical clustering using complete linkage
hc <- hclust(d, method = 'complete')

# Plot dendrogram
plot(hc)

#Selecting Optimal Number of Clusters

# Initialize a vector that will hold the silhouette values
slc = c()
# Iterate over the number of clusters
for (i in 2:20){
  # Create clusters
  clusters = cutree(hc, k = i)
  # Calculate and store silhouette values
  slc [i-1] = mean(silhouette(clusters, d)[, 3])
}

# Initialize a vector that will hold the silhouette values
plot(2:20, slc, type="b", xlab="Number of Clusters", ylab="Silhouette")

# Form the 7 clusters
clusters = cutree(hc, k = 7)
# Plot dendrogram
plot(hc)
# Display clusters
rect.hclust(hc, k = 7)

# Plot dendrogram
s3d = scatterplot3d(europe, angle = 125, scale.y = 1.5,
                    color = clusters)
coords <- s3d$xyz.convert(europe)
text(coords$x, coords$y, labels=row.names(europe),
     pos=sample(1:4), col = clusters)

# Calculate silhouette
model_silhouette = silhouette(clusters, d)
# Plot silhouette
plot(model_silhouette)
