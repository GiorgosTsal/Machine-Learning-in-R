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