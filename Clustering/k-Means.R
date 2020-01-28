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


