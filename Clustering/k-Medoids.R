#k-Medoids Application
# Vector that holds Rank data
Rank = c("High", "Low", "High", "Low", "Low", "High")
# Vector that holds Topic data
Topic = c("SE", "SE", "ML", "DM", "ML", "SE")
# Data frame
conferences = data.frame(Rank, Topic)

# Import library
library(cluster)
# Create k-Medoids model
model = pam(conferences, 3)

# Get medoids
medoids = model$medoids
# Get distribution of samples into clusters
distro = model$clustering

conf = conferences[model$id.med,]
# Get the different ranks
L1 = levels(conferences$Rank)
# Get the different topics
L2 = levels(conferences$Topic)
# Plot data
plot(model$data, xaxt = "n", yaxt = "n", pch = 15, col = model$cluster)
# Insert axes labels
axis(1, at = 1:length(L1), labels = L1)
axis(2, at = 1:length(L2), labels = L2)
# Display medoids
points(conferences[model$id.med,], col = 1:3, pch = "o", cex = 2)