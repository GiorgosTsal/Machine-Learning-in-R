#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L

# Read data
gdata = read.csv("../data/gdata.txt")
# Save 1D data in the vector x
x = gdata[, 1]
# Save cluster index in the vector y
y = gdata[, 2]

# Plot data
plot(data.frame(x, 0), ylim = c(-0.01, 0.25), col = y,
     xlab = "Data",
     ylab = "Density")
# Plot probability density function
lines(density(x), col = y)

#Calculting the parameters of the normal distibutions
#Initialize the means and the latent variables  

# Initialize means
mu = c(0, 1)
# Initialize lambdas
lambda = c(0.5, 0.5)
# Set stopping criterion
epsilon = 1e-08
# Compute initial log-likelihood
log_likelihood = sum(log(lambda[1] * dnorm(x, mean = mu[1], sd = 1) +
                           lambda[2] * dnorm(x, mean = mu[2], sd = 1)))

#loop until convergence

repeat {
  # ------ Expectation step ------
  # Find distributions given mu, lambda (and sigma)
  T1 <- dnorm(x, mean = mu[1], sd = 1)
  T2 <- dnorm(x, mean = mu[2], sd = 1)
  P1 <- lambda[1] * T1 / (lambda[1] * T1 + lambda[2] * T2)
  P2 <- lambda[2] * T2 / (lambda[1] * T1 + lambda[2] * T2)
  # ------ Maximization step ------
  # Find mu, lambda (and sigma) given the distributions
  mu[1] <- sum(P1 * x) / sum(P1)
  mu[2] <- sum(P2 * x) / sum(P2)
  lambda[1] <- mean(P1)
  lambda[2] <- mean(P2)
  # Calculate the new log likelihood (to be maximized)
  new_log_likelihood = sum(log(lambda[1] * dnorm(x, mean = mu[1], sd = 1) +
                                 lambda[2] * dnorm(x, mean = mu[2], sd = 1)))
  # Print the current parameters and the log likelihood
  cat("mu =", mu, " lambda =", lambda, " log_likelihood =", new_log_likelihood,
      "\n")
  # Break if the algorithm converges
  if (new_log_likelihood - log_likelihood <= epsilon) break
  log_likelihood = new_log_likelihood
}


#GMMs model construction using mixtools
library(mixtools)

#expectation maximization (EM) algorithm
# Apply EM using the aforementioned initialization
model <- normalmixEM(x, mu = c(0,1), sd.constr = c(1,1))
# Get means of the distrubitions
model$mu
# Get lambda values
model$lambda
# Get final log-likelihood
model$loglik

# Plot estimated probability density function
plot(model, which = 2)
# Plot original probability density function
lines(density(x), lty = 2, lwd = 2)
library(mixtools)