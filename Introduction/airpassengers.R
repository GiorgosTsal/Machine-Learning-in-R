#load data
data("AirPassengers")
AP <- AirPassengers

#plot data
plot(AP, ylab="Passengers (1000s)", type="o", pch =20)

#decomposing the Data
AP.decompM <- decompose(AP, type = "multiplicative")
plot(AP.decompM)

#model fitting-inspecting the trend component in the decomposition plot 
t <- seq(1, 144, 1)
modelTrend <- lm(formula = AP.decompM$trend ~ t)
predT <- predict.lm(modelTrend, newdata = data.frame(t))

plot(AP.decompM$trend[7:138] ~ t[7:138], ylab="T(t)", xlab="t",
     type="p", pch=20, main = "Trend Component: Modelled vs Observed")
lines(predT, col="red")

layout(matrix(c(1,2,3,4),2,2))
plot(modelTrend)

print(summary(modelTrend))

#for 1961 (time 145 to 156 inc.), the trend component (T) is
Data1961 <- data.frame("T" = 2.667*seq(145, 156, 1) + 84.648, S=rep(0,12), e=rep(0,12),
                       row.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

print(Data1961)

#seasonal component
print(AP.decompM$seasonal)

Data1961$S <- unique(AP.decompM$seasonal)
print(Data1961)

#ploting the density estimation of random error
plot(density(AP.decompM$random[7:138]),
     main="Random Error") #Values 1:6 & 139:44 are NA

print(mean(AP.decompM$random[7:138]))

Data1961$e <- 1

sd_error <- sd(AP.decompM$random[7:138])
print(sd_error)

Data1961$R <- Data1961$T * Data1961$S * Data1961$e                  #Realistic Estimation
Data1961$O <- Data1961$T * Data1961$S * (Data1961$e+1.95*sd_error)  #Optimistic Estimation
Data1961$P <- Data1961$T * Data1961$S * (Data1961$e-1.95*sd_error)  #Pessimistic Estimation
print(Data1961)

#plotting the above predictions
xr = c(1,156)
plot(AP.decompM$x, xlim=xr, ylab = "Passengers (100s)", xlab = "Month")
lines(data.frame(AP.decompM$x))
lines(Data1961$R, x=seq(145,156,1), col="blue")
lines(Data1961$O, x=seq(145,156,1), col="green")
lines(Data1961$P, x=seq(145,156,1), col="red")
      
