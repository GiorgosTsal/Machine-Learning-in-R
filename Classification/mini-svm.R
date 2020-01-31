rm(list=ls()) #fir clearing env
cat("\014") # for clearing console or use Ctrl+L

X1=c(2,2,-2,-2,1,1,-1,-1)
X2=c(2,-2,-2,2,1,-1,-1,1)
Y=c(1,1,1,1,2,2,2,2)
X_train=data.frame(X1,X2)
alldata=data.frame(X1, X2, Y)

#X_train=alldata[,c("X1","X2")] alternative of X_train split


#plot data
plot(X_train, col = Y, pch = c("1","2")[Y])

svm_model=svm(Y~., kernel="radial", type="C-classification", data=alldata)
plot(svm_model, alldata)
plot(alldata[,-3],col=(Y+3)/2)
points(alldata[svm_model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm_model$coefs) %*% svm_model$SV
b <- -svm_model$rho
a=-b/w[1,2]
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(0,b, col="blue", lty=3)

mygrid=expand.grid(X1, X2)
colnames(mygrid)=colnames(X_train)[1:2]

pred=predict(svm_model, mygrid)
