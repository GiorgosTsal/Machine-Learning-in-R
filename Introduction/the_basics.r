#ls() # With ls() one can check all the variables existing in the current R session
#rm(list=ls()) # To delete all the variables in the current session you can use the call

#VECTORS

a<-c(10,5,3,100,-2,5,-50)
a
a[c(1,3:4)] #Select the elements of the vectorawith indices 1, 3, 4, and 5


seq(1,6, by=1)
seq(1,6, by=2)
seq(1,by=2, length=6)

class(a)

a>5 #To check whichaelements have a value greater than 5

which(a>5) #To return the indices of vectora, for which the values are TRUE

length(a) #legnth of vector

c<-1:7
rbind(a,c)

cbind(a,c)

#MATRICES

matrix(10,3,2)
matrix(c(1,2,3,4,5,6),3,2)
matrix(c(1,2,3),3,2)

args(matrix)

m=matrix(1:9, byrow=TRUE, nrow=3)
m

m2<-rbind(m, m)
m2
rowSums(m2)
colSums(m2)
mean(m2)

am<-matrix(10:18, byrow=TRUE, nrow=3)
am

bm<-matrix(c(3,6,7,10,8,1,2,3,2), byrow=TRUE, nrow=3)
bm

am*bm

am%*%bm

t(am)

#Data frames
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

data<-read.csv(file="../data/cdata.txt", header=TRUE)
head(data)

# type of the variable
class(data)

# dimensions
dim(data)

# structure
str(data)

# statistical summarization of the data frame
summary(data)

# first value in data
data[1,1]

# doesn't have to start from the beginning
data[4:10,1:2]

# specific rows and columns
data[c(3,10,22,42), c(3)]

# All columns from row 5
data[5, ]

# All rows from column 3
data[,3]

#We need to apply the functionmaxormeanper row or columnrespectivelly. 
#Luckily there is the functionapplythat applies a function for each one of the“margins”, 1 for rows and 2 for columns:

avg<-apply(data,1,mean)
plot(avg)

max<-apply(data, 1, max)
plot(max)

min<-apply(data, 1, min)
plot(min)

#Functions

fahr_to_kelvin<-function(temp) {
  kelvin<-((temp-32)*(5/9))+273.15
  return(kelvin)
}

# freezing point of water
fahr_to_kelvin(32)

# boiling point of water
fahr_to_kelvin(212)

kelvin_to_celsius<-function(temp) {
  celsius<-temp-273.15
  return(celsius)
}

#absolute zero in Celsius
kelvin_to_celsius(0)

fahr_to_celsius<-function(temp) {
  temp_k<-fahr_to_kelvin(temp)
  result<-kelvin_to_celsius(temp_k)
  return(result)
}

# freezing point of water in Celsius
fahr_to_celsius(32.0)

# freezing point of water in Celsius
kelvin_to_celsius(fahr_to_kelvin(32.0))

#for loops
best_practice<-c("Let","the","computer","do","the","work")

print_words<-function(sentence) {
  for(word in sentence) {
    print(word)
  }
}

print_words(best_practice)

#Making decisions (if&else)
num<-37
if(num>100) {
  print("greater")
}else{
  print("not greater")
}

#Datasets and statistics
#By running the function data() we can see some datasets that are currently included in the R installation

#data()
head(iris)
str(iris)
summary(iris)

iris$Sepal.Length #to get full attribute of sepal.lenght
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
range(iris$Sepal.Length)
sort(iris$Sepal.Length)
length(iris$Sepal.Length)

#Factors
sex<-factor(c("male","female","female","male"))
levels(sex)
nlevels(sex)

food<-factor(c("low","high","medium","high","low","medium","high"))
levels(food)

food<-factor(food, levels=c("low","medium","high"), ordered=TRUE)
levels(food)
min(food)
