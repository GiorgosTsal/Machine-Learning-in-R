#in order to set current directory as root
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
cat("\014") # for clearing console or use Ctrl+L

library(arules)
fileURL <- "http://fimi.ua.ac.be/data/retail.dat.gz"
#download.file(fileURL, destfile="../data/retail.data.gz") #uncomment to download dataset

# Read the data in basket format
trans = read.transactions("../data/retail.data.gz", format = "basket", sep=" ");
inspect(trans[1:10])

print(summary(trans))

rules <- apriori(trans, parameter = list(support = 0.01, confidence = 0.6))
quality(rules) <- round(quality(rules), digits=3)
rules

inspect(rules)