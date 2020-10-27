library(arules)


#read the data
mydata <- read.csv(file.choose())
View(mydata)



rules1 <-  apriori(as.matrix(mydata[,1:11]),parameter=list(support=0.03,confidence=0.7))
#inspect(rules1)
inspect(head(sort(rules1, by="lift")))

#adjust minlen
rules <-  apriori(as.matrix(mydata[,1:11]),parameter=list(support=0.04,confidence=0.7, minlen = 2))
#writing ... [116 rule(s)]
rules <-  apriori(as.matrix(mydata[,1:11]),parameter=list(support=0.04,confidence=0.7, minlen = 3))
#writing ... [109 rule(s)]
rules <-  apriori(as.matrix(mydata[,1:11]),parameter=list(support=0.04,confidence=0.7, minlen = 4))
#writing ... [65 rule(s)]
#inspect(rules)
inspect(head(sort(rules, by="lift")))


#the change in number of rules for different support,confidence values
rules <-  apriori(as.matrix(mydata[,1:11]),parameter=list(support=0.05,confidence=0.7))
#writing ... [79 rule(s)]


rules <-  apriori(as.matrix(mydata[,1:11]),parameter=list(support=0.03,confidence=0.8))
#writing ... [128 rule(s)]

rules <-  apriori(as.matrix(mydata[,1:11]),parameter=list(support=0.04,confidence=0.8))
#writing ... [94 rule(s)]

rules <-  apriori(as.matrix(mydata[,1:11]),parameter=list(support=0.05,confidence=0.8))
#writing ... [62 rule(s)]

library("arulesViz")

plot(rules1)
plot(rules1,method="grouped")
plot(rules1[1:30],method = "graph") 
#  plotting only few rules
#  for good visualization

write(rules, file="rules1book.csv",sep=",")
getwd()