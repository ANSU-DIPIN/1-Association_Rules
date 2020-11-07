################################groceries.csv###############################
install.packages('arules')
library(arules)
# Groceries data comes with the arules pkg
data("Groceries")

# view the transactions, 
inspect(head(Groceries, 3))

#  more utility functions 
size(head(Groceries)) # number of items in each observation
LIST(head(Groceries, 3)) # convert transactions to a list

#eclat() takes transactions and gives the most frequent items
#maxlen finds the maximum number of items in each itemset of frequent items.
frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) # support for frequent items
inspect(frequentItems)
itemFrequencyPlot(Groceries, topN=10, type="absolute", main="Item Frequency") # plot frequent items

# product recommendation rules
rules2 <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.8))
inspect(head(sort(rules2, by="lift")))

# adjust minlen

rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.8,minlen=2))
#writing ... [410 rule(s)]
rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.8,minlen=3))
#writing ... [410 rule(s)] 
rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.8,minlen=4))
#writing ... [381 rule(s)]
rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.8,minlen=5))
#writing ... [152 rule(s)]
rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.8,minlen=6))
#writing ... [12 rule(s)] 
inspect(head(sort(rules, by="lift")))

#the change in number of rules for different support,confidence values

rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.8))
#writing ... [410 rule(s)]
rules <- apriori (Groceries, parameter = list(supp = 0.002, conf = 0.8))
#writing ... [11 rule(s)] 
rules <- apriori (Groceries, parameter = list(supp = 0.003, conf = 0.8))
#writing ... [1 rule(s)] 

rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.7))
#writing ... [1279 rule(s)]
rules <- apriori (Groceries, parameter = list(supp = 0.002, conf = 0.9))
#writing ... [0 rule(s)]
rules <- apriori (Groceries, parameter = list(supp = 0.003, conf = 0.6))
#writing ... [120 rule(s)] 

library("arulesViz")

plot(rules2)
plot(rules2,method="grouped")
plot(rules2[1:10],method = "graph") # (for better visualization) plotting only a few rules

rules_conf <- sort (rules2, by="confidence", decreasing=TRUE) # high-confidence rules.
inspect(head(rules_conf)) #  the support, lift and confidence

rules_lift <- sort (rules2, by="lift", decreasing=TRUE) # 'high-lift' rules
inspect(head(rules_lift)) #  the support, lift and confidence




rules <- apriori(Groceries, parameter = list (supp = 0.001, conf = 0.5, maxlen=3)) # maxlen = 3 limits the elements in a rule to 3

#Remove Redundant Rules

subsetRules <- which(colSums(is.subset(rules2, rules2)) > 1) # get subset rules in vector
length(subsetRules)  #> 3913
rules <- rules2[-subsetRules] # remove subset rules.


rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="whole milk"), control = list (verbose=F)) # rules that lead to buying 'whole milk'
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.
inspect(head(rules_conf))


rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="whole milk"), control = list (verbose=F)) # those who bought 'milk' also bought..
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # high-confidence rules.
inspect(head(rules_conf))
write(rules2, file="rules_groceries.csv",sep=",")

getwd()
