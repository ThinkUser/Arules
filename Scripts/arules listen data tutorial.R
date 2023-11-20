## Listen Data Market Basket Analysis ##
## https://www.listendata.com/2015/12/market-basket-analysis-with-r.html

# Read CSV File
mydata  <- read.csv(file="./Data/MBA.csv", header = TRUE, sep=",")
# See the top 10
head(mydata,n =10)

# Split data (creates a list)
dt <- split(mydata$Products, mydata$ID)

# Load arules library
library(arules)

# Convert data into transaction level data
dt2 = as(dt, "transactions")
summary(dt2)

# Most frequent Items
itemFrequency(dt2, type="relative")
itemFrequencyPlot(dt2, topN=5)

# Rules options
# aggregated data
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8))
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, minlen = 3))
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, maxlen = 4))

# Convert Rules into data frame
# will keep the rules in the same cell lhd => rhs
rules3 = as(rules, "data.frame")
# you can use DATAFRAME and it will split lhs and rhs
rules4 <- DATAFRAME(rules)

# save rules to csv file
write(rules, "rules.csv", sep=",")

# Show particular rules for a specific product
inspect(subset(rules, subset = rhs %pin% "Product H"))

# Show the top 10 rules
inspect(rules[1:10])

# Summary information
summary(rules)

# Sort by lift
rules <- sort(rules, by="lift", decreasing = TRUE)

# Remove Unnecessary (redundant) Rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
rules<-rules.pruned
summary(rules)

#Clean Rules
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)

# What are customers likely to buy before they purchase "Product A"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="Product A"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# What are customers likely to buy if they purchased "Product A"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="rhs",lhs="Product A"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

