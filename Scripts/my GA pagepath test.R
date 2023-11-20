## My test to pageview relations arules ##
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(lubridate)
library(plyr)
library(dplyr)

df <- read_excel("./Data/arulesga.xlsx")
df <- df[complete.cases(df), ]

itemList <- ddply(df,c("cid"), 
                  function(df1)paste(df1$page, 
                                     collapse = ","))

itemList$cid <- NULL
colnames(itemList) <- c("items")
write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = FALSE)
tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

## item frequency plot ## 
itemFrequencyPlot(tr, topN=10, type='absolute')

## Create some rules
## Apriori algorithm in Arules library to mine frequent itemsets and association rules
## supp=0.001 and conf=0.8 to return all the rules that have a support of at least 0.1% and confidence of at least 80%

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)

summary(rules)

## Inspect top 10 rules
inspect(rules[1:10])

## plot these top 10 rules
topRules <- rules[1:10]
plot(topRules)
## Assosiaction plot
plot(topRules, method="graph")

