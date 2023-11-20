## Arules online retail
## https://towardsdatascience.com/a-gentle-introduction-on-market-basket-analysis-association-rules-fa4b986a40ce

library(tidyverse)
library(arules)
library(arulesViz)
library(readxl)
library(lubridate)

retail <- read_excel("onlineretail.xlsx")

## Remove cases with missing values 
retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

## What time do people often purchase online? ##
## Shopping time distribution ##
retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)

retail %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")

## How many items each customer buy? ##
## Number of items per invoice distribution ##
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))
  

## Top 10 best sellers

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()


## Association rules for online retailer ##
## Turning the dataframe to transaction structure (all items in a single line)
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
library(dplyr)
#ddply() accepts a data frame, splits it into pieces based on one or more factors, computes on the pieces, and then returns the results as a data frame. We use “,” to separate different items
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

## We only need item transactions, so remove customerID and Date columns.
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

##  Write the data fram to a csv file and check whether our transaction format is correct.

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = FALSE)

## Now we have our transaction dataset, and it shows the matrix of items being bought together. We don’t actually see how often they are bought together, and we don’t see rules either

## Read as transactions
tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

## Tooltip : density: The percentage of non-empty cells in the sparse matrix. In another words, the total number of items that are purchased divided by the total number of possible items in that matrix

## item frequency plot ## 
itemFrequencyPlot(tr, topN=20, type='absolute')


## Create some rules
## Apriori algorithm in Arules library to mine frequent itemsets and association rules
## supp=0.001 and conf=0.8 to return all the rules that have a support of at least 0.1% and confidence of at least 80%

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)

## Statistical summary of rules
summary(rules)

## TOOLTIPS
#The number of rules: 89,697.
#The distribution of rules by length: a length of 6 items has the most rules.
#The summary of quality measures: ranges of support, confidence, and lift.
#The information on data mining: total data mined, and the minimum parameters we set earlier.

## Inspect top 10 rules
inspect(rules[1:10])

## plot these top 10 rules
topRules <- rules[1:10]

## Scatter plot
plot(topRules)

## Assosiaction plot
plot(topRules, method="graph")

## Rules Matrix 
plot(topRules, method = "grouped")

## Create a dataframe of the rules
rulesdf <- DATAFRAME(rules)

#-- New Graphing Network --
subrules2 <- head(sort(rules, by="confidence"), 20)
ig <- plot( subrules2, method="graph", control=list(type="items"))

ig_df <- toVisNetworkData(ig, idToLabel = FALSE)

visNetwork(ig_df$nodes, ig_df$edges) %>% 
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000))


  

