## Arules online reatiler DataCamp Tutorial ##
# https://www.datacamp.com/community/tutorials/market-basket-analysis-r
# ## https://towardsdatascience.com/a-gentle-introduction-on-market-basket-analysis-association-rules-fa4b986a40ce

library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(lubridate)
library(dplyr)

#Read execl file into R dataframe
retail <- read_excel("./Data/onlineretail.xlsx")

## Remove cases with missing values 
#complete.cases(data) will return a logical vector indicating which rows have no missing values. Then use the vector to get only rows that are complete using retail[,].
retail <- retail[complete.cases(retail), ]

#Convert description to factor
retail <- retail %>% mutate(Description = as.factor(Description))
#Convert country to factor
retail <- retail %>% mutate(Country = as.factor(Country))
#create date format from chr
retail$Date <- as.Date(retail$InvoiceDate)

#Extract Time from invoiceDate to a new variable
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")

##Convert and edit InvoiceNo into numeric
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

#Bind transtime and invoiceno to retail dataframe
cbind(retail, TransTime)
cbind(retail, InvoiceNo)

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
#Group data by customerid and date or invoiceno and date
#ddply() accepts a data frame, splits it into pieces based on one or more factors, computes on the pieces, and then returns the results as a data frame. We use ?,? to separate different items
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
library(plyr)
transactionData <- ddply(retail, c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))

#We dont need InvoiceNo And Date any more
transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL

#Rename column to items
colnames(transactionData) <- c("items")

##  Write the data fram to a csv file and check whether our transaction format is correct.
write.csv(transactionData, file="trdata.csv", row.names = TRUE)

## Now we have our transaction dataset, and it shows the matrix of items being bought together. We don?t actually see how often they are bought together, and we don?t see rules either
#read csv as transaction type in basket format
tr <- read.transactions("trdata.csv", format = "basket", sep=",")
tr
summary(tr)
## Tooltip : density: The percentage of non-empty cells in the sparse matrix. In another words, the total number of items that are purchased divided by the total number of possible items in that matrix

#Create an item frequency plot for top 20 items
library(RColorBrewer)
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

# Create Rules
## Apriori algorithm in Arules library to mine frequent itemsets and association rules
## supp=0.001 and conf=0.8 to return all the rules that have a support of at least 0.1% and confidence of at least 80%

rules <- apriori(tr, parameter = list(supp=0.001,conf=0.8,maxlen=10))
rules <- sort(rules, by='confidence', decreasing = TRUE)

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