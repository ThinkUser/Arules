#-- Arules Groceries V2 #-- 
#-- http://www.salemmarafi.com/code/market-basket-analysis-with-r/
#-- https://rpubs.com/sbushmanov/180410

library(arulesViz)
library(datasets)
library(tidyverse)
library(dplyr)
library(tidyr)
library(arules)

#-- Take the data from the arules data sets and from the csv files --#
data(Groceries)
summary(Groceries)

#-- Get item labels --#
itemLabels(Groceries)[1:10] #-- [1:10] can be dropped to show all items

#-- Most frequent Items --#
itemFrequency(Groceries, type="relative")
itemFrequency(Groceries, type="absolute")

#-- Create an item frequency plot for the top 20 items absolute counts (vertical) --#
itemFrequencyPlot(Groceries,topN=20,type="absolute")

#-- Top 10 most frequent items by absolute counts (horizontal)
itemFrequencyPlot(Groceries,
                  type="absolute",
                  topN=10,
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, absolute')

#-- Top 10 most frequent items by frequency, relative (horizontal) --#
itemFrequencyPlot(Groceries,
                  type="relative",
                  topN=10, #can be changed to the number of interest
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency, relative')

#-- Search for frequent itemsets --#
itemsets <- apriori(Groceries,
                    parameter = list(support=.001,
                                     minlen=2,
                                     target='frequent'))

#-- Simple contingency table(useful tool for examining relationships between categorical variables) --#
tbl <- crossTable(Groceries, sort=TRUE)
tbl[1:5,1:5]

#-- How many times was milk purchased with flour? --#
tbl['whole milk','flour']

#-- Add lift to the contingency table --#
tbl_lift <- crossTable(Groceries, measure='lift',sort=T)[1:5,1:5]
tbl_lift[1:5,1:5]


#-- Get the rules --#
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
#-- Converting rules to dataframes to view in Rstudio using DATAFRAME function from arules --#
#-- https://rdrr.io/cran/arules/man/DATAFRAME.html --#
#-- default coercions (same as as(rules, "data.frame")) --#
rulese_df <- DATAFRAME(rules)
#-- If you want to seperate the items in the itemset with a "+" Sign --#
#-- DATAFRAME(rules, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = '') --#


#-- Show the top 5 rules, but only 2 digits --#
options(digits=2)
inspect(rules[1:5])

summary(rules)

#-- the most likely rules (sort by confidence) --#
rules_conf<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules_conf[1:5])

#-- Want shorter rules ? use the maxlen parameter --#
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))

#-- rules by lift --#
rules_lift <-sort(rules, by="lift", decreasing=TRUE)
inspect(rules_lift[1:5])

#-- Show particular rules for a specific product --#
inspect(subset(rules, subset = rhs %pin% "whole milk"))

#-- Targeting Items - How to Find Rules Related To Given Item/s ? --#
#-- achieved by modifying the appearance parameter in the apriori() function --#
#-- http://www.salemmarafi.com/code/market-basket-analysis-with-r/ --#
#-- What are customers likely to buy before buying whole milk --#

rules_rhs_wholemilk<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))

rules_rhs_wholemilk<-sort(rules_rhs_wholemilk, decreasing=TRUE,by="confidence")
inspect(rules_rhs_wholemilk[1:5])

#-- To find out what products were purchased after/along with product X --#
#-- What are customers likely to buy if they purchase whole milk? --#
rules_lhs_wholemilk<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules_lhs_wholemilk<-sort(rules_lhs_wholemilk, decreasing=TRUE,by="confidence")
inspect(rules_lhs_wholemilk[1:5])

#-- Visualization --#
plot(rules,method="graph",interactive=TRUE,shading=NA)

#-- Interactive Scatter Plot --#
plot(rules, interactive = T)


####################################################
#-- PRO TIP when lift is lower than 1, chisquare --# 
####################################################

#--  whole milk goes well with all products but soda. So, judged by lift, we are on the way to claim that soda is a substitute for “whole milk” for some people: they tend to buy either one or the other, but buying them together is a relatively rare event.
#-- To convince ourselves that lower than 1 lift is not due to chance, let’s apply chiSquared test:
crossTable(Groceries, measure='chi')['whole milk', 'soda']

#-- [1] 0.0004535
#--  Indeed, the low p-value excludes possibility that lift less than 1 is due to chance.
#-- More Details https://rpubs.com/sbushmanov/180410

#####################################
#               BETA               #
#####################################
#-- Remove Unnecessary (redundant) Rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#-- Clean Rules
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)
