library(tidyverse)
library(dplyr)
library(tidyr)
library(arules)

#Read Data in transactions structure
tdata <- read.transactions("groceries.csv", sep="\t")

## Converting rules to dataframes to view in Rstudio using DATAFRAME function from arules
## https://rdrr.io/cran/arules/man/DATAFRAME.html
### default coercions (same as as(rules, "data.frame"))
DATAFRAME(rules)

#DATAFRAME(rules, separate = TRUE)
## If you want to seperate the items in the itemset with a "+" Sign
#DATAFRAME(rules, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = '')

# Read data in dataframe structure
data <- read_delim("groceries.csv" , ",", quote = "\"", skip = 0 , col_names = FALSE , na = c("","NA") , locale=locale(encoding = "ASCII", decimal_mark = "."), trim_ws = FALSE , progress = FALSE)

## Each row is a transaction, we will add transaction id to each row and remove empty cells
data$transactionid <- row_number(1:nrow(data))
data2 <- gather(data, key, product, -transactionid, na.rm=TRUE) %>%
  arrange(transactionid) %>% 
  filter(product !="")

##Finding association rules
## what customers are most likely to buy based on what they already chose.
##For example, if a customer already chose citrus fruit and semi-finished bread, then what is the possibility of buying margarine?”.
## This kind of if ~, then. possibility is called association rule.


