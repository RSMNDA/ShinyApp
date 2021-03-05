library(data.table)     # Run once per session
library(ggplot2)        # Run once per session
library(tidyverse)
library(knitr)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

rm(list=ls()) # clean environment

setwd("~/Documents/RSM Erasmus/NDA/Group assignment")
DataCoSupplyChainDataset <- read.csv("~/Documents/RSM Erasmus/NDA/Group assignment/archive 2/DataCoSupplyChainDataset.csv", comment.char="#")
dt.chain <- data.table(DataCoSupplyChainDataset) 

# normalize names to comply with NDA style guide
setnames(dt.chain , names(dt.chain ), 
         tolower(gsub(".", "_", names(dt.chain ), fixed=TRUE))) 

# seperate time and day
new.columns <- do.call( rbind , strsplit( as.character(dt.chain$order_date__dateorders_ ) , " " ) )
cbind(dt.chain , time = new.columns[,2] , Date = new.columns[,1] )
dt.supply.chain <- cbind(dt.chain , time = new.columns[,2] , date = new.columns[,1] )
dt.supply.chain

dt.supply.chain <- separate(dt.supply.chain, "date", c("month", "day", "year"), sep = "/")
dt.supply.chain


# Top 10 best selling products
products.most.bought <- dt.supply.chain %>% 
  group_by(product_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
products.most.bought.10 <- head(products.most.bought, n=10)

products.most.bought.10 %>% 
  ggplot(aes(x=reorder(product_name,count), y=count)) +
  geom_bar(stat="identity",fill="indian red") +
  coord_flip() +
  labs(title = "Top 10 best selling products") 

# table of number of products bought per year.
dt.products.per.year <- dt.supply.chain[order(year), .(n_products = .N), by = year][, .(year, n_products, csum_n_products = cumsum(n_products))]
dt.products.per.year

# number of products per consumer
dt.supply.chain_n <- dt.supply.chain[,n_products := .N, by = customer_id]
dt.unique.consumers <- unique(dt.supply.chain_n, by = "customer_id")

ggplot(dt.unique.consumers, aes(n_products)) + geom_histogram(binwidth = 1) +
  labs(x = "Number of products", y = "Consumer Count", title = "Histogram of number of products per consumer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(fill="indian red")

# I don't know if this is correct?!? Does not make sense

#other ideas for descriptions: 

# What time do people often purchase online?
# see https://towardsdatascience.com/a-gentle-introduction-on-market-basket-analysis-association-rules-fa4b986a40ce


