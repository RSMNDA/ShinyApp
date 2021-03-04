library(data.table)     
library(ggplot2)        
library(igraph)         
library(dplyr)
library(threejs) # 3D graph

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


# Interactive graph
dt.supply.chain.2015 <- filter(dt.supply.chain, date == "1/1/2015" )

all.products <- dt.supply.chain.2015 [, list(name = unique(product_name),  type = TRUE)]
all.customers <- dt.supply.chain.2015 [, list(name = unique(customer_id), type = FALSE)]

all.vertices <- rbind(all.products, all.customers )
g.supply.chain <- graph_from_data_frame(dt.supply.chain.2015 [, list(product_name, customer_id)], directed=FALSE, vertices=all.vertices)
summary(g.supply.chain) 

# 3D graph
graphjs(g.supply.chain, vertex.size = 1)
