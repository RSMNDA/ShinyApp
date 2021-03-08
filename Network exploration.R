library(data.table)     # Run once per session
library(ggplot2)        # Run once per session
library(igraph)         # Run once per session
library(dplyr)

rm(list=ls()) # clean environment

setwd("~/Documents/RSM Erasmus/NDA/Group assignment")
DataCoSupplyChainDataset <- read.csv("~/Documents/RSM Erasmus/NDA/Group assignment/archive 2/DataCoSupplyChainDataset.csv", comment.char="#")
dt.chain <- data.table(DataCoSupplyChainDataset) 

# normalize names to comply with NDA style guide
setnames(dt.chain , names(dt.chain ), 
         tolower(gsub(".", "_", names(dt.chain ), fixed=TRUE))) 

new.columns <- do.call( rbind , strsplit( as.character(dt.chain$order_date__dateorders_ ) , " " ) )
cbind(dt.chain , time = new.columns[,2] , Date = new.columns[,1] )
dt.supply.chain <- cbind(dt.chain , time = new.columns[,2] , date = new.columns[,1] )
dt.supply.chain

# Create graph  ---------------------------
# only for a particular day
dt.supply.chain.2015 <- filter(dt.supply.chain, date == "1/1/2015" )

dt.unique.products <- dt.supply.chain.2015 [, list(name = unique(product_name))]
dt.unique.products 

dt.unique.customer_id <- dt.supply.chain.2015[ , list(name = unique(customer_id))]
dt.unique.customer_id 

all.products <- dt.supply.chain.2015 [, list(name = unique(product_name),  type = TRUE)]
all.customers <- dt.supply.chain.2015 [, list(name = unique(customer_id), type = FALSE)]

all.vertices <- rbind(all.products, all.customers, )
g.supply.chain <- graph.data.frame(dt.supply.chain.2015 [, list(product_name, customer_id)], directed=FALSE, vertices=all.vertices)
summary(g.supply.chain) 

#sg <- induced_subgraph(g.supply.chain, 1:100)
sg <- delete.vertices(g.supply.chain, degree(g.supply.chain) == 0 )
plot(sg, vertex.label = NA, margin = 0, edge.arrow.size = 0, edge.arrow.width = 0, vertex.size = 5 )

# Bipartite projection
g.products <- bipartite.projection(g.supply.chain)$proj2
summary(g.products)
plot(g.products, vertex.label = NA, margin = 0, edge.arrow.size = 0, edge.arrow.width = 0, vertex.size = 5 )

# Descriptives
diameter(g.products) #5
get.diameter(g.products)
average.path.length(g.products) # average path length
transitivity(g.products, type = "average") # average clustering coefficient

# centrality
degree(g.products)
closeness(g.products) 
betweenness(g.products) 
evcent(g.products)$vector 


# The degree distribution --------------------------- 
ggplot() + 
  geom_histogram(aes(x = degree(g.products)), binwidth = 10) +
  xlim(0, 25) + 
  ylim(0, 25)  + 
  labs(x = "Degree", 
       y = "Count", 
       title = "Histogram of Degree distribution") +
  theme(plot.title = element_text(hjust = 0.5))


# Products purchased together  2015--------------------------- 
g.supply <- dt.supply.chain %>%
  filter(date == "1/1/2015" ) %>%
  select(customer_id, order_id) %>%
  graph_from_data_frame(directed = T)
  
summary(g.supply)

sg <- delete.vertices(g.supply, degree(sg) == 0 )
plot(sg, vertex.label = NA, margin = 0, edge.arrow.size = 0, edge.arrow.width = 0, vertex.size = 5 )

# Because we are interested in understanding how items are purchased together 
# and whether or not they are reciprocally purchased, dyad and triad censuses 
# can provide a useful initial look.
# A dyad census will tell us how many items are purchased reciprocally vs. asymmetrically. 
# The triad census will tell us which items might be important

# Perform dyad census
dyad_census(g.supply) 
# 0 mutual connections, meaning no items were bought together.

# Perform triad census
triad_census(g.supply)

# Find the edge density
edge_density(g.supply)

# Products purchased together  2018---------------------------
g.supply <- dt.supply.chain %>%
  filter(date == "1/13/2018" ) %>%
  select(customer_id, order_id) %>%
  graph_from_data_frame(directed = T)

summary(g.supply)
# 0 items bought together

sg <- delete.vertices(g.supply, degree(sg) == 0 )
plot(sg, vertex.label = NA, margin = 0, edge.arrow.size = 0, edge.arrow.width = 0, vertex.size = 5 )
# again no products purchased together

# Products purchased together  all time --------------------------
g.supply <- dt.supply.chain %>%
  select(customer_id, order_id) %>%
  graph_from_data_frame(directed = T)

summary(g.supply)
sg <- induced.subgraph(g.supply, 1:500)
sg <- delete.vertices(g.supply, degree(sg) == 0 )
plot(sg, vertex.label = NA, margin = 0, edge.arrow.size = 0, edge.arrow.width = 0, vertex.size = 5 )

# Perform dyad census
dyad_census(g.supply)

# 2 mutual connections, meaning only 2 times items were bought together.
