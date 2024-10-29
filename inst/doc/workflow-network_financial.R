## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(
  rmarkdown.html_vignette.check_title = FALSE
)

library(FinNet)
library(markdown)

## ----workflow_1, echo=TRUE----------------------------------------------------
# Check if `yahoofinancer` is installed
isTRUE(requireNamespace('yahoofinancer', quietly = TRUE))

# Create a list of the desired firms
data('firms_US')

## ----clean, echo=FALSE, include=FALSE-----------------------------------------
firms <- firms_US
rm(firms_US)

## ----workflow_3, echo=TRUE----------------------------------------------------
# Identify common-ownership relations in a firm-firm matrix
FF <- FF.norm.ownership(firms)

## ----workflow_4, echo=TRUE----------------------------------------------------
# Create a simple-looking graph
net <- FF.net(FF, aesthetic = 'simple')

## ----checks, echo=TRUE--------------------------------------------------------
# The order of the graph equals the number of rows in the FF matrix
network.size(net) == nrow(FF)

# The names of its vertex match the row names of the FF matrix
vertex.names(net) == rownames(FF)

# Plot it
plot_network(net, displaylabels = TRUE, vertex.cex = 3, edge.lwd = 6)

## ----workflow_5, echo=TRUE----------------------------------------------------
# Load dataset
data('firms_BKB')

# Identify common-ownership relations in a firm-firm matrix
FF <- FF(firms_BKB, who = 'own',
         ties = 'naive', Matrix = TRUE)

# Create a nice-looking graph
net <- FF.net(FF, aesthetic = 'nice')

# Plot it
plot_network(net, displaylabels = FALSE, edge.col = 'darkgray',
             vertex.cex = 3, edge.lwd = 1)

