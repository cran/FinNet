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
# Create a list of the desired firms
data('firms_BKB')
# Check a weakly connected subset of the network
FF_uncnnctd <- FF(firms_BKB[20:23], who = 'own', ties = 'naive', Matrix = TRUE)
g_uncnnctd <- FF.graph(FF_uncnnctd, 'simple')
# Check a strongly connected subset of the network
FF_cnnctd <- FF(firms_BKB[5:10], who = 'own', ties = 'naive', Matrix = TRUE)
g_cnnctd <- FF.graph(FF_cnnctd, 'simple')
# Plot to compare
layout(matrix(1:2, nrow = 1));{
    plot_igraph(g_uncnnctd, asp = .75)
    text(0, 1.3, 'Weakly connected')
    plot_igraph(g_cnnctd, asp = .75)
    text(0, 1.3, 'Better connected')
}


## ----workflow_4, echo=TRUE----------------------------------------------------
cfa(FF_uncnnctd)|> knitr::kable()

## ----workflow_5, echo=TRUE----------------------------------------------------
cfa(FF_cnnctd)|> knitr::kable()

