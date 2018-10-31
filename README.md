# TreeAndLeaf
A R package for reorganizing dendrograms and adding additional layers of information

## Installation
```r
# Install devtools from github
devtools::install_github("leonardokume/TreeAndLeaf")
```

## Quick Start

```{r, eval=FALSE}
library(igraph)
library(RedeR)
library(TreeAndLeaf)

hc <- hclust(dist(USArrests), "ave")
gg <- hclust2igraph(hc)

#---set alias
idx <- match(V(gg$g)$name, rownames(USArrests))
V(gg$g)$nodeAlias <- V(gg$g)$name
V(gg$g)$nodeAlias[is.na(idx)]<-""

#-- node font
V(gg$g)$nodeFontSize<-25
V(gg$g)$nodeFontSize[V(gg$g)$nodeAlias==""]<- 1

#-- node color
V(gg$g)$nodeColor<-"black"

#-- node size
sz <- USArrests$UrbanPop
names(sz) <- rownames(USArrests)
idx <- match(names(sz), V(gg$g)$name)

V(gg$g)$nodeSize[idx] <- sz
V(gg$g)$nodeSize[V(gg$g)$nodeAlias==""] <- 1

rdp <- RedPort()
calld(rdp)
layout <- treeAndLeaf(gg$g, size = "small")
addGraph(rdp, gg$g, layout = layout, zoom = 20)
relax(rdp, p1 = 50, p2 = 100, p3 = 50, p4 = 100, p5 = 100, p8 = 40)
```

