#' Initial layout creation for the TreeAndLeaf
#'
#' Returns a matrix of positions to be relaxed by the force based algorithm
#' implemented in the RedeR package
#'
#' @param gg an hclust or igraph object, containing the dendrogram to
#' be reorganized
#' @param size a string that describes the size of the dendrogram. "small" = less
#' than 100 nodes; "medium" = less than 500 nodes; "large" = 500+ nodes.
#' This is just a reference point, use the size that gives the best results.
#'
#' @return A matrix of the nodes positions
#'
#' @seealso \code{\link[RedeR:addGraph]{addGraph}}
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' layout <- treeAndLeaf(hc)
#'
#' @export


treeAndLeaf <- function(gg, size = "small"){
  if(class(gg)=="hclust"){
    gg <- hclust2igraph(gg)
    gg <- gg$g
    print("hey")
  }

  #-- Find root and get number of leafs
  edgelist <- get.edgelist(gg)
  numLeafs <- length(V(gg))
  root <- .findRoot(edgelist)

  #-- Start layout
  layout <- matrix(0, nrow = vcount(gg), ncol = 2,
                   dimnames = list(V(gg)$name, c("x","y")))

  #-- Calculate the edges lengths for root
  elL <- .findSubTreeSizeLeft(root, edgelist)^2
  elR <- .findSubTreeSizeRight(root, edgelist)^2

  #-- Find the root's children from edgelist
  children <- edgelist[which(edgelist[,1] %in% root),2]

  #-- Set the layout
  layout[children[1],] <- c(elR, 0)
  layout[children[2],] <- c(-elL, 0)

  #-- Recursively set the layout for the rest of the binary tree
  layout <- .setLayout(children[1], edgelist, numLeafs, layout, size = size)
  layout <- .setLayout(children[2], edgelist, numLeafs, layout, size = size)

  return(layout)
}

.setLayout <- function(node, edgelist, numLeafs, layout, count = 1, size = "small"){
  if(node %in% edgelist[,1]){
    #-- Counter to alternate between directions (should find a better way)
    count <- count + 1

    #-- Find children
    children <- edgelist[which(edgelist[,1] %in% node),2]

    #-- Calculate edges
    if(size == "small"){
      elL <- (.findSubTreeSizeLeft(node, edgelist)^2)
      elR <- (.findSubTreeSizeRight(node, edgelist)^2)
    }
    if(size == "medium"){
      elL <- (.findSubTreeSizeLeft(node, edgelist)^2 + .countChildren(node, edgelist)*0.5)
      elR <- (.findSubTreeSizeRight(node, edgelist)^2 + .countChildren(node, edgelist)*0.5)
    }
    if(size == "large"){
      elL <- (.findSubTreeSizeLeft(node, edgelist)^2 + .countChildren(node, edgelist))
      elR <- (.findSubTreeSizeRight(node, edgelist)^2 + .countChildren(node, edgelist))
    }

    #-- Alternates between up/down and left/right
    if(count %% 2 == 0){
      coord.child1 <- c(layout[node, 1], layout[node, 2] + elR)
      coord.child2 <- c(layout[node, 1], layout[node, 2] - elL)
    }
    else{
      coord.child1 <- c(layout[node, 1] + elR, layout[node, 2])
      coord.child2 <- c(layout[node, 1] - elL, layout[node, 2])
    }

    #-- Set the layout
    layout[children[1],] <- coord.child1
    layout[children[2],] <- coord.child2

    #-- Recursive call
    layout <- .setLayout(children[1], edgelist, numLeafs, layout, count, size)
    layout <- .setLayout(children[2], edgelist, numLeafs, layout, count, size)
  }
  else{
    return(layout)
  }
}

.findSubTreeSizeLeft <- function(node, edgelist, length = 0){
  children <- edgelist[which(edgelist[,1] %in% node),2]
  if(node %in% edgelist[,1]){
    length <- length + 1
    length <- .findSubTreeSize(children[2], edgelist, length)
  }
  return(length)
}

.findSubTreeSizeRight <- function(node, edgelist, length = 0){
  children <- edgelist[which(edgelist[,1] %in% node),2]
  if(node %in% edgelist[,1]){
    length <- length + 1
    length <- .findSubTreeSize(children[1], edgelist, length)
  }
  return(length)
}

.findSubTreeSize <- function(node, edgelist, length = 0){
  children <- edgelist[which(edgelist[,1] %in% node),2]
  if(node %in% edgelist[,1]){
    length <- length + 1
    left <- .findSubTreeSize(children[1], edgelist, length)
    right <- .findSubTreeSize(children[2], edgelist, length)
    if(left > length || right > length){
      if(left > right){
        return(left)
      }
      else{
        return(right)
      }
    }
    return(length)
  }
  return(length)
}

.findRoot <- function(edgelist){
  return(unique(edgelist[which( is.na(match(edgelist[,1], edgelist[,2])) == TRUE)]))
}

.countChildren <- function(node, edgelist, count = -1){
  children <- edgelist[which(edgelist[,1] %in% node),2]
  if(node %in% edgelist[,1]){
    count <- .countChildren(children[1], edgelist, count)
    count <- .countChildren(children[2], edgelist, count)
  }
  return(count+1)
}
