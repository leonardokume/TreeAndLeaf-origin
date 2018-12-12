#' Extract the layout from an igraph object.
#'
#' @param g An igraph object.
#' @return The layout from the igraph object.
#' @examples
#' getLayout(gg)
getLayout <- function(g){
  l <- igraph::vertex.attributes(g)
  layout <- matrix(nrow = length(l$name), ncol = 2)
  colnames(layout) <- c("x", "y")
  rownames(layout) <- l$name
  layout[, 1] <- l$coordX
  layout[, 2] <- l$coordY
  return(layout)
}
