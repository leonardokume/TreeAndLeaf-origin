#' Extracts the layout from an igraph object
#'
#' Function for extracting the layout from an igraph object for posterior usage
#'
#' @param g an igraph object
#'
#' @return A matrix of positions
#'
#' @seealso \code{\link[RedeR:addGraph]{addGraph}},
#' \code{\link[igraph]{igraph}}
#'
#' @examples
#' layout <- getLayout(g)
#'
#' @export
getLayout <- function(g){
  l <- igraph::vertex.attributes(g)
  layout <- matrix(nrow = length(l$name), ncol = 2)
  colnames(layout) <- c("x", "y")
  rownames(layout) <- l$name
  layout[, 1] <- l$coordX
  layout[, 2] <- l$coordY
  return(layout)
}
