#' Calculate the dot product of two vectors.
#' @return The dot product (scalar) of \code{u} and \code{v}
#' @param u A numeric vector
#' @param v A numeric vector
#' @export
#'
#' @examples
#' u = c(1, 10, 100)
#' v = c(-1, 0, 1)
#' dot(u, v)
dot <- function(u, v){
  return(sum(u*v))
}



#' Helper function to bind columns into a single list column
#'
#' @param ... 
#'
#' @return
#' @export
make_list_column <- function(...){
  return(mapply(list, ..., SIMPLIFY=FALSE))
}
  
