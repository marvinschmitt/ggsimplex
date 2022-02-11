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