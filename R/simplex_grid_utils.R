#' Construct a simplex grid of specified resolution
#'
#' @param n_x 
#' @param n_y 
#'
#' @return
#' @export
construct_simplex_grid <- function(n_x, n_y){
  epsilon = 0.001
  lambda_1 = seq(epsilon, 1-epsilon, length.out=n_x)
  lambda_3 = seq(epsilon, 1-epsilon, length.out=n_y)
  lambda_2 = 1 - lambda_3 - lambda_1
  simplex_grid = as.matrix(data.table::CJ(lambda_1, lambda_2, lambda_3))
  simplex_grid = simplex_grid[simplex_grid[, 2] > 0, ]
  simplex_grid = simplex_grid[abs(rowSums(simplex_grid)-1) < .Machine$double.eps ^ 0.5, ]
}

