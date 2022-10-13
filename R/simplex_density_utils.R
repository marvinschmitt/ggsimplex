#' Construct and evaluate a simplex density as a grid-based color map
#'
#' @param density_function Density function that is evaluated at each grid point
#' @param args arguments to the density function
#' @param resolution number of grid points along axes
#' @param col_scale scale mapping, e.g. "linear" or "sqrt"
#'
#' @return A matrix of color codes
#' @export
construct_density_matrix <- function(density_function, args, 
                                     resolution = 101,
                                     col_scale="linear"){
  B = as.matrix(data.frame(x = c(0, 1, 0.5), y=c(0, 0, sqrt(3)/2)))
  
  n_colors = 1000
  color_palette = viridis::viridis(n=n_colors)
  
  # Nyquist Sampling Theorem -> sample at >2*freq_max
  if (resolution == 101){
    pi_grid = sysdata.pi_grid_101
  } else if (resolution == 201){
    pi_grid = sysdata.pi_grid_201
  } else {
    pi_grid = construct_simplex_grid(n_x = 2.01*resolution, n_y = 2.01*resolution)
  }
  
  args = append(args, list(x=pi_grid), after=0)
  pi_grid_density = do.call(density_function, args)
  if (is.null(col_scale) || col_scale=="linear"){
    pi_grid_density = pi_grid_density # do nothing
  }
  else if (col_scale == "log"){
    pi_grid_density = log(pi_grid_density)
  }
  else if (col_scale == "sqrt"){
    pi_grid_density = sqrt(pi_grid_density)
  }
  else if (is.numeric(col_scale)){
    pi_grid_density = pi_grid_density ^ (1/col_scale)
  }
  else{
    stop("Please provide a valid mapping or leave empty!")
  }
  
  cuts = seq(
    min(pi_grid_density), 
    max(pi_grid_density), 
    length.out = n_colors
  )
  color_indices = 
    unlist(
      lapply(pi_grid_density, 
                         function(z) {
                           idxs = which(z <= cuts)
                           return(idxs[1])
                           }
  )) # calculate color indices (as array)
  
  colors = color_palette[color_indices]
  
  M = matrix(data = NA, nrow = resolution, ncol=resolution)
  
  pi_grid_M_idx = ceiling(pi_grid %*% B %*% diag(c(resolution, resolution/(sqrt(3)/2))))
  M[pi_grid_M_idx] = colors
  
  # Problem: matrix index is M[y, x] 
  # TODO: Make construction more elegant?
  M = t(M)[ncol(M):1,] 
  return(M)
}