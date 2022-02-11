#' Construct and evaluate a simplex density as a grid-based color map
#'
#' @param density_function 
#' @param args 
#'
#' @return A matrix of color codes
#' @export
construct_density_matrix <- function(density_function, args){
  B = as.matrix(data.frame(x = c(0, 1, 0.5), y=c(0, 0, sqrt(3)/2)))
  
  resolution = 101
  
  n_colors = 1000
  viridis_palette = viridis::viridis(n=n_colors)
  
  # Nyquist Sampling Theorem -> sample at >2*freq_max
  pi_grid = construct_simplex_grid(n_x = 2.01*resolution, n_y = 2.01*resolution)
  args = append(args, list(x=pi_grid), after=0)
  pi_grid_density = do.call(density_function, args)
  pi_grid_density = pi_grid_density^(1/4)
  
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
  
  colors = viridis_palette[color_indices]
  
  M = matrix(data = NA, nrow = resolution, ncol=resolution)
  
  pi_grid_M_idx = ceiling(pi_grid %*% B %*% diag(c(resolution, resolution/(sqrt(3)/2))))
  M[pi_grid_M_idx] = colors
  
  # Problem: matrix index is M[y, x] 
  # TODO: Make construction more elegant?
  M = t(M)[ncol(M):1,] 
  return(M)
}