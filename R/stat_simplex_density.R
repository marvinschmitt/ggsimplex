# main function ---------------

#' Draw a simplex density heatmap from distributional parameters
#'
#' @param mapping 
#' @param data 
#' @param geom 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#' @param fun 
#' @param args 
#' @param ... 
#'
#' @return
#' @export
stat_simplex_density <- function(mapping = NULL, data = NULL, 
                                 geom = "simplex_heatmap",
                                 position = "identity", na.rm = FALSE, show.legend = NA, 
                                 inherit.aes = FALSE, fun = NULL, 
                                 args = NULL, 
                                 ...) {
  mapping = ggplot2::aes_all(args)
  
  if (is.null(fun)) warning("PLEASE PROVIDE A FUNCTION in fun!")
  ggplot2::layer(
    stat = StatSimplexDensity, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(fun = fun, args = args, na.rm = na.rm,...)
  )
}

# ggproto ----------
StatSimplexDensity <- ggplot2::ggproto(
  "StatSimplexDensity", ggplot2::Stat, 
  compute_group = function(data, scales, fun=NULL, args=NULL) {
    density_args = lapply(args, function(arg) data[, as.character(arg)][[1]])
    
    # TODO: Create a lookup for common name conflicts
    names(density_args)[names(density_args) == "Alpha"] = "alpha"
    
    M = construct_density_matrix(fun, density_args)
    d = data.frame(0)
    d$M = list(M)
    return(d)
  }
)