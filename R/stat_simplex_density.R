# main function ---------------

#' Draw a simplex density heatmap from distributional parameters
#'
#' @inheritParams ggplot2::stat_identity
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value.
#' @param fun density function to evaluate at each grid point
#' @param args arguments to the function `fun`
#' @param col_scale specify a color scale mapping. 
#' Possible values include: "linear", "sqrt", "log", any numeric n for n'th root, 
#' NULL (defaults to linear)
#' @export
stat_simplex_density <- function(mapping = NULL, data = NULL, 
                                 geom = "simplex_heatmap",
                                 position = "identity", na.rm = FALSE, show.legend = NA, 
                                 inherit.aes = FALSE, fun = NULL, 
                                 args = NULL, col_scale = "linear",
                                 ...) {
  mapping = ggplot2::aes_all(args)
  
  if (is.null(fun)) warning("PLEASE PROVIDE A FUNCTION in fun!")
  ggplot2::layer(
    stat = StatSimplexDensity, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    check.aes=FALSE,
    params = list(fun = fun, args = args, col_scale = col_scale,
                  na.rm = na.rm,...)
  )
}

# ggproto ----------
StatSimplexDensity <- ggplot2::ggproto(
  "StatSimplexDensity", ggplot2::Stat, 
  compute_group = function(data, scales, 
                           fun=NULL, args=NULL, col_scale="linear") {
    density_args = lapply(args, function(arg) data[, as.character(arg)][[1]])
    # TODO: Create a lookup for common name conflicts
    names(density_args)[names(density_args) == "Alpha"] = "alpha"
    
    M = construct_density_matrix(density_function = fun, 
                                 args = density_args, 
                                 col_scale = col_scale)
    d = data.frame(0)
    d$M = list(M)
    return(d)
  }
)