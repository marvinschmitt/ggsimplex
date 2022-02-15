# main function --------------

#' Draw a simplex raster from a matrix of color values
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value.
#'
#' @export
geom_simplex_raster <- function(
  mapping = NULL, data = 0, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = FALSE, ...){
  ggplot2::layer(
    geom = GeomSimplexRaster, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, ...)
  )
}


# ggproto ----------------
GeomSimplexRaster <- ggplot2::ggproto(
  "GeomSimplexRaster", ggplot2::Geom,
  required_aes = c("M"),
  default_aes = ggplot2::aes(alpha = 1.0),
  draw_key = ggplot2::draw_key_blank,
  
  draw_panel = function(data, panel_params, coord){
    M = data$M[[1]]
    alpha_hex = format(
      as.hexmode(floor(data$alpha * 255)),
      upper.case = TRUE, width=2)
    M = sub('FF$', alpha_hex, M)
    
    B = data.frame(x = c(0, 1, 0.5), y=c(0, 0, sqrt(3)/2))
    
    B_ggcoords = coord$transform(B, panel_params)
    ggcoords = list(
      width = B_ggcoords[2, "x"] - B_ggcoords[1, "x"],
      height = B_ggcoords[3, "y"] - B_ggcoords[1, "y"],
      origin_x = B_ggcoords[1, "x"],
      origin_y = B_ggcoords[1, "y"]
    )
    
    grob = grid::rasterGrob(M, interpolate=TRUE, 
                            width=ggcoords["width"], height=ggcoords["height"],
                            x = ggcoords["origin_x"], y = ggcoords["origin_y"],
                            hjust=0, vjust=0)
    
    return(grob)
  }
)
