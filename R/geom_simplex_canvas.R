# main function ------------------------------------

#' Draw Simplex Canvas
#'
#' @param mapping 
#' @param data 
#' @param stat 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#' @param ... 
#'
#' @return
#' @export
geom_simplex_canvas <- function(
  mapping = NULL, data = data.frame(0), stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA, 
  inherit.aes = FALSE, ...){
  ggplot2::layer(
    geom = GeomSimplexCanvas, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(...)
  )  
}



# ggproto part -------------------------------------

GeomSimplexCanvas = ggplot2::ggproto(
  "GeomSimplexCanvas", ggplot2::GeomPolygon, required_aes = c(),
  default_aes = ggplot2::aes(),
  draw_key = ggplot2::draw_key_polygon,
  draw_panel = function(data, panel_params, coord){
    data = data.frame(x = c(0, 1, 0.5), y=c(0, 0, sqrt(3)/2))
    coords = coord$transform(data, panel_params)
    
    grob = grid::polygonGrob(
      coords$x, coords$y,
      default.units="native",
      gp = grid::gpar(
        col = "black",
        fill = scales::alpha("white", 0.0),
        lwd = 1 * .pt,
        lty = 1
      )
    )
    return(grob)
  }
)
