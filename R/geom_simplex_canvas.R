# main function ------------------------------------

#' Draw Simplex Canvas
#'
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value.
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
  default_aes = ggplot2::aes(linewidth=1, linetype=1, 
                             colour="black", alpha = 1,
                             fontsize = 12),
  draw_key = ggplot2::draw_key_polygon,
  draw_panel = function(data, panel_params, coord){
    style_params = data
    style_params = coord$transform(style_params, panel_params)
    B = matrix(
      c(0, 0, 
        1, 0,
        0.5, sqrt(3)/2), byrow=TRUE, ncol=2
    )
    data = data.frame(x = B[, 1], y=B[, 2])
    coords = coord$transform(data, panel_params)
    
    grob = grid::polygonGrob(
      coords$x, coords$y,
      default.units="native",
      gp = grid::gpar(
        col = scales::alpha(style_params$colour, style_params$alpha),
        fill = scales::alpha("black", 0.0), # fully transparent fill
        lwd = style_params$linewidth * .pt,
        lty = style_params$linetype
      )
    )
    
    text_style = grid::gpar(
      col = "black", 
      fontsize=style_params$fontsize
      )
    text_grob_1 = grid::textGrob(expression(M[1]),
                    x = coords$x[1] - 0.021,
                    y = coords$y[1],
                    #just = c("left", "center"),
                    gp = text_style
                    )
    text_grob_2 =grid::textGrob(expression(M[2]),
                    x = coords$x[2] + 0.021,
                    y = coords$y[2],
                    #just = c("right", "center"),
                    gp = text_style
                    )
    text_grob_3 = grid::textGrob(expression(M[3]),
                    x = coords$x[3],
                    y = coords$y[3] + 0.021,
                    #just = c("center", "top"),
                    gp = text_style
                    )
    
    return(grid::grobTree(grob, text_grob_1, text_grob_2, text_grob_3))
  }
)
