# main function -----------------

#' Draw points on a simplex from barycentric coordinates
#'
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value.
#' @export
geom_simplex_point <- function(mapping=NULL, data=NULL,
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, stat="identity",
                         inherit.aes = TRUE, ...){
  ggplot2::layer(
    stat = "identity", geom = GeomSimplexPoint, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = TRUE,
    params = list(na.rm = na.rm, ...)
  )
}


# ggproto -----------------

GeomSimplexPoint = ggplot2::ggproto(
  "GeomSimplexPoint", ggplot2::Geom, required_aes = c("pmp"),
  
  default_aes = ggplot2::aes(colour = "#f6546a", alpha = 0.4, 
                             shape = 19, size=0.3),
  
  draw_key = ggplot2::draw_key_point,
  
  draw_panel = function(data, panel_params, coord){
    B = data.frame(x = c(0, 1, 0.5), y=c(0, 0, sqrt(3)/2))
    PMP = matrix(unlist(data$pmp), nrow=length(data$pmp), byrow=TRUE)
    data[, c("x", "y")] = as.data.frame(
      PMP %*% as.matrix(B)
    )
    coords = coord$transform(data, panel_params)
    grob = grid::pointsGrob(
      coords$x, coords$y,
      default.units = "native",
      pch = coords$shape,
      gp = grid::gpar(
        col = scales::alpha(coords$colour, coords$alpha),
        cex = coords$size
      )
    )
    
    return(grob)
  }
)