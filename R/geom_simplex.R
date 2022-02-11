# main function -----------------

#' Draw points on a simplex from barycentric coordinates
#'
#' @param mapping 
#' @param data 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param stat 
#' @param inherit.aes 
#' @param ... 
#'
#' @return
#' @export
geom_simplex <- function(mapping=NULL, data=NULL,
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, stat="identity",
                         inherit.aes = TRUE, ...){
  ggplot2::layer(
    stat = "identity", geom = GeomSimplexScatter, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = TRUE,
    params = list(na.rm = na.rm, ...)
  )
}


# ggproto -----------------

GeomSimplexScatter = ggplot2::ggproto(
  "GeomSimplexScatter", ggplot2::Geom, required_aes = c("pmp"),
  
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