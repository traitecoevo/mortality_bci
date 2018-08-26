# Custom ggplot2 theme function
#' 
# Custom ggplot2 theme function
#' @param legend.position The position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param strips Logical. Whether to include strip titles or not
#' @return ggplot2 theme
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) ggplot2::element_rect(fill='lightgrey') else ggplot2::element_blank()
  st <- if(strips==TRUE) ggplot2::element_text() else ggplot2::element_blank()
  ggplot2::theme_classic(base_size = 7) + 
    ggplot2::theme(strip.text = st,
                   strip.background = sb,
                   legend.position = legend.position,
                   axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                   axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                   plot.margin = ggplot2::unit(c(3,3,3,3), "mm"))
}