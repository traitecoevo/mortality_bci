# Plot average mortality curves
#' 
# Plot average mortality curves
#' @param model List. Model object obtained from compile_models()
#' @param growth_range Numeric vector. The growth range to predict curve over
#' @param trait_name Character. Name of trait (can be one of `wood density`, `gap_index` or `dbh_95`)
#' @param trait_values Numeric vector. The trait values one wishes to examine.
#' @param hazard_curve Logical. Whether to plot hazard (TRUE) or probability (FALSE) curve
#' @param ylab Character. Y axis title
#' @param xlab Character. X axis title
#' @param legend_position The position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param legend_label Character. Name of legend title. NULL = variable name
#' @return Plot
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
plot_mu_curves <- function(model, 
                           growth_range = c(0.03,0.5), 
                           trait_name, 
                           trait_values, 
                           hazard_curve = FALSE, 
                           ylab = "1-yr mortality probability", 
                           xlab = expression("Annual dbh growth"~("cm yr"^-1)),
                           legend_position = "right",
                           legend_label = NULL) {
  
  preds <- predict_mu_hazards_curves(model, growth_range, trait_name, trait_values, hazard_curve)
  
  breaks <- c(0.0001,0.001,0.01,0.1, 1, 10, 100)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  
  p1 <- ggplot2::ggplot(preds, ggplot2::aes(x = dbh_growth,y = mean, group = type, colour = factor(trait_value), fill=factor(trait_value))) + 
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(name=legend_label, values = c("blue","red")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.5, colour=NA) +
    ggplot2::scale_fill_manual(legend_label,values = c("blue","red")) +
    ggplot2::scale_x_continuous(expand=c(0,0)) +
    ggplot2::xlab(xlab)
  
  if(hazard_curve ==FALSE) {
    p1 + ylab(ylab) + 
      scale_y_continuous(expand=c(0,0), limits=c(0,1)) + 
      plot_theme(legend.position = legend_position)
  }
  else {
    p1 + ylab(ylab) + 
      scale_y_log10(breaks= breaks, labels = labels, limits=c(0.001,100)) +
      plot_theme(legend.position = legend_position)
  }
}