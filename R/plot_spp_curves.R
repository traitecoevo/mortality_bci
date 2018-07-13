# Plot all species mortality curves
#' 
# Plot all species mortality curves
#' @param model List. Model object obtained from compile_models()
#' @param data R object. Data used in model
#' @param growth_range Numeric vector. The growth range to predict curve over
#' @param trait_name Character. Name of trait to use as colour gradient
#' @param hazard_curve Logical. Whether to plot hazard (TRUE) or probability (FALSE) curve
#' @param ylab Character. Y axis title
#' @param xlab Character. X axis title
#' @param legend_position The position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param legend_label Character. Name of legend title. NULL = variable name
#' @return Plot
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

plot_spp_curves <- function(model, 
                            data, 
                            growth_range = c(0.03,0.5), 
                            trait_name, 
                            hazard_curve = FALSE, 
                            ylab= "Trait value", 
                            xlab= expression("Annual dbh growth"~("cm yr"^-1)), 
                            legend_position = "right",
                            legend_label = NULL) {
  
  `%>%` <- magrittr::`%>%`
  
  preds <- predict_spp_hazard_curves(model, data, growth_range) %>%
    filter(trait == trait_name)
  
  base::switch(trait_name,
               "wood_density" = {
                 legend_limit = c(floor(min(preds$trait_value)/0.1)*0.1,ceiling(max(preds$trait_value)/0.1)*0.1)
                 legend_breaks <- pretty(legend_limit, n = 5,min.n = 5)
               },
               "dbh_95" = {
                 legend_limit = c(1,ceiling(max(preds$trait_value)/100)*100)
                 legend_breaks <- pretty(legend_limit, n = 5,min.n = 5)
               },
               "gap_index" = {
                 legend_limit = c(floor(min(preds$trait_value)/0.1)*0.1,ceiling(max(preds$trait_value)/0.1)*0.1)
                 legend_breaks <- pretty(legend_limit, n = 5,min.n = 5)
               })
  
  breaks <- c(0.0001,0.001,0.01,0.1, 1, 10, 100)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  
  if(hazard_curve ==FALSE) {
    ggplot2::ggplot(preds, ggplot2::aes(x = dbh_growth,y = annual_prob_mort, group = sp, colour = trait_value)) + 
      ggplot2::geom_line(alpha=0.5) +
      ggplot2::scale_x_continuous(expand=c(0,0)) +
      ggplot2::scale_y_continuous(expand=c(0,0), limits = c(0, 1)) +
      ggplot2::facet_wrap(~trait) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab) +
      viridis::scale_colour_viridis(legend_label, limit = legend_limit, breaks = legend_breaks) +
      plot_theme(legend.position = legend_position)
  }
  else {
    ggplot2::ggplot(preds, ggplot2::aes(x = dbh_growth,y = inst_hazard, group = sp, colour = trait_value)) + 
      ggplot2::geom_line(alpha=0.5) +
      ggplot2::scale_x_continuous(expand=c(0,0)) +
      ggplot2::scale_y_log10(breaks= breaks, labels = labels, limits=c(0.001,100)) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab) +
      viridis::scale_colour_viridis(legend_label, limit = legend_limit, breaks = legend_breaks) +
      plot_theme(legend.position = legend_position)
  }
}