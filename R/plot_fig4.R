# Plot Figure 4 for manuscript
#' 
# Plot Figure 4 for manuscript
#' @param model List. Model object obtained from compile_models()
#' @param data R object. Data used in model
#' @param growth_range Numeric vector. The growth range to predict curve over
#' @param hazard_curve Logical. Whether to plot hazard (TRUE) or probability (FALSE) curve
#' @return Figure 4 of manuscript
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

plot_fig4 <- function(model, 
                      data, 
                      growth_range = c(0.03,0.5),
                      hazard_curve = FALSE) {
  
  p1 <- plot_spp_curves(model, 
                        data, 
                        growth_range, 
                        trait_name = "wood_density", 
                        hazard_curve, 
                        colour_by_trait = FALSE, 
                        xlab= NULL, 
                        ylab=("1-yr mortality probability"))
  
  p2 <- plot_mu_curves(model, 
                       growth_range, 
                       trait_name = "wood_density", 
                       trait_values = c(0.2,0.8), 
                       hazard_curve, 
                       xlab=NULL, 
                       ylab = ("1-yr mortality probability"), 
                       legend_position = c(0.75,0.7),
                       legend_label = expression("Wood density"~(g~cm^-3))) +
          ggplot2::theme(legend.background = ggplot2::element_rect(fill = "transparent"),
                     legend.key = ggplot2::element_rect(fill = "transparent", 
                                                        color = "transparent"),
                     legend.title = ggplot2::element_text(size=5),
                     legend.text = ggplot2::element_text(size=5),
                     legend.key.size = unit(0.5, "cm"),
                     legend.title.align=0.5)

  p3 <- plot_mu_curves(model,
                       growth_range, 
                       trait_name = "dbh_95", 
                       trait_values = c(3,180),
                       hazard_curve, 
                       xlab=NULL, 
                       ylab = ("1-yr mortality probability"), 
                       legend_position = c(0.75,0.7),                       
                       legend_label = expression("Maximum dbh (cm)")) +
          ggplot2::theme(legend.background = ggplot2::element_rect(fill = "transparent"),
                     legend.key = ggplot2::element_rect(fill = "transparent", 
                                                        color = "transparent"),
                     legend.title = ggplot2::element_text(size=5),
                     legend.text = ggplot2::element_text(size=5),
                     legend.key.size = unit(0.5, "cm"),
                     legend.title.align=0.5)
  
  p4 <- plot_mu_curves(model, 
                       growth_range, 
                       trait_name = "gap_index", 
                       trait_values = c(0.5, 0.8), 
                       hazard_curve, 
                       xlab=expression("Annual dbh growth"~("cm yr"^-1)), 
                       ylab = ("1-yr mortality probability"), 
                       legend_position = c(0.75,0.8),
                       legend_label = expression("Light demand"))  +
          ggplot2::theme(legend.background = ggplot2::element_rect(fill = "transparent"),
                     legend.key = ggplot2::element_rect(fill = "transparent", 
                                                        color = "transparent"),
                     legend.title = ggplot2::element_text(size=5),
                     legend.text = ggplot2::element_text(size=5),
                     legend.key.size = unit(0.5, "cm"),
                     legend.title.align =2)
  
  cowplot::plot_grid(p1,p2,p3,p4, ncol=1, labels=LETTERS[1:4], label_size = 7)
}