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
  
  p1 <- plot_spp_curves(model, data, growth_range, trait_name = "wood_density", hazard_curve, xlab= NULL, ylab=expression("1-yr mortality probability"), legend_label = expression(atop("Wood density",(g~cm^-3))))
  p2 <- plot_mu_curves(model, growth_range, trait_name = "wood_density", trait_values = c(0.2,0.8), hazard_curve, xlab=NULL, ylab = NULL, legend_label = expression(atop("Wood density",(g~cm^-3))))
  p3 <- plot_spp_curves(model, data, growth_range, trait_name = "dbh_95", hazard_curve, xlab= NULL, ylab=expression("1-yr mortality probability"), legend_label = expression(atop("Max dbh","(cm)")))
  p4 <- plot_mu_curves(model, growth_range, trait_name = "dbh_95", trait_values = c(3,180),hazard_curve, xlab=NULL, ylab = NULL, legend_label = expression(atop("Max dbh","(cm)")))
  p5 <- plot_spp_curves(model, data, growth_range, trait_name = "gap_index", hazard_curve, xlab= expression("Annual dbh growth"~("cm yr"^-1)), ylab=expression("1-yr mortality probability"), legend_label = expression(atop("Shade","intolerance")))
  p6 <- plot_mu_curves(model, growth_range, trait_name = "gap_index", trait_values = c(0.5, 0.8), hazard_curve, xlab=expression("Annual dbh growth"~("cm yr"^-1)), ylab = NULL, legend_label = expression(atop("Shade","intolerance"))) 
  
  cowplot::plot_grid(p1,p2,p3,p4,p5,p6, ncol=2, labels=LETTERS[1:6], label_size = 7)
}