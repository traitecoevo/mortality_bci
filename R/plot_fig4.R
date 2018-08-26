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
                        ylab=NULL) +
    theme(plot.margin=unit(c(0.35,0.2,0.5,0.5),"cm"))
  
  p2 <- plot_mu_curves(model, 
                       growth_range, 
                       trait_name = "gap_index", 
                       trait_values = c(0.5, 0.8), 
                       hazard_curve, 
                       xlab= NULL, 
                       ylab = NULL) +
    plot_theme() +
    theme(plot.margin=unit(c(0.35,0.2,0.5,0.5),"cm"))
  
  p3 <- plot_mu_curves(model, 
                       growth_range, 
                       trait_name = "wood_density", 
                       trait_values = c(0.2,0.8), 
                       hazard_curve, 
                       xlab=NULL, 
                       ylab =NULL) +
    plot_theme() + 
    theme(plot.margin=unit(c(0.35,0.2,0.5,0.5),"cm"))

  p4 <- plot_mu_curves(model,
                       growth_range, 
                       trait_name = "dbh_95", 
                       trait_values = c(3,180),
                       hazard_curve, 
                       xlab= NULL, 
                       ylab = NULL) +
        plot_theme() +
        theme(plot.margin=unit(c(0.35,0.2,0.5,0.5),"cm"))
  
  
  if(isTRUE(hazard_curve)) {
    title <- "Hazard rate"
  } else {
    title <- "Annual mortality probability"
  }
  
  cowplot::plot_grid(p1,p2,p3,p4, ncol=1, labels=c("A) Species curves",
                                                   "B) Maximum dbh",
                                                   "C) Wood density",
                                                   "D) Light demand"), 
                     label_size = 7, 
                     label_fontface ="plain") +
    draw_label(title, x= 0, y=0.5, vjust= 1.5, angle=90,size = 7) +
    draw_label(expression("Annual dbh growth"~("cm yr"^-1)), x= 0.5, y=0.03, angle=0,size = 7)
}
