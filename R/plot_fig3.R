# Plot figure 3 of manuscript
#' 
# Plot figure 3 of manuscript
#' @param spp_params Dataframe obtained from summarise_spp_params()
#' @param mu_tend R object obtained from  param_by_trait_mu_trends()
#' @return Figure 3 of manuscript
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

plot_fig3 <- function(spp_params, mu_trend) {
  
  spp_params <- spp_params %>%
    mutate(trait_value2 = ifelse(trait=="dbh_95", log10(trait_value),trait_value))
  
  mu_trend <- mu_trend %>%
    mutate(trait_value2 = ifelse(trait=="dbh_95", log10(trait_value),trait_value))
  
  breaks <- c(0.001,0.01,0.1, 1, 10)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  trait_label_lookup <- c(wood_density ="Wood~density~(g~cm^-3)", gap_index = "Light~demand", dbh_95 = "log10(Max~dbh~(cm))")
  param_label_lookup <- c(alpha = "Low~growth~effect~(alpha)", beta = "Growth~decay~rate~(beta)", gamma ="Baseline~hazard~(gamma)")
  
  ggplot2::ggplot(spp_params, ggplot2::aes(x = trait_value2,y = mean)) + 
    ggplot2::geom_ribbon(data = mu_trend, ggplot2::aes(x = trait_value2,ymin = `2.5%`,ymax = `97.5%`), alpha=0.5, colour=NA) +
    ggplot2::facet_grid(param~trait, scales = "free",
                        labeller = labeller(param = ggplot2::as_labeller(param_label_lookup, default = ggplot2::label_parsed), 
                                            trait = ggplot2::as_labeller(trait_label_lookup, default = ggplot2::label_parsed))) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = `2.5%`, ymax=`97.5%`), size=0.25, shape= 16) +
    ggplot2::geom_point(shape= 21, fill='red') +
    ggplot2::geom_line(data = mu_trend, ggplot2::aes(x = trait_value2, y= mean), col='blue') +
    ggplot2::scale_y_log10(breaks= breaks, labels = labels) +
    ggplot2::ylab(expression("Hazard rate"~("yr"^-1))) +
    ggplot2::xlab("Trait value") +
    plot_theme(strips = TRUE) +
    theme(panel.border = ggplot2::element_rect(color="grey", size=0.5, linetype="solid", fill=NA),
          panel.spacing = ggplot2::unit(.5, 'pt'))
}
