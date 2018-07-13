# Plot species random effects
#' 
# Plot species random effects. Required for Figs S3-S5
#' @param gap_index_raster List of rasters
#' @param recruit_gap_conditions List of SpatialPointsDataframes
#' @return Plots required for manuscript Figs S3-S5
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
plot_spp_params <- function(model, data, parameter,xlab ='Log effect size', title=NULL) {
  
  `%>%` <- magrittr::`%>%`
  # Extract hyper distribution summary
  hyper <- summarise_hyper_params(model,data) %>%
    dplyr::filter(param == paste0("mu_log_",parameter)) %>%
    dplyr::mutate(
      hyper_param = as.character(TRUE),
      species = as.character('Hyper distribution')) %>%
    dplyr::select(param, hyper_param, species, mean, sd,`2.5%`,`97.5%`)
  
  # Extract species param summaries
  spp <- summarise_spp_params(model,data, TRUE) %>% 
    dplyr::filter(param == paste0("log_",parameter)) %>%
    dplyr::mutate(hyper_param = as.character(FALSE),
                  sp = as.character(sp),
                  species = as.character(species)) %>%
    dplyr::select(param, hyper_param, species, mean, sd,`2.5%`,`97.5%`) %>%
    dplyr::distinct()
  
  # Merge dataframes and prep for plotting
  dat <-dplyr::bind_rows(hyper, spp) %>%
    dplyr::mutate(species = as.factor(species)) %>%
    dplyr::mutate(species = factor(species, levels=species[order(hyper_param,mean)], ordered=TRUE))
  
  #Plot 
  ggplot2::ggplot(dat, ggplot2::aes(x = mean,y = species, col=hyper_param)) + 
    ggplot2::geom_segment(ggplot2::aes(x=`2.5%`,y=species, xend=`97.5%`, yend=species), size=0.3) +
    ggplot2::geom_point(ggplot2::aes(x=mean, y=species), size=0.3) +
    ggplot2::scale_colour_manual(values = c("TRUE" ="red","FALSE" ="black")) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(NULL) +
    ggplot2::labs(title=title) +
    plot_theme() +
    ggplot2::theme(axis.text.y=ggplot2::element_text(size=3),
                   axis.ticks = ggplot2::element_line(size= 0.3))
}
