# Plot correlations between traits
#' 
# Plot correlations between traits
#' @param data Dataframe
#' @return Figure
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

plot_trait_cors <- function(data) {
  x <- data %>% 
    dplyr::select(rho,gap_index,dbh_95) %>% 
    dplyr::distinct()
  
  trait_label_lookup <- c(gap_index = "Light~demand", rho ="Wood~density~(g~cm^-3)", dbh_95 = "Max~dbh~(cm)")
  
  print(GGally::ggpairs(x,labeller = ggplot2::as_labeller(trait_label_lookup, 
                                                    default = ggplot2::label_parsed)) + 
    plot_theme(strips=TRUE) + 
    ggplot2::theme(panel.border = ggplot2::element_rect(fill=NA)))
}