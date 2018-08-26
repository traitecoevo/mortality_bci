# Plot Figure S8 for manuscript
#' 
# Plot Figure S8 for manuscript
#' @param data Dataframe
#' @return Figure S8 of manuscript
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

plot_figS8 <- function(data) {
  p1 <- ggplot2::ggplot(data) + 
    geom_point(aes(x = true_dbh2, y = true_dbh_dt),alpha = 0.4) +
    ylab(expression("DBH growth"~("cm yr"^-1))) +
    xlab("Diameter (cm)")+
    ggplot2::theme(axis.title=ggplot2::element_text(size=7),
                   axis.text = ggplot2::element_text(size=5.5))
  
  p2 <- ggplot2::ggplot(data) + 
    geom_point(aes(x = true_dbh2, y = true_basal_area_dt), alpha =0.4) +
    ylab(expression("Basal area growth"~("cm"^2~"yr"^-1))) +
    xlab("Diameter (cm)") +
    ggplot2::theme(axis.title=ggplot2::element_text(size=7),
                   axis.text = ggplot2::element_text(size=5.5))
  
  cowplot::plot_grid(p1,p2,labels=LETTERS[1:2], label_size = 7, ncol=1)  
}