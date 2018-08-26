# Plot Figure S1 for manuscript
#' 
# Plot Figure S1 for manuscript
#' @param data Dataframe containing observed dbh and predicted dbh - measurement error
#' @return Figure S1 of manuscript
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
plot_figS1 <- function(data) {
  # Observed DBH @ t1 vs Prediction
  p1 <- ggplot2::ggplot(data = data,  ggplot2::aes(x = dbh_prev, y = true_dbh1)) + 
    ggplot2::geom_point(alpha = 0.4, size= 0.2) +
    ggplot2::geom_abline(intercept = 0, slope = 1,col='red', size= 0.2) +
    ggplot2::scale_x_continuous(expand=c(0,0), limit=c(NA, 160)) +
    ggplot2::scale_y_continuous(expand=c(0,0), limit=c(0, 160)) +
    ggplot2::ylab("Predicted DBH"["t1"]~("cm")) + 
    ggplot2::xlab("Observed DBH"["t1"]~("cm")) +
    plot_theme() +
    ggplot2::theme(axis.title=ggplot2::element_text(size=5.5))
  
  # Observed DBH @ t2 vs Prediction
  p2 <- ggplot2::ggplot(data = data,  ggplot2::aes(x = dbh, y = true_dbh2)) + 
    ggplot2::geom_point(alpha = 0.4,size= 0.2) +
    ggplot2::geom_abline(intercept = 0, slope = 1,col='red', size= 0.2) +
    ggplot2::scale_x_continuous(expand=c(0,0), limit=c(NA, 160)) +
    ggplot2::scale_y_continuous(expand=c(0,0), limit=c(0, 160)) +
    ggplot2::ylab("Predicted DBH"["t2"]~("cm")) + 
    ggplot2::xlab("Observed DBH"["t2"]~("cm")) +
    plot_theme() +
    ggplot2::theme(axis.title=ggplot2::element_text(size=5.5))
  
  # Observed DBH growth vs Prediction
  p3 <- ggplot2::ggplot(data = data,  ggplot2::aes(x = obs_dbh_dt, y = true_dbh_dt)) + 
    ggplot2::geom_point(alpha = 0.4, size= 0.2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, col='red', size= 0.2) +
    ggplot2::scale_x_continuous(expand=c(0,0), limit=c(NA, 5)) +
    ggplot2::scale_y_continuous(expand=c(0,0), limit=c(0, 5)) +
    ggplot2::ylab(expression("Predicted DBH growth"~("cm yr"^-1))) +
    ggplot2::xlab(expression("Observed DBH growth"~("cm yr"^-1))) +
    plot_theme() +
    ggplot2::theme(axis.title=ggplot2::element_text(size=5.5))
  
  # Observed basal area growth vs Prediction
  p4 <- ggplot2::ggplot(data = data,  ggplot2::aes(x = obs_basal_area_dt, y = true_basal_area_dt)) + 
    ggplot2::geom_point(alpha = 0.4, size= 0.2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, col='red', size= 0.2) +
    ggplot2::scale_x_continuous(expand=c(0,0), limit=c(NA, 600)) +
    ggplot2::scale_y_continuous(expand=c(0,0), limit=c(0, 600)) +
    ggplot2::ylab(expression("Predicted basal area growth"~("cm"^2~"yr"^-1))) +
    ggplot2::xlab(expression("Observed basal area growth"~("cm"^2~"yr"^-1))) +
    plot_theme() +
    ggplot2::theme(axis.title=ggplot2::element_text(size=5.5))
  
  cowplot::plot_grid(p1, p2, p3, p4, ncol=2, labels=LETTERS[1:4], label_size = 7)
}