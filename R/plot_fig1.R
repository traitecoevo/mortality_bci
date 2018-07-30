# Plot figure 1 of manuscript
#' 
# Plot figure 1 of manuscript
#' @param file_alive Character. Path to png of alive tree 
#' @param file_dead Character. Path to png of dead tree
#' @param file_crossval Character. Path to crossval png
#' @return Figure 1 of manuscript
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

plot_fig1 <- function(file_alive, file_dead, file_crossval) {
  
  # Example base only hazard function
  base_only_haz <- function(x) { 
    1-exp(-(0*x +0.03))
  }
  
  # Example growth hazard function
  growth_haz <- function(x) {
    1-exp(-0.15 * exp(-10 * x))
  }
  
  # Example base-growth hazard function
  base_growth_haz <- function(x) {
    1-exp(-(0.13 * exp(-10 * x) + 0.03))
  }
  
  # Plot hazard curves
  hazard_plot <- function(f, text){
    x <- seq(0,1,length.out = 50)
    empty_plot()
    lines(x, f(x), type='l', col="#d7301f")
    text(0.1, 0.12, text, pos=4, xpd=NA)
  }
  
  # Function to add labels to panels
  my_label <- function(text, x=-0.1) label(x, 1.3, text)
  
  # Plot layout
  layout(matrix(c(1,2,3,4,4,4,5,5,5,6,6,6), byrow=TRUE, ncol=3))
  par(mar=c(1,1,2.5,1), oma=c(1,1,1,1), cex=0.5)
  
  # Produce empty plot
  empty_plot <- function(){
    par(mar=c(2,2,3,1))
    plot(NA, ylim = c(0,0.14),  xlim=c(0,1), xaxs='i', axes=FALSE, ann=FALSE)
    axis(1, at = c(-1, 2), labels=NA)
    axis(2, at = c(-1, 2),labels=NA)
  }
  
  # Plot base hazard
  hazard_plot(base_only_haz, expression(gamma))
  my_label("A) Alternative mortality functions", x=-0.36)
  mtext("Mortality rate", 2, line=1, cex=0.5)
  
  # Plot growth-dependent hazard
  hazard_plot(growth_haz, expression(alpha*"e"^{-beta~"X"}))
  mtext("Growth rate", 1, line =1, cex=0.5)
  
  # Example full baseline + growth-dependent hazard
  hazard_plot(base_growth_haz, expression(gamma + alpha*"e"^{-beta~"X"}))
  
  # Tree growth diagram
  plot(1:2, type='n', ann=FALSE, axes=FALSE, xlim=c(-1,1), ylim=c(-1,1))
  my_label("B) Repeat census data", x=-0.10)
  
  # note we are only plotting to last viewport, but calls to other two needed to make figure work.
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  plot_tree_life(file_alive, file_dead)
  popViewport(3)
  
  # Plot logloss curve
  par(mar=c(2.5,3,2,1))
  plot(NA, xlim = c(0, 1), ylim= c(0, 5), ann = FALSE, las=1)
  p <- seq(0,1, length.out=200)
  lines(p, -log(1-p), type='l', col="#5e3c99")
  text(0.85, 3, expression(paste(S[i],"=0 (survived)")), col="#5e3c99", pos=3, xpd=NA)
  lines(p, -log(p), type='l', col="#e66101")
  text(0.15, 3, expression(paste(S[i],"=1 (died)")), col="#e66101", pos=3, xpd=NA)
  
  my_label("C) Penalty for incorrect prediction", x=-0.137)
  mtext(expression(paste("Probability of death, ", p[i])), 1, line=2.1, cex=0.5)
  mtext("log loss", 2, line=2.1, cex=0.5)
  
  # Plot cross validation procedure
  par(mar=c(0.1,0.1,3,0.1))
  plot(NA, xlim = c(0, 1), ylim= c(0, 1), ann = FALSE, axes=FALSE)
  my_label("D) 10-fold cross validation", x =-0.03)
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  img <- readPNG(file_crossval)
  grid.raster(img, unit(0.4, "npc"),  y = unit(0, "npc"),
              just=c("bottom"), height=unit(1, "npc"))
  
  popViewport(3)
  text(0.85, 0.45,  expression(paste(bar("logloss"), " =", sum(italic("L")[i])/10)), xpd=NA)
}