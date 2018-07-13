# Plot example life of a tree across censuses
#' 
# Plot example life of a tree across censuses
#' @param file_alive Character. Path to png of alive tree 
#' @param file_dead Character. Path to png of dead tree
#' @return plot
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
plot_tree_life <- function(file_alive, file_dead) {
  
  gp0 <- gpar(cex=0.8)
  gp2 <- gpar(lwd=2)
  
  img_alive <- readPNG(file_alive)
  img_dead <- readPNG(file_dead)
  
  grid.lines(x = c(0.1, 0.95), y = c(0.1, 0.1),
             arrow = arrow(ends = "last", length=unit(0.1, "inches")),
             gp=gpar(lwd=2))
  
  y0 <- 0.2
  x0 <- 0.2
  grid.raster(img_alive, unit(x0, "npc"),  y = unit(y0, "npc"),
              just=c("bottom"), height=unit(0.60, "npc"))
  grid.draw(ellipseGrob(x0 - 0.01, y0+0.1, size=1,ar=3,angle=0, def="npc"))
  grid.text(expression(paste(D[1])), x = x0 + 0.03 , y = y0+0.1, just="left", gp=gp0)
  grid.lines(x = c(x0, x0), y = c(0.05, 0.1), gp=gp2)
  grid.text(expression(paste(t[1])), x = x0, y = 0, just="top", gp=gp0)
  
  x0 <- 0.5
  grid.raster(img_alive, unit(x0, "npc"),  y = unit(y0, "npc"),
              just=c("bottom"), height=unit(0.80, "npc"))
  grid.draw(ellipseGrob(x0 - 0.015, y0+ 0.1, size=1,ar=3,angle=0, def="npc"))
  grid.text(expression(paste(D[2])), x = x0 + 0.03 , y = y0+0.1, just="left", gp=gp0)
  grid.lines(x = c(x0, x0), y = c(0.05, 0.1), gp=gp2)
  grid.text(expression(paste(t[2])), x = x0, y = 0, just="top", gp=gp0)
  
  x0 <- 0.8
  grid.raster(img_dead, unit(x0, "npc"),  y = unit(y0, "npc"),
              just=c("bottom"), height=unit(0.80, "npc"))
  grid.text(expression(paste(S[3])),
            x = x0 + 0.03 , y = y0+0.1, just="left", gp=gp0)
  grid.lines(x = c(x0, x0), y = c(0.05, 0.1), gp=gp2)
  grid.text(expression(paste(t[3])), x = x0, y = 0, just="top", gp=gp0)
}

# position label at a fractional x/y position on a plot
label <- function(px, py, lab, ..., adj = c(0, 1)) {
  usr <- par("usr")
  x <- usr[1] + px * (usr[2] - usr[1])
  y <- usr[3] + py * (usr[4] - usr[3])
  
  if (par("ylog"))
    y <- 10^y
  if (par("xlog"))
    x <- 10^x
  
  text(x, y, lab, adj = adj, xpd=NA, ...)
}