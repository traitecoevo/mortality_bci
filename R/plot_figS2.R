# Plot Fig S2 for manuscript
#' 
# Plot Fig S2 for manuscript
#' @param gap_index_raster List of rasters
#' @param recruit_gap_conditions List of SpatialPointsDataframes
#' @return Figure S2 of manuscript
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
plot_figS2 <- function(gap_index_raster, recruit_gap_conditions) {
  greys <- grey.colors(20, start =0.1, end=1.0)
  
  print(rasterVis::levelplot(raster::stack(gap_index_raster), 
                             names.attr=paste("Recruits from", names(gap_index_raster)),
                             layout=c(1, 2), 
                             colorkey=list(height=0.6), 
                             scales=list(alternating=FALSE, tck=c(0.5, 0.5), 
                                         x=list(at=seq(0, 1000, 200)),
                                         y=list(at=seq(0, 400, 100))),
                             col.regions= greys[c(1,1, seq_along(greys), 20, 20)],
                             at=seq(0,1,0.05),
                             par.settings = list(fontsize = list(text = 7), 
                                                 strip.background=list(col='white'),
                                                 strip.border=list(col='transparent'))) +
          latticeExtra::layer(sp::sp.points(SpatialPoints(recruit_gap_conditions[[panel.number()]]), 
                                            pch='+', col='red', alpha=0.6, cex=0.1),
                              data=list(recruit_gap_conditions=recruit_gap_conditions)))
  
}