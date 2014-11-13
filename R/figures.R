# source("R/utils.R")
# library(R2jags)
# load('output/JAGS_mort.rda')
# # Prep for simulations
# sim <- sim.coeff(dbh.mod, c('b.3', 'b.2', 'b.1', 'GM'))
# cov.info <- cbind(data.summary(bci.mainstem, c('dbh','dbh.gr','dbh.rgr','basal.area','basal.area.gr','basal.area.rgr')), # individual level
#                   data.summary(unique(bci.mainstem[,c('sp','sg100c_avg')])['sg100c_avg'], c('sg100c_avg'))) # species level


dbh.figs <- function(){
  par(mfrow=c(3,2))

  #dbh
  coeff.plot(dbh.mod, c('b.3', 'b.2','b.1','GM'), labels = c('WD x DBH','DBH','WD','GM'),
             xlab = 'cloglog (effect size)')
  plot.3d(jagsfit = dbh.mod, x = seq(0.2,0.8,0.05), y = seq(11,230, 10), xmean =
            cov.info['mean', 'sg100c_avg'], xstd = cov.info['std', 'sg100c_avg'],
          ymean = cov.info['mean', 'dbh'], ystd = cov.info['std', 'dbh'],
          xlab = 'WD', ylab= 'DBH', zlab = 'Prob dying')

  #dbh.gr
  coeff.plot(dbh.gr.mod, c('b.3', 'b.2','b.1','GM'), labels = c('WD x DBH.gr','DBH.gr','WD','GM'),
             xlab = 'cloglog (effect size)')
  plot.3d(jagsfit = dbh.gr.mod, x = seq(0.2,0.8,0.05), y = seq(0,5, 0.5), xmean =
            cov.info['mean', 'sg100c_avg'], xstd = cov.info['std', 'sg100c_avg'],
          ymean = cov.info['mean', 'dbh.gr'], ystd = cov.info['std', 'dbh.gr'],
          xlab = 'WD', ylab= 'DBH.gr', zlab = 'Prob dying')

  #dbh.rgr
  coeff.plot(dbh.rgr.mod, c('b.3', 'b.2','b.1','GM'), labels = c('WD x DBH.gr','DBH.rgr','WD','GM'),
             xlab = 'cloglog (effect size)')
  plot.3d(jagsfit = dbh.rgr.mod, x = seq(0.2,0.8,0.05), y = seq(0,0.1, 0.01), xmean =
            cov.info['mean', 'sg100c_avg'], xstd = cov.info['std', 'sg100c_avg'],
          ymean = cov.info['mean', 'dbh.rgr'], ystd = cov.info['std', 'dbh.rgr'],
          xlab = 'WD', ylab= 'DBH.rgr', zlab = 'Prob dying')
}


basal.figs <-function(){
  #basal.area
  par(mfrow=c(3,2))
  coeff.plot(basal.area.mod, c('b.3', 'b.2','b.1','GM'), labels = c('WD x BA','BA','WD','GM'),
             xlab = 'cloglog (effect size)')
  plot.3d(jagsfit = basal.area.mod, x = seq(0.2,0.8,0.05), y = seq(150,44000, 2200), xmean =
            cov.info['mean', 'sg100c_avg'], xstd = cov.info['std', 'sg100c_avg'],
          ymean = cov.info['mean', 'basal.area'], ystd = cov.info['std', 'basal.area'],
          xlab = 'WD', ylab= 'basal area', zlab = 'Prob dying')

  #basal.area.gr
  coeff.plot(basal.gr.mod, c('b.3','b.2','b.1','GM'), labels = c('WD x BA.gr','BA.gr','WD','GM'),
             xlab = 'cloglog (effect size)')
  plot.3d(jagsfit = basal.gr.mod, x = seq(0.2,0.8,0.05), y = seq(0,1100, 50), xmean =
            cov.info['mean', 'sg100c_avg'], xstd = cov.info['std', 'sg100c_avg'],
          ymean = cov.info['mean', 'basal.area.gr'], ystd = cov.info['std', 'basal.area.gr'],
          xlab = 'WD', ylab= 'basal gr', zlab = 'Prob dying')

  #basal.area.rgr
  coeff.plot(basal.rgr.mod, c('b.3', 'b.2','b.1','GM'), labels = c('WD x BA.rgr','BA.rgr','WD','GM'), xlab = 'cloglog (effect size)')
  plot.3d(jagsfit = basal.rgr.mod, x = seq(0.2,0.8,0.05), y = seq(0,0.2, 0.01), xmean =
            cov.info['mean', 'sg100c_avg'], xstd = cov.info['std', 'sg100c_avg'],
          ymean = cov.info['mean', 'basal.area.rgr'], ystd = cov.info['std', 'basal.area.rgr'],
          xlab = 'WD', ylab= 'basal rgr', zlab = 'Prob dying')
}

# to.pdf(dbh.figs(), "output/dbh.figs.pdf", height=10, width=10)
# to.pdf(basal.figs(), "output/basal.figs.pdf", height=10, width=10)
