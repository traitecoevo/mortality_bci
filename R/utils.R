# Coefficient Plot

coeff_plot <- function(stanfit, params, labels=NULL,quantiles =c(0.025,0.975,0.1,0.9), xlab='effect size', transform=NULL) {
  dat <- summary(stanfit, pars=params, probs=quantiles)$summary
  if (!is.null(transform))
    dat <-transform(dat)

  if (is.null(labels)) labels <- row.names(dat)
  opar <- par(mai=c(1, max(strwidth(labels, 'inches')) + 0.3, 0.5, 0.5))
  on.exit(par(opar))
  plot(dat[,'mean'], seq_len(nrow(dat)), xlab='', ylab='', yaxt='n', pch=21, bg='black',
       xlim=range(pretty(range(dat[, c(min(paste0((quantiles*100), '%')),
                                       paste0(max(quantiles*100), '%'))]))),

       panel.first={
         abline(v=0, lty=3)
         if(length(quantiles) == 2) {
           segments(dat[, paste0(quantiles*100, '%')], seq_len(nrow(dat)),
                    x1=dat[, paste0(quantiles*100, '%')], lend=1)
         } else if(length(quantiles) == 4) {
           segments(dat[, paste0(quantiles[1]*100, '%')], seq_len(nrow(dat)), x1=dat[, paste0(quantiles[2]*100, '%')], lwd=1, lend=1)
           segments(dat[, paste0(quantiles[3]*100, '%')], seq_len(nrow(dat)), x1=dat[, paste0(quantiles[4]*100, '%')], lwd=3, lend=1)
         }
         else {
           stop('Only two credible intervals can be plotted at any one time')
         }
       })
  axis(2, at=seq_len(nrow(dat)), labels=labels, las=1)

  mtext(xlab, side = 1, line=2.5)
}

# Model simulation
sim_mortality <- function(model_fit, model_data, cov_name='rho',
                          wd_values=c(200,800), growth_name='dbh_dt',
                          growth_range=NULL, mortality_curve = FALSE){
  model_fit <- extract(model_fit)

  new_cs_lnwd <- (log(wd_values) - mean(log(model_data[[cov_name]])))/ (2*sd(log(model_data[[cov_name]])))

  if (is.null(growth_range)){
    new_s_dbh_dt <- seq(min(model_data[[growth_name]]),max(model_data[[growth_name]]), by=0.0001)/ (2*sd(model_data[[growth_name]]))
  }
  else {
    new_s_dbh_dt <- seq(min(growth_range), max(growth_range), by=0.0001)/ (2*sd(model_data[[growth_name]]))}

  preds <- sapply(new_cs_lnwd, function(wd) {
    with(model_fit,
         mapply(function(a0_mu, b0_mu, c0_mu, a1, b1, c1) {
           a_log = a0_mu + a1 * wd
           b_log = b0_mu + b1 * wd
           ho_log = c0_mu + c1 * wd
           if (mortality_curve ==TRUE){
             preds <-1 - exp(-(exp(a_log - exp(b_log) * new_s_dbh_dt) + exp(ho_log)))
           }
           else {
             preds <- exp(a_log - exp(b_log) * new_s_dbh_dt) + exp(ho_log)
           }
         }, a0_mu, b0_mu, c0_mu, a1, b1, c1))}, simplify = 'array')
  output <- list(mn = apply(preds,c(1,3), mean),
                 l95 = apply(preds, c(1,3), quantile, 0.025),
                 u95 =  apply(preds, c(1,3), quantile, 0.975),
                 new_s_dbh_dt = new_s_dbh_dt)
  return(output)
}


# Plot hazard and mortality curves
plot_mortality <- function(model_fit, model_data, cov_name='rho', wd_values=c(200,800),
                           growth_name='dbh_dt', growth_range=NULL,
                           mortality_curve = FALSE, xaxis_on=TRUE,legend=TRUE, haz_lim=NULL,xlim=xlim) {

  output <- sim_mortality(model_fit = model_fit, model_data = model_data, cov_name= cov_name, wd_values=wd_values, growth_name=growth_name, growth_range=growth_range, mortality_curve = mortality_curve)

  if (mortality_curve == TRUE){
    plot(mn[,1]*100~ new_s_dbh_dt, type='n', data=output, ylim=c(0,100),
         xaxt='n', ylab='Pr(Mortality)/year', xlab=NA)

    polygon(c(output$new_s_dbh_dt, rev(output$new_s_dbh_dt)),
            c(output$l95[,1]*100, rev(output$u95[,1]*100)),
            col=rgb(1,0,0,0.5), border=NA)

    lines(mn[,1]*100~new_s_dbh_dt, type='l', lwd=2, data=output, col='darkred')

    polygon(c(output$new_s_dbh_dt, rev(output$new_s_dbh_dt)),
            c(output$l95[,2]*100, rev(output$u95[,2]*100)),
            col=rgb(0,0,1,0.5), border=NA)

    lines(mn[,2]*100~new_s_dbh_dt, type='l', lwd=2, data=output, lty=2, col='blue')
  }

  else {
    if (is.null(haz_lim)) {
      haz_lim <- range(pretty(output$u95))
    }
    plot(mn[,1]~ new_s_dbh_dt, type='n', data=output, ylim=haz_lim,
         xaxt='n', ylab='Instantaneous hazard rate', xlab=NA)

    polygon(c(output$new_s_dbh_dt, rev(output$new_s_dbh_dt)),
            c(output$l95[,1], rev(output$u95[,1])),
            col=rgb(1,0,0,0.5), border=NA)

    lines(mn[,1]~new_s_dbh_dt, type='l', lwd=2, data=output, col='darkred')

    polygon(c(output$new_s_dbh_dt, rev(output$new_s_dbh_dt)),
            c(output$l95[,2], rev(output$u95[,2])),
            col=rgb(0,0,1,0.5), border=NA)

    lines(mn[,2]~new_s_dbh_dt, type='l', lwd=2, data=output, lty=2, col='blue')
  }
  if(legend == TRUE){
    legend('topright',
           legend = c(paste(cov_name, '=', wd_values[1]), (paste(cov_name, '=', wd_values[2]))),
           bty = 'n', col=c('darkred', 'blue'), lty=c(1,2), lwd=2)
  }
  if(xaxis_on==TRUE){
    axis(1,at = pretty(output$new_s_dbh_dt),
         label= round(pretty(output$new_s_dbh_dt)*(2 * sd(model_data$dbh_dt)),3))
    title(xlab ='DBH growth (m)')
  }
  else
    axis(1,at = pretty(output$new_s_dbh_dt), label= NA)
}

#3-dimensional plot
plot3d <- function(model_fit, model_data,  cov_name='rho', wd_values=seq(200:800, by=5),
                    growth_name='dbh_dt', growth_range=NULL,
                    mortality_curve = FALSE, legend=TRUE, haz_lim=NULL, theta=110, phi=20,
                    ticktype='detailed') {


  output <- sim_mortality(model_fit = model_fit, model_data = model_data, cov_name= cov_name,
                          wd_values=wd_values, growth_name=growth_name, growth_range=growth_range,
                          mortality_curve = mortality_curve)

  nrz <- nrow(output$mn)
  ncz <- ncol(output$mn)
  jet.colors <- colorRampPalette(c('blue','yellow','orange', 'red'))
  # Generate the desired number of colors from this palette
  nbcol <- 10000
  color <- jet.colors(nbcol)
  # Compute the z-value at the facet centres
  zfacet <- output$mn[-1, -1] + output$mn[-1, -ncz] + output$mn[-nrz, -1] + output$mn[-nrz, -ncz]
  # Recode facet z-values into color indices
  facetcol <- cut(zfacet, nbcol)

  if (mortality_curve==TRUE){
    persp(output$new_s_dbh_dt*(sd(model_data[[growth_name]])), wd_values, output$mn,
          col = color[facetcol], xlab= 'dbh growth', ylab='rho',
          zlab='Mortality probablity/yr', theta=theta, phi=phi, border=NA,ticktype=ticktype)
  }
  else {
    persp(output$new_s_dbh_dt*(sd(model_data[[growth_name]])), wd_values, output$mn,
          col = color[facetcol], xlab= 'dbh growth', ylab='rho',
          zlab='Instantaneous hazard rate', theta=theta, phi=phi, border=NA,ticktype=ticktype)
  }
}
