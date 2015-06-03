#Compile chains for each growth measure either for each model or across kfolds
compile_growth_model_fits <- function(subset_growth=NULL, pool_kfolds = FALSE) {
  if(any(!subset_growth %in% c('dbh_dt','dbh_dt_rel','basal_area_dt', 'basal_area_dt_rel'))) {
    stop("subset_growth can either be NULL or contain one or multiple of the following: 'dbh_dt', 'dbh_dt_rel', 'basal_area_dt', 'basal_area_dt_rel'")
  }
  pars <- pars_growth() 

  if(!is.null(subset_growth)) {
   pars <- pars[pars$growth_measure %in% subset_growth,]
  }
  if(pool_kfolds==FALSE) {
    sets <- split(pars, pars$modelid)
  } else {
    sets <- split(pars, pars$growth_measure)
  }
  lapply(sets,function(s) {
      files <- s[['filename']]
      x <- sflist2stanfit(lapply(files, readRDS))})
}


#Compile chains for each rho combination either for each model or across kfolds
compile_rho_model_fits <- function(subset_rho_combos=NULL, pool_kfolds = FALSE) {
  if(any(!subset_rho_combos %in% c("","a","b","c","ab","ac","bc","abc"))) {
    stop("subset_rho_combos can either be NULL or contain one or multiple of the following:'','a','b','c','ab','ac','bc','abc'")
  }
  pars <- pars_rho_combos(growth_measure = 'dbh_dt') 
 
  if(!is.null(subset_rho_combos)) {
   pars <- pars[pars$rho_combo %in% subset_rho_combos,]
  }
  if(pool_kfolds==FALSE) {
    sets <- split(pars, pars$modelid)
  } else {
    sets <- split(pars, pars$rho_combo)
  }

  lapply(sets,function(s) {
    files <- s[['filename']]
    x <- sflist2stanfit(lapply(files, readRDS))
  })
}

#Model diagnostics
model_diagnostics <- function(model_comparison = "growth", pool_kfolds = FALSE) {
  if(model_comparison %in% c('growth', 'rho')==FALSE) {
    stop("model_comparison must be growth or rho")
  }
  if(model_comparison == "growth") {
    models <- compile_growth_model_fits(pool_kfolds = pool_kfolds)
  } 
  if(model_comparison == "rho") { # growth_measure will need to be changed depending on outcome of growth comparison.
    models <- compile_rho_model_fits(pool_kfolds = pool_kfolds) 
  }
  out <- do.call(rbind, lapply(models, function(x) {
    summary_model <- summary(x)$summary
    sampler_params <- get_sampler_params(x)
    data.frame(
      n_eff_min = min(summary_model[, 'n_eff']),
      rhat_max = max(summary_model[, 'Rhat']),
      r_bad_n = length(which(summary_model[, 'Rhat'] > 1.1)),
      divergent_n = sum(sapply(sampler_params, function(y) y[,'n_divergent__'])),
      treedepth_max = max(sapply(sampler_params, function(y) y[,'treedepth__'])))
  }))
  out$model_id <- names(models)
  out <- out[, c('model_id','treedepth_max','divergent_n','n_eff_min','r_bad_n','rhat_max')]
}


#Extract parameters for growth models
growth_summaries <- function(subset_params=NULL, subset_growth=NULL, pool_kfolds = TRUE, quantiles= c(0.025, 0.5, 0.975)) {
  models <- compile_growth_model_fits(subset_growth, pool_kfolds)
  if(is.null(subset_params)) {
    f <- function(x) {
      summary(x, probs=quantiles)$summary
    }
  } else {
    f <- function(x) {
      summary(x, pars=subset_params, probs=quantiles)$summary
    }
  }
  lapply(models, f)
}

#Extract parameters for rho models
rho_summaries <- function(subset_params=NULL, subset_growth=NULL, pool_kfolds = TRUE, quantiles= c(0.025, 0.5, 0.975)) {
  models <- compile_rho_model_fits(subset_growth, pool_kfolds)
  if(is.null(subset_params)) {
    f <- function(x) {
      summary(x, probs=quantiles)$summary
    }
  } else{
     f <- function(x) {
      summary(x, pars=subset_params, probs=quantiles)$summary
    }
  }
  lapply(models,f)
}

# Model average coefficient plot

coeff_plot_growth <- function(subset_params =c('a2','b2','c2'), param_names = NULL, subset_growth ='dbh_dt', xlab = NA, ylab = NA, x_tick_labs = TRUE) {
  models <- growth_summaries(subset_params, subset_growth,pool_kfolds = TRUE, quantiles = c(0.025,0.975,0.1,0.9))[[subset_growth]]
  plot(models[,'mean'], rev(seq_len(nrow(models))), xlab=xlab, ylab=ylab, yaxt='n', xaxt='n', pch=21, bg='black',
       xlim=range(pretty(c(models[, '2.5%'], models[, '97.5%']))))
  segments(models[, '2.5%'], seq_len(nrow(models)), models[, '97.5%'], lwd=1, lend=1)
  segments(models[, '10%'], seq_len(nrow(models)), models[, '90%'], lwd=2, lend=1)
  abline(v=0, lty=2)
  if(is.null(param_names)) {
    axis(2, at=seq_len(nrow(models)), labels=rev(subset_params), las=1)
  } else {
    axis(2, at=seq_len(nrow(models)), labels=rev(param_names), las=1)
  }
  if(x_tick_labs == FALSE) {
    axis(1, labels = FALSE)
  } else {
    axis(1)
  }
}

#Plot - rho effects for each model
growth_comparison_plot <- function() {
  par(mfrow=c(4,1), mar=c(2,4,1,1), oma=c(3,3,0,0), lend='butt', cex.axis = 1.2, cex.lab = 1.2)
  coeff_plot_growth(subset_growth = 'dbh_dt', ylab = 'dbh dt', x_tick_labs = FALSE)
  coeff_plot_growth(subset_growth = 'dbh_dt_rel', ylab = 'dbh dt rel', x_tick_labs = FALSE)
  coeff_plot_growth(subset_growth = 'basal_area_dt', ylab = 'basal area dt', x_tick_labs = FALSE)
  coeff_plot_growth(subset_growth = 'basal_area_dt_rel', ylab = 'basal area dt rel', x_tick_labs = TRUE)
  title(ylab = mtext('Rho effects for each growth model', 2, outer=TRUE, line = 1))
  title(xlab = mtext('Effect size', 1, line = 2.5))
}



log_likelihood_plot <- function(model_comparison = 'growth', log_likelihood = 'log_lik_tilde_total', xlab='log likelihood', ylab='Growth model') {
  if(model_comparison =='growth') {
    par(mar=c(6,10,1,1))
    summaries <- growth_summaries(log_likelihood, pool_kfolds = TRUE, quantiles = c(0.025,0.975,0.1,0.9))
    models <- do.call(rbind, summaries)
    row.names(models) <- names(summaries)
    models <- models[c('basal_area_dt_rel', 'basal_area_dt', 'dbh_dt_rel', 'dbh_dt'),]
    y_tick_labels <- c('rel basal area dt', 'basal area dt', 'rel dbh dt', 'dbh dt')
    
    plot(models[,'mean'], seq_len(nrow(models)), xlab=xlab, ylab=NA, yaxt='n', pch=21, bg='black',
         xlim = range(pretty(c(models[, '2.5%'], models[, '97.5%']))))
    axis(2, seq_len(nrow(models)), labels = y_tick_labels, las = 1)
    title(ylab = ylab, line = 8)
    segments(models[, '2.5%'], seq_len(nrow(models)), models[, '97.5%'], lwd=1)
    segments(models[, '10%'], seq_len(nrow(models)), models[, '90%'], lwd=2)
    
  } else if(model_comparison =='rho') {
    par(mar=c(5,4,1,1))
    summaries <- rho_summaries(log_likelihood, pool_kfolds = TRUE, quantiles = c(0.025,0.975,0.1,0.9))
    models <- do.call(rbind, summaries)
    row.names(models) <- names(summaries)
    row.names(models)[1] <- 'none'
    models <- models[c('abc','bc','ac','ab','c','b','a','none'),]
    y_tick_labels <- row.names(models)
    
    plot(models[,'mean'], seq_len(nrow(models)), xlab=xlab, ylab=NA, yaxt='n', pch=21, bg='black',
         xlim = range(pretty(c(models[, '2.5%'], models[, '97.5%']))))
    axis(2, seq_len(nrow(models)), labels = y_tick_labels, las = 1)
    title(ylab = ylab, line = 3)
    segments(models[, '2.5%'], seq_len(nrow(models)), models[, '97.5%'], lwd=1)
    segments(models[, '10%'], seq_len(nrow(models)), models[, '90%'], lwd=2)
  }
}



#Best model simulation plot
sim_mortality <- function(subset_growth ='dbh_dt', rho=c(200,800), growth_range=NULL, mortality_curve = FALSE){
  c_ln_rho <- log(rho) - log(600)
  model <- compile_growth_model_fits(subset_growth = subset_growth, pool_kfolds = TRUE)[[subset_growth]]
  model_sims <- extract(model, pars =c('a0_mu', 'b0_mu', 'c0_mu', 'a1_mu', 'b1_mu', 'c1_mu', 'a2', 'b2', 'c2'))
  if(is.null(growth_range)) {
    dat <- readRDS('export/bci_data_full.rds')
    growth_range <- range(dat$train[,subset_growth])
  }
  growth <- seq(min(growth_range), max(growth_range),length.out = 100)
  predictions <- sapply(rho, function(rho) {
    with(model_sims,
         mapply(function(a0_mu, b0_mu, c0_mu, a1_mu, b1_mu, c1_mu, a2, b2, c2) {
           a_log = a0_mu + a1_mu + a2 * c_ln_rho
           b_log = b0_mu + b1_mu + b2 * c_ln_rho
           c_log = c0_mu + c1_mu + c2 * c_ln_rho
           if (mortality_curve ==TRUE){
             preds <-1 - exp(-(exp(a_log - exp(b_log) * growth) + exp(c_log)))
           }
           else {
             preds <- exp(a_log - exp(b_log) * growth) + exp(c_log)
           }
         }, a0_mu, b0_mu, c0_mu, a1_mu, b1_mu, c1_mu, a2, b2, c2))}, simplify = 'array')
  output <- list(mn = apply(predictions,c(1,3), mean),
                 l95 = apply(predictions, c(1,3), quantile, 0.025),
                 u95 =  apply(predictions, c(1,3), quantile, 0.975),
                 growth = growth)
  return(output)
}


# Plot hazard and mortality curves
plot_compare_rho_mortality <- function(subset_growth ='dbh_dt', xlab = 'dbh dt', 
                                       rho=c(200,800), growth_range=NULL, mortality_curve = TRUE, 
                                       ylim=c(0,1), legend = TRUE) {
  if(length(rho) >2) {
    stop("Only two curves can be plotted at any one time")
  }
  par(xaxs='i')
  output <- sim_mortality(subset_growth = subset_growth, rho = rho, growth_range = growth_range, mortality_curve = mortality_curve)
  plot(mn[,1] ~ growth, type='n', data=output, ylim=ylim, 
       ylab='Pr(Mortality)/year', xlab= xlab)
  
  polygon(c(output$growth, rev(output$growth)),
          c(output$l95[,1], rev(output$u95[,1])),
          col=rgb(1,0,0,0.5), border=NA)
  lines(mn[,1] ~ growth, type='l', lwd=2, data=output, col='darkred')
  
  if(length(rho)>1) {
    polygon(c(output$growth, rev(output$growth)),
            c(output$l95[,2], rev(output$u95[,2])),
            col=rgb(0,0,1,0.5), border=NA)
    lines(mn[,2] ~ growth, type='l', lwd=2, data=output, lty=2, col='blue')
    
    if(legend==TRUE) {
      legend('topright',
             legend = c(paste('rho', '=', rho[1]), (paste('rho', '=', rho[2]))),
             bty = 'n', col=c('darkred', 'blue'), lty=c(1,2), lwd=2)
    }
  }
}
