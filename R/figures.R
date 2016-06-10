# Plot theme
partial_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text(face='italic') else element_blank()
  theme_classic(base_size = 7) + theme(strip.text = st,
                                       strip.background = sb,
                                       legend.position = legend.position,
                                       axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                                       axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                                       plot.margin = unit(c(3,3,3,3), "mm"))
}
# Plot observed vs predicted true dbh and growth
plot_obs_v_pred_growth <- function(data) {
  
  p1 <- ggplot(data = data,  aes(x = dbh_prev, y = true_dbh1)) + 
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1) +
    ylab('Estimated true dbh at t-1 (cm)') + 
    xlab('Observed dbh at t-1 (cm)') +
    partial_plot_theme()
  
  p2 <- ggplot(data = data,  aes(x = dbh, y = true_dbh2)) + 
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1) +
    ylab('Estimated true dbh at t (cm)') + 
    xlab('Observed dbh at t (cm)') +
    partial_plot_theme()
  
  p3 <- ggplot(data = data,  aes(x = obs_dbh_dt, y = true_dbh_dt)) + 
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1) +
    ylab('Estimated true annual dbh growth (cm)') + 
    xlab('Observed annual dbh growth (cm)') +
    partial_plot_theme()
  
  p4 <- ggplot(data = data,  aes(x = obs_basal_area_dt, y = true_basal_area_dt)) + 
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1) +
    ylab('Estimated true annual basal area growth (cm)') + 
    xlab('Observed annual basal area growth (cm)') +
    partial_plot_theme()
  
  plot_grid(p1, p3, p2, p4, ncol=2, labels=LETTERS[1:4], label_size = 7)
}

plot_fig2a <- function(logloss_summaries) {
  dat <- logloss_summaries %>%
    filter(model_type == "rho_combinations_base_growth_hazard_c" |
             comparison %in% c("function_growth_comparison","species_random_effects"))
  
  ggplot(dat, aes(x = model_type,y = mean, group = growth_measure, fill=growth_measure, shape = model)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(.5)) +
    ylab('Logarithmic Loss') + 
    xlab('Model') +
    scale_shape_manual(values = c(21, 24, 22)) +
    scale_fill_manual(values =c('white','grey80','black')) +
    scale_x_discrete(labels=c("function_growth_comparison_base_hazard_none" =  expression(gamma),
                              "function_growth_comparison_growth_hazard_none" = expression(alpha*"e"^{-beta~"X"["i"]}),
                              "function_growth_comparison_base_growth_hazard_none" = expression(alpha*"e"^{-beta~"X"["i"]} + gamma),
                              "rho_combinations_base_growth_hazard_c" = expression(alpha*"e"^{-beta~"X"["i"]} + gamma*rho["s"]^gamma[1]),
                              "species_random_effects_base_growth_hazard_none" = expression(alpha[s]*"e"^{-beta[s]~"X"["i"]} + gamma[s]))) +
    partial_plot_theme()
}


plot_fig2b <- function(logloss_summaries) {
  dat <- logloss_summaries %>%
    filter(growth_measure == "true_dbh_dt" &
             (comparison == "rho_combinations" |
                model_type == "function_growth_comparison_base_growth_hazard_none"))
  
  ggplot(dat, aes(x = model_type,y = mean)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), shape=22, fill='black') +
    ylab('Logarithmic Loss') + 
    xlab(expression('Wood density effects on'~alpha*"e"^{-beta~"X"["i"]} + gamma)) +
    scale_x_discrete(labels=c("function_growth_comparison_base_growth_hazard_none" = "none",
                              "rho_combinations_base_growth_hazard_a" = expression(alpha),
                              "rho_combinations_base_growth_hazard_b" = expression(beta),
                              "rho_combinations_base_growth_hazard_ab" = expression(alpha~"&"~beta),
                              "rho_combinations_base_growth_hazard_c" = expression(gamma),
                              "rho_combinations_base_growth_hazard_ac" = expression(alpha~"&"~gamma),
                              "rho_combinations_base_growth_hazard_bc" = expression(beta~"&"~gamma),
                              "rho_combinations_base_growth_hazard_abc" = expression(alpha~","~beta~","~gamma))) +
    partial_plot_theme()
}


# Plot species predicted mortality v growth curves
plot_spp_curves <- function(model, data, growth_range= c(0.03,0.5), hazard_curve = FALSE) {
  preds <- predict_spp_hazard(model, data, growth_range)
  if(hazard_curve ==FALSE) {
    ggplot(preds, aes(x = dbh_growth,y = annual_prob_mort, group = sp, colour = wood_density)) + 
      geom_line(alpha=0.4, size=0.3) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), limits = c(0, NA)) +
      ylab("Annual mortality probability") +
      xlab("Annual dbh growth (cm)") +
      scale_colour_gradient("wood density",low="blue", high="red") +
      partial_plot_theme(legend.position="right")
  }
  else {
    ggplot(preds, aes(x = dbh_growth,y = inst_hazard, group = sp, colour = wood_density)) + 
      geom_line(alpha=0.4, size=0.2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), limits = c(0, NA)) +
      ylab("Instantaneous mortality rate") +
      xlab("Annual dbh growth (cm)") +
      scale_colour_gradient("wood density",low="blue", high="red") +
      partial_plot_theme(legend.position="right")
  }
}

plot_mu_curves <- function(model,wood_density=c(0.3,0.8), growth_range = c(0.03,0.5), hazard_curve = FALSE) {
  preds <- predict_mu_hazards(model,wood_density, growth_range, hazard_curve)
  
  if(hazard_curve ==FALSE) {
    ylab <- "Annual mortality probability"
    limits = c(0, 1)
  }
  else {
    ylab <- "Instantaneous mortality rate"
    limits = c(0, NA)
  }
  ggplot(preds, aes(x = dbh_growth,y = mean, group = type, colour = wood_density, fill=wood_density)) + 
    geom_line(size=0.3) +
    scale_color_gradient(limits=c(0.197,NA),low='blue',high='red') +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) +
    scale_fill_gradient(limits=c(0.2,NA),low='blue',high='red') +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits = limits) +
    ylab(ylab) +
    xlab("Annual dbh growth (cm)") +
    partial_plot_theme()
}

# Manuscript figures

plot_fig1 <- function() {
  growth_haz <- function(x) {
    1-exp(-0.15 * exp(-10 * x))
  }
  
  base_growth_haz <- function(x) {
    1-exp(-(0.13 * exp(-10 * x) + 0.03))
  }
  
  layout(matrix(c(1,2,3,4,4,4,5,5,5), byrow=TRUE, ncol=3))
  par(mar=c(1.3,1.3,1.3,0.1),ps = 7)
  # Example baseline hazard
  plot(seq(0,1,length.out = 14), rep(0.03,14), type='l', ylim=c(0,0.14), xaxs='i', xaxt = "n", yaxt = "n", ylab = NULL, xlab =NULL, lwd=1.5)
  title(main = expression(gamma), line=0.7)
  title(ylab="Annual probability of death", line=0.5)
  
  # Example growth-dependent hazard
  curve(growth_haz(x), xlim=c(0,1), ylim=c(0,0.14), xaxs='i', xaxt = "n", yaxt = "n", xlab = NULL,lwd=1.5)
  title(main = expression(alpha*"e"^{-beta~"X"["i"]}), line=0.7)
  title(xlab="X", line=0.5)
  
  # Example full baseline + growth-dependent hazard
  curve(base_growth_haz(x), xlim=c(0,1), ylim=c(0,0.14),xaxs='i', xaxt = "n", yaxt = "n", xlab =NULL, lwd=1.5)
  title(main = expression(alpha*"e"^{-beta~"X"["i"]} + gamma), line=0.7)
  
  # Place holder for tree growth diagram
  plot(c(0, 1), c(0, 1), ann = F, type = 'n', xaxt = 'n', yaxt = 'n')
  text(0.5,0.5,paste("place holder for \n", "tree growth diagram"))
  
  #place holder for cross val diagram
  plot(c(0, 1), c(0, 1), ann = F, type = 'n', xaxt = 'n', yaxt = 'n')
  text(0.5,0.5,paste("place holder for \n","crossval diagram"))
}

plot_fig2 <- function(logloss_summaries) {
  p1 <- plot_fig2a(logloss_summaries)
  p2 <- plot_fig2b(logloss_summaries)
  plot_grid(p1,p2, ncol=1, labels=LETTERS[1:2], label_size = 7)
}

# Plot gamma vs wood density with mean trend line
plot_fig3 <- function(model, data) {
spp <- summarise_spp_params(model, data)$gamma
med <- predict_mu_baseline_hazard(model, data)

ggplot(spp, aes(x = wood_density,y = log(mean))) + 
      geom_pointrange(aes(ymin = log(`2.5%`), ymax=log(`97.5%`)), size=0.1) +
      geom_ribbon(data = med, aes(ymin = log(`2.5%`),ymax = log(`97.5%`)), alpha=0.4, colour=NA) +
      geom_line(data = med, aes(x = wood_density, y= log(mean)), size=1) +
      partial_plot_theme() +
     ylab("log(instantaneous mortality rate)") +
     xlab("Wood density")
}

plot_fig4 <- function(model, data) {
  p1 <- plot_mu_curves(model, hazard_curve= TRUE) + ggtitle('Median species')
  p2 <- plot_spp_curves(model, data, hazard_curve = TRUE) + partial_plot_theme(legend.position= c(0.8,0.5)) + ggtitle('All species')
  p3 <- plot_mu_curves(model, hazard_curve= FALSE)
  p4 <- plot_spp_curves(model, data, hazard_curve = FALSE) + partial_plot_theme()
  plot_grid(p1,p2,p3,p4, ncol=2, labels=LETTERS[1:4], label_size = 7)
}

# plot_fig2a <- function() {
# df <- data.frame(
#   x = seq(0,4,length.out = 100)
# )
# 
# prob_mort <- function(x) {
#   1-exp(-5 * (0.13 * exp(-10 * x) + 0.03))
# }
# 
# ggplot(df, aes(x)) +
#   stat_function(fun = prob_mort) +
#   partial_plot_theme()
#}


