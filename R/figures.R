# Plot theme
partial_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text(face='italic') else element_blank()
  theme_classic(base_size = 9) + theme(strip.text = st,
                          legend.title = element_blank(),
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

plot_fig2 <- function(logloss_summaries) {
  ggplot(logloss_summaries, aes(x = model_type,y = mean, group = growth_measure, fill=growth_measure, shape = model)) + 
    geom_blank() +
    geom_rect(aes(xmin = which(model_type == 'function_growth_comparison_base_hazard_none') -1,
                  xmax= which(model_type == 'function_growth_comparison_base_growth_hazard_none')[-2]-0.5,
                  ymin=-Inf, ymax=Inf), fill ='cornsilk2') +
    geom_rect(aes(xmin = which(model_type == 'function_growth_comparison_base_growth_hazard_none')[-2]-0.5,
                  xmax= which(model_type == 'rho_combinations_base_growth_hazard_bc')-0.5,
                  ymin=-Inf, ymax=Inf), fill ='cornsilk') +
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(.5)) +
    ylab('Logarithmic Loss') + 
    xlab('Model') +
    scale_shape_manual(values = c(21, 22, 24)) +
    scale_fill_manual(values =c('white','grey80','black')) +
    scale_x_discrete(labels=c("function_growth_comparison_base_hazard_none" =  expression(gamma),
                              "function_growth_comparison_growth_hazard_none" = expression(alpha*"e"^{-beta~"X"["i"]}),
                              "function_growth_comparison_base_growth_hazard_none" = expression(alpha*"e"^{-beta~"X"["i"]} + gamma),
                              "rho_combinations_base_growth_hazard_a" = expression(alpha),
                              "rho_combinations_base_growth_hazard_b" = expression(beta),
                              "rho_combinations_base_growth_hazard_ab" = expression(alpha~"&"~beta),
                              "rho_combinations_base_growth_hazard_c" = expression(gamma),
                              "rho_combinations_base_growth_hazard_ac" = expression(alpha~"&"~gamma),
                              "rho_combinations_base_growth_hazard_bc" = expression(beta~"&"~gamma),
                              "rho_combinations_base_growth_hazard_abc" = expression(alpha~","~beta~","~gamma),
                              "species_random_effects_base_growth_hazard_none" = expression(alpha[s]*"e"^{-beta[s]~"X"["i"]} + gamma[s]))) +
    partial_plot_theme()
}

plot_fig1a <- function() {
  par(mfrow=c(1,3), mar=c(0.1,0.1,1.5,0.1), oma=c(1.5,1.5,0,0))
  # Example baseline hazard
  plot(seq(0,1,length.out = 14), rep(0.03,14), type='l', ylim=c(0,0.14), xaxs='i', xaxt = "n", yaxt = "n", ylab = NULL, xlab =NULL, lwd=1.5)
  title(main = expression(gamma), line=0.7)
  # Example growth-dependent hazard
  curve(1-exp(-0.15 * exp(-10 * x)), xlim=c(0,1), ylim=c(0,0.14), xaxs='i', xaxt = "n", yaxt = "n", xlab ='X',lwd=1.5)
  title(main = expression(alpha*"e"^{-beta~"X"["i"]}), line=0.7)
  # Example full baseline + growth-dependent hazard
  curve(1-exp(-(0.13 * exp(-10 * x) + 0.03)), xlim=c(0,1), ylim=c(0,0.14),xaxs='i', xaxt = "n", yaxt = "n", xlab ='X', lwd=1.5)
  title(main = expression(alpha*"e"^{-beta~"X"["i"]} + gamma), line=0.7)
  title(ylab="Annual probability of death", line=0, outer= TRUE)
  title(xlab="X", line=0, outer= TRUE)
}

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


plot_fig2b <- function(xlim=c(0,0.5)) {
  suppressMessages(ggplot(dat, aes(x= true_dbh_dt)) +
    geom_density(stat="density",adjust=5, aes(group = as.factor(dead_next_census), 
                                     colour= as.factor(dead_next_census),
                                     fill = as.factor(dead_next_census)),
                 alpha = 0.3) +
    scale_colour_manual(values =c("blue","red")) +
    scale_fill_manual(values =c("blue","red")) +
    xlim(xlim) +
    partial_plot_theme())
}


