# Plot theme
partial_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text(face='italic') else element_blank()
  theme_classic(base_size = 7) + theme(strip.text = st,
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

# Plot mean CV log likelihoods for functional comparison
plot_function_growth_comparison <- function(logloss_summary) {
  dat <- logloss_summary %>%
    mutate(model = factor(model, levels=c('base_hazard','growth_hazard', 
                                          'base_growth_hazard'))) %>%
    mutate(model = factor(model, labels = c( "Baseline hazard","Growth hazard","Baseline & growth hazard"))) %>%
    arrange(model)
  
  ggplot(dat, aes(x = model,y = mean, group = as.factor(growth_measure), colour=as.factor(growth_measure))) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(.1), size=0.2) +
    scale_colour_manual('',values = c("true_basal_area_dt" ="blue","true_dbh_dt" ="red", "none" = "black")) +
    ylab('Average Logarithmic Loss') + 
    xlab('Model') +
    partial_plot_theme()
}

# Plot mean CV log likelihoods for growth comparison
plot_random_effect_comparison <- function(log_loss_summary) {
  dat <- filter(log_loss_summary, model =='growth_comparison' & 
                  logloss =='logloss_heldout' & 
                  rho_combo=='') %>%
    mutate(model = factor(growth_measure, levels=c("base_growth_hazard_re","base_growth_hazard"))) %>%
    mutate(model = factor(growth_measure, labels = c("Without","With"))) %>%
    arrange(growth_measure)
  
  ggplot(dat, aes(x = mean,y = model)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(.1), size=0.2) +
    xlab('Log likelihood') +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    partial_plot_theme()
}

# Plot mean CV log likelihoods for rho combination comparison
plot_rho_comparison <- function(log_loss_summary) {
  dat <- filter(log_loss_summary, 
                  logloss =='logloss_heldout') %>%
    mutate(rho_combo = factor(rho_combo, levels=c("abc","bc","ac","ab","c","b","a","none"))) %>%
    mutate(rho_combo = factor(rho_combo, labels = c("all parameters","beta & gamma","alpha & gamma",'alpha & beta', "gamma", "beta","alpha","none"))) %>%
    arrange(rho_combo)
  
  ggplot(dat, aes(x = mean,y = rho_combo)) + 
    geom_segment(aes(x=`2.5%`,y=rho_combo, xend=`97.5%`, yend=rho_combo), size=0.2)+
    geom_point(aes(x=mean, y=rho_combo), size =0.5) +
    xlab('Logarithmic loss') +
    ylab('Wood density parameter combinations') +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    partial_plot_theme()
}
