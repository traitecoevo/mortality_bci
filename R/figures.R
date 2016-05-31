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
plot_function_growth_comparison <- function(logloss_summary, title=NULL) {
  dat <- logloss_summary %>%
    filter(model!= "base_hazard" | growth_measure !='true_basal_area_dt') %>%
    mutate(growth_measure = replace(growth_measure, model=="base_hazard", "none")) %>%
    mutate(model = factor(model, levels=c('base_hazard','growth_hazard', 
                                          'base_growth_hazard'))) %>%
    arrange(model)
  
  ggplot(dat, aes(x = model,y = mean, group = as.factor(growth_measure), colour=as.factor(growth_measure))) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(.1), size=0.2) +
    scale_colour_manual('',values = c("true_basal_area_dt" ="blue","true_dbh_dt" ="red", "none" = "black")) +
    ylab('Logarithmic Loss') + 
    xlab('Model') +
    scale_x_discrete(labels=c("base_hazard" = expression(gamma),
                              "growth_hazard" = expression(alpha*"e"^{-beta~"growth"["i"]}),
                              "base_growth_hazard" = expression(alpha*"e"^{-beta~"growth"["i"]} + gamma))) +
    ylim(0.45,0.511) +
    labs(title=title) +
    partial_plot_theme()
}

# Plot mean CV log likelihoods for growth comparison
plot_random_effect_comparison <- function(log_loss_summary, title=NULL) {
  dat <- filter(log_loss_summary, growth_measure =='true_dbh_dt' &
                  (model =='base_growth_hazard' | model =='base_growth_hazard_re')) %>%
    mutate(model = factor(model, levels=c("base_growth_hazard","base_growth_hazard_re"))) %>%
    arrange(model)
  
  ggplot(dat, aes(x = model,y = mean)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), size=0.2) +
    ylab('Logarithmic Loss') +
    xlab('Model') +
    scale_x_discrete(labels=c("base_growth_hazard" = expression(alpha*"e"^{-beta~"growth"["i"]} + gamma),
                                  "base_growth_hazard_re" = expression(alpha["s"]*"e"^{-beta["s"]~"growth"["i"]} + gamma["s"]))) +
    ylim(0.45,0.511) +
    labs(title=title) +
    partial_plot_theme()
}

# Plot mean CV log likelihoods for rho combination comparison
plot_rho_comparison <- function(log_loss_summary, title = NULL) {
  dat <- filter(log_loss_summary, 
                  logloss =='logloss_heldout') %>%
    mutate(rho_combo = factor(rho_combo, levels=c("none","a","b","c","ab","ac","bc","abc"))) %>%
    arrange(rho_combo)
  
  ggplot(dat, aes(x = rho_combo,y = mean)) + 
    geom_pointrange(aes(ymin=`2.5%`, ymax=`97.5%`), size=0.2) +
    xlab('Wood density parameter combinations') +
    ylab('Logarithmic loss') +
    ylim(0.45,0.511) +
    scale_x_discrete(labels=c("none" = "none",
                              "a" = expression(alpha),
                              "b" = expression(beta),
                              "c" = expression(gamma),
                              "ab" = expression(alpha~"&"~beta),
                              "ac" = expression(alpha~"&"~gamma),
                              "bc" = expression(beta~"&"~gamma),
                              "abc" = expression(alpha~","~beta~","~gamma))) +
    labs(title=title) +
    partial_plot_theme()
}

plot_fig1 <- function(logloss_func_growth, logloss_randomeffects, logloss_rho_combo) {
  p1 <- plot_function_growth_comparison(logloss_func_growth, title="Stage 1")
  p2 <- plot_random_effect_comparison(logloss_randomeffects, title="Stage 2")
  p3 <- plot_rho_comparison(logloss_rho_combo, title="Stage 3")
  plot_grid(p1, p2, p3, ncol=1, labels=LETTERS[1:3], label_size = 7)
  }
