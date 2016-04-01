coefficent_plot_theme <- function() {
  theme_classic() + theme(axis.title.x = element_text(face = "bold", size = 12),
                          axis.title.y = element_blank(),
                          axis.text.x = element_text(size = 10),
                          axis.text.y = element_text(size = 12),
                          plot.title = element_text(size = 18),
                          title = element_text(face = "bold"),
                          panel.margin = unit(4,"mm"))
}


response_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text(color = "black", face = "bold", size = 10) else element_blank()
  theme_classic() + theme(axis.title = element_text(face = "bold", size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = st,
                          legend.title = element_text(size = 10),
                          legend.text = element_text(size = 10), 
                          plot.title = element_text(size = 18),
                          legend.title = element_blank(),
                          strip.background = sb,
                          legend.position = legend.position,
                          panel.margin = unit(4,"mm"))
}

# Allows different ggplots to be included on same page
# Code from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## CURRENTLY BELOW WILL NOT WORK WITH THIS WORKFLOW. NEEDS TO BE UPDATED

# Plot observed vs predicted true dbh and growth
plot_obs_v_pred_growth <- function(data) {
  
  p1 <- ggplot(data = data,  aes(x = dbh_prev, y = true_dbh1)) + 
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1) +
    ylab('Estimated true dbh at t-1 (cm)') + 
    xlab('Observed dbh at t-1 (cm)') +
    response_plot_theme()
  
  p2 <- ggplot(data = data,  aes(x = dbh, y = true_dbh2)) + 
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1) +
    ylab('Estimated true dbh at t (cm)') + 
    xlab('Observed dbh at t (cm)') +
    response_plot_theme()
  
  p3 <- ggplot(data = data,  aes(x = obs_dbh_dt, y = true_dbh_dt)) + 
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1) +
    ylab('Estimated true annual dbh growth (cm)') + 
    xlab('Observed annual dbh growth (cm)') +
    response_plot_theme()
  
  p4 <- ggplot(data = data,  aes(x = obs_basal_area_dt, y = true_basal_area_dt)) + 
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1) +
    ylab('Estimated true annual basal area growth (cm)') + 
    xlab('Observed annual basal area growth (cm)') +
    response_plot_theme()
  
  multiplot(p1, p3, p2, p4, cols=2)
}

# Plot mean CV log likelihoods for functional comparison
plot_functional_form_comparison <- function(log_likelihood_summary) {
  dat <- filter(log_likelihood_summary, model %in% c('null_model','no_gamma_model', 
                                                     'rho_combinations', 'null_model_random_effects', 
                                                     'no_gamma_model_random_effects', 'growth_comparison') & 
                  likelihood =='sum_log_lik_heldout' & 
                  rho_combo=='' & 
                  growth_measure=='true_dbh_dt') %>%
    mutate(model = factor(model, levels=c('null_model','no_gamma_model', 
                                          'rho_combinations', 'null_model_random_effects', 
                                          'no_gamma_model_random_effects', 'growth_comparison'))) %>%
    mutate(model = factor(model, labels = c( "Baseline hazard","Growth hazard","Baseline & growth hazard",
                                             "Baseline hazard (spp)","Growth hazard (spp)",
                                             "Baseline & growth hazard (spp)"))) %>%
    arrange(model)
  
  ggplot(dat, aes(x = elpd,y = model)) + 
    geom_segment(aes(x=lower_ci,y=model, xend=upper_ci, yend=model), size=0.5)+
    geom_point(aes(x=elpd, y=model), size =3) +
    xlab('Log likelihood') +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    coefficent_plot_theme()
}

# Plot mean CV log likelihoods for growth comparison
plot_growth_comparison <- function(log_likelihood_summary) {
  dat <- filter(log_likelihood_summary, model =='growth_comparison' & 
                  likelihood =='sum_log_lik_heldout' & 
                  rho_combo=='') %>%
    mutate(growth_measure = factor(growth_measure, levels=c("true_basal_area_dt","true_dbh_dt"))) %>%
    mutate(growth_measure = factor(growth_measure, labels = c("Area growth","Increment growth"))) %>%
    arrange(growth_measure)
  
  ggplot(dat, aes(x = elpd,y = growth_measure)) + 
    geom_segment(aes(x=lower_ci,y=growth_measure, xend=upper_ci, yend=growth_measure), size=0.5)+
    geom_point(aes(x=elpd, y=growth_measure), size =3) +
    xlab('Log likelihood') +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    coefficent_plot_theme()
}

# Plot mean CV log likelihoods for rho combination comparison
plot_rho_comparison <- function(log_likelihood_summary) {
  dat <- filter(log_likelihood_summary, model =='rho_combinations' & 
                  likelihood =='sum_log_lik_heldout') %>%
    mutate(rho_combo = factor(rho_combo, levels=c("abc","bc","ac","ab","c","b","a",""))) %>%
    mutate(rho_combo = factor(rho_combo, labels = c("all parameters","beta & gamma","alpha & gamma",'alpha & beta', "gamma", "beta","alpha","none"))) %>%
    arrange(rho_combo)
  
  ggplot(dat, aes(x = elpd,y = rho_combo)) + 
    geom_segment(aes(x=lower_ci,y=rho_combo, xend=upper_ci, yend=rho_combo), size=0.5)+
    geom_point(aes(x=elpd, y=rho_combo), size =3) +
    xlab('Log likelihood') +
    scale_x_continuous(breaks= scales::pretty_breaks(6)) +
    coefficent_plot_theme()
}