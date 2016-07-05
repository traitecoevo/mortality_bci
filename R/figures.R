# Plot observed vs predicted true dbh and growth
plot_obs_v_pred_growth <- function(data) {
  
  p1 <- ggplot(data = data,  aes(x = dbh_prev, y = true_dbh1)) + 
    geom_point(alpha = 0.4, size= 0.2) +
    geom_abline(intercept = 0, slope = 1,col='red', size= 0.2) +
    scale_x_continuous(expand=c(0,0), limit=c(NA, 160)) +
    scale_y_continuous(expand=c(0,0), limit=c(0, 160)) +
    ylab(expression(widehat("DBH"["t1"])~(cm))) + 
    xlab("DBH"["t1"]~("cm")) +
    partial_plot_theme()
  
  p2 <- ggplot(data = data,  aes(x = dbh, y = true_dbh2)) + 
    geom_point(alpha = 0.4,size= 0.2) +
    geom_abline(intercept = 0, slope = 1,col='red', size= 0.2) +
    scale_x_continuous(expand=c(0,0), limit=c(NA, 160)) +
    scale_y_continuous(expand=c(0,0), limit=c(0, 160)) +
    ylab(expression(widehat("DBH"["t2"])~(cm))) + 
    xlab("DBH"["t2"]~("cm")) +
    partial_plot_theme()
  
  p3 <- ggplot(data = data,  aes(x = obs_dbh_dt, y = true_dbh_dt)) + 
    geom_point(alpha = 0.4, size= 0.2) +
    geom_abline(intercept = 0, slope = 1, col='red', size= 0.2) +
    scale_x_continuous(expand=c(0,0), limit=c(NA, 5)) +
    scale_y_continuous(expand=c(0,0), limit=c(0, 5)) +
    ylab(expression(widehat("DBH growth")~(cm/yr))) + 
    xlab('DBH growth (cm/yr)') +
    partial_plot_theme()
  
  p4 <- ggplot(data = data,  aes(x = obs_basal_area_dt, y = true_basal_area_dt)) + 
    geom_point(alpha = 0.4, size= 0.2) +
    geom_abline(intercept = 0, slope = 1, col='red', size= 0.2) +
    scale_x_continuous(expand=c(0,0), limit=c(NA, 300)) +
    scale_y_continuous(expand=c(0,0), limit=c(0, 300)) +
    ylab(expression(widehat("Basal area growth")~(cm^2/yr))) + 
    xlab(expression("Basal area growth"~(cm^2/yr))) +
    partial_plot_theme()
  
  plot_grid(p1, p2, p3, p4, ncol=2, labels=LETTERS[1:4], label_size = 7)
}

# Plot species predicted mortality v growth curves
plot_spp_curves <- function(model, data, growth_range= c(0.03,0.5), hazard_curve = FALSE, 
                            ylab="Annual mortality probability", xlab="Annual mortality probability") {
  preds <- predict_spp_hazard(model, data, growth_range)
  
  breaks <- c(0.0001,0.001,0.01,0.1, 1, 10, 100)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  
  if(hazard_curve ==FALSE) {
    ggplot(preds, aes(x = dbh_growth,y = annual_prob_mort, group = sp, colour = wood_density)) + 
      geom_line(alpha=0.4, size=0.3) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), limits = c(0, NA)) +
      ylab(ylab) +
      xlab(xlab) +
      scale_colour_gradient(expression("wood density"~("g/cm"^3)),low="blue", high="red") +
      partial_plot_theme()
  }
  else {
    ggplot(preds, aes(x = dbh_growth,y = inst_hazard, group = sp, colour = wood_density)) + 
      geom_line(alpha=0.4, size=0.2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_log10(breaks= breaks, labels = labels, limits=c(0.001,100)) +
      ylab(ylab) +
      xlab(xlab) +
      scale_colour_gradient(expression("wood density"~(g/cm^3)),low="blue", high="red") +
      partial_plot_theme()
  }
}

# Plot average species curve
plot_mu_curves <- function(model,wood_density=c(0.3,0.8), growth_range = c(0.03,0.5), hazard_curve = FALSE, 
                           ylab="Annual mortality probability", xlab="Annual dbh growth (cm)") {
  preds <- predict_mu_hazards(model,wood_density, growth_range, hazard_curve)
  
  breaks <- c(0.0001,0.001,0.01,0.1, 1, 10, 100)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  
  p1 <- ggplot(preds, aes(x = dbh_growth,y = mean, group = type, colour = wood_density, fill=wood_density)) + 
    geom_line(size=0.3) +
    scale_color_gradient(name=expression("wood density"~(g/cm^3)),limits=c(0.197,NA),low='blue',high='red') +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) +
    scale_fill_gradient(expression("wood density"~(g/cm^3)),limits=c(0.197,NA),low='blue',high='red') +
    scale_x_continuous(expand=c(0,0)) +
    xlab(xlab)
  
  if(hazard_curve ==FALSE) {
    p1 + ylab(ylab) + 
      scale_y_continuous(expand=c(0,0), limits=c(0,1)) + 
      partial_plot_theme()
  }
  else {
    p1 + ylab(ylab) + 
      scale_y_log10(breaks= breaks, labels = labels, limits=c(0.001,100)) +
      partial_plot_theme()
  }
}

# Plot proportions of predicted variance explained by wood density, species and census
plot_param_prop_explained <- function(param_prop_explained, ylab=NULL, xlab=NULL) {
  dat <- filter(param_prop_explained, !param %in% c("baseline", "growth_dependent")) %>% droplevels() %>%
    mutate(param = factor(param, levels=c("wood_density","species","census")),
           level = ifelse(param %in% c("wood_density","species"), "species","census"),
           level = factor(level, levels=c("species","census")))
  ggplot(dat, aes(x=level, y=proportion)) +
    geom_bar(stat='identity', aes(fill=param), width=0.5) +
    scale_x_discrete(labels=c('species','census')) +
    scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
    scale_fill_manual(values = c("wood_density" ="darkgrey","species" ="black", "census"="black")) + 
    ylab(ylab) +
    xlab(xlab) +
    partial_plot_theme(legend.position="none") 
}

# Plot proportions of predicted variance explained by baseline and growth dependent hazards
plot_base_v_growth_prop_explained <- function(param_prop_explained, ylab=NULL, xlab=NULL) {
  dat <- filter(param_prop_explained, param %in% c("baseline", "growth_dependent")) %>% droplevels()
  ggplot(dat, aes(x=param, y=proportion)) +
    geom_bar(stat='identity', fill='black', width=0.5) +
    scale_x_discrete(labels=c('growth dependent','baseline')) +
    scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
    ylab(ylab) +
    xlab(xlab) +
    partial_plot_theme() 
}

# Plot log loss curve
logloss_curve <- function() {
  df <- data.frame(x = seq(0.001,1,length.out = 100))
  logloss <- function(x, eps = 1e-15) {
    predicted = pmin(pmax(x, eps), 1-eps)
    - (1 * log(predicted) + (1 - 1) * log(1 - predicted))
  }
  ggplot(df, aes(x)) +
    stat_function(fun = logloss) +
    xlab("Predicted probability") +
    ylab('Logarithmic loss') +
    partial_plot_theme()
}

# Map of plot gap index with recruits overlayed for 1985 to 1990 and 1990 to 1995
plot_recruits_gapindex <- function(gap_index_raster, recruit_gap_conditions) {
  #pdf(f <- tempfile(fileext = '.pdf'), 4.5, 4.5)
  print(rasterVis::levelplot(raster::stack(gap_index_raster), 
                       names.attr=paste("Recruits from", names(gap_index_raster)),
                       layout=c(1, 2), 
                       colorkey=list(height=0.6), 
                       scales=list(alternating=FALSE, tck=c(0.5, 0.5), 
                                   x=list(at=seq(0, 1000, 200)),
                                   y=list(at=seq(0, 400, 100))),
                       col.regions=viridisLite::viridis(100),
                       at=seq(0,1,0.1), 
                       par.settings = list(fontsize = list(text = 7), 
                                           strip.background=list(col='white'),
                                           strip.border=list(col='transparent'))) +
          latticeExtra::layer(sp::sp.points(SpatialPoints(recruit_gap_conditions[[panel.number()]]), 
                                        pch='.', col='black', alpha=0.4), 
                              data=list(recruit_gap_conditions=recruit_gap_conditions)))
  
}

# Plot other covariates by parameter
plot_spp_param_by_covariate <- function(data, focal_param, covariate ="mean_gap_index", ylab =NULL, xlab = NULL) {
  spp <- data[[focal_param]] %>%
    select_('sp', covariate, 'mean', '`2.5%`','`97.5%`') %>%
    filter(complete.cases(.))
  
  # Sets limit for beta in order to produce nice log axis
  if (focal_param =='beta') {
    ylim <- c(1,100)
  } else {
    ylim = NULL
  }
  
  breaks <- c(0.001,0.01,0.1, 1, 10, 100)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  
  p1 <- ggplot(spp, aes_string(x = covariate,y = "mean")) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), size=0.1, shape= 16) +
    geom_point(shape= 21, fill='red', size=0.6) +
    geom_smooth(method='lm', size=0.5, alpha =0.7, se=FALSE) +
    partial_plot_theme() +
    scale_y_log10(breaks= breaks, labels = labels, limits=ylim) +
    ylab(ylab) +
    xlab(xlab)
  
  if(covariate =="dbh_95") {
   fit <- data.frame(r2 = summary(lm(log10(mean)~log10(get(covariate)), data = data[[focal_param]]))$r.squared)
    p1 + scale_x_log10(breaks= breaks) +
      annotate('text',x=Inf,y=0, label=paste("r2 =", signif(fit$r2,1)), vjust=-0.7, hjust=1, size=2)
  }
  else {
    fit <- data.frame(r2 = summary(lm(log10(mean)~get(covariate), data = data[[focal_param]]))$r.squared)
    p1 + annotate('text',x=Inf,y=0, label=paste("r2 =", signif(fit$r2,2)), vjust=-0.7, hjust=1, size=2)
  }
}

# Manuscript figures

plot_fig1 <- function(tree1, tree2, panelc) {
  growth_haz <- function(x) {
    1-exp(-0.15 * exp(-10 * x))
  }
  
  base_growth_haz <- function(x) {
    1-exp(-(0.13 * exp(-10 * x) + 0.03))
  }
  
  my_label <- function(text, x=-0.1) label(x, 1.3, text, cex=1.5)
  
  layout(matrix(c(1,1,1,2,3,4,5,5,5), byrow=TRUE, ncol=3))
  par(mar=c(2,2,3,1), oma=c(1,1,1,1))
  
  # Tree growth diagram
  
  plot(1:2, type='n', ann=FALSE, axes=FALSE, xlim=c(-1,1), ylim=c(-1,1))
  my_label("A) Repeat census data")
  
  # note we are only plotting to last viewport, but calls to other two needed to make figure work.
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  fig.tree(tree1, tree2)
  popViewport(3)
  
  # Example baseline hazard
  empty_plot <- function(){
    plot(NA, ylim = c(0,0.14),  xlim=c(0,1), xaxs='i', axes=FALSE, ann=FALSE)
    axis(1, at = c(-1, 2), labels=NA, lwd=1.5)
    axis(2, at = c(-1, 2),labels=NA, lwd=1.5)
  }
  
  hazard_plot <- function(f, text){
    x <- seq(0,1,length.out = 50)
    empty_plot()
    lines(x, f(x), type='l', lwd=2, col="red")
    text(0.1, 0.12, text, cex=1.5, pos=4, xpd=NA)
  }
  
  hazard_plot(function(x) 0*x +0.03, expression(gamma))
  my_label("B) Alternative mortality functions", x=-0.45)
  mtext("Mortality rate", 2, line=1, cex=0.75)
  
  # Example growth-dependent hazard
  hazard_plot(growth_haz, expression(alpha*"e"^{-beta~"X"}))
  mtext("Growth rate", 1, line =1, cex=0.75)
  # Example full baseline + growth-dependent hazard
  hazard_plot(base_growth_haz, expression(alpha*"e"^{-beta~"X"}+gamma))
  
  plot(NA, xlim = c(0, 1), ylim= c(0, 1), ann = FALSE, axes=FALSE)
  my_label("C) 10-fold cross validation")
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  img <- readPNG(panelc)
  grid.raster(img, unit(0.4, "npc"),  y = unit(0, "npc"),
              just=c("bottom"), height=unit(1, "npc"))
  
  popViewport(3)
  text(1, 0.65,  expression(paste(bar("L"), "=")), cex=1.25, xpd=NA)
  text(1, 0.35,  expression(sum("L"[i])/10), cex=1.25, xpd=NA)
  
}

fig.tree <- function(file_alive, file_dead) {
  
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

plot_fig2a <- function(logloss_summaries) {
  dat <- logloss_summaries %>%
    filter(model_type == "rho_combinations_base_growth_hazard_c" |
             comparison %in% c("null_model","function_growth_comparison","species_random_effects")) %>%
    mutate(comparison = replace(comparison, comparison =="function_growth_comparison" & model=="base_hazard", "census"),
      comparison = factor(comparison, levels=c('null_model','census','function_growth_comparison','rho_combinations','species_random_effects'),
                               labels = c('null','census','function form','wd','species')))
  
  ggplot(dat, aes(x = model_type,y = mean, group = growth_measure, fill=growth_measure, shape = model)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(1), stroke = 0.5, size=0.4) +
    ylab('Logarithmic Loss') + 
    xlab('Model') +
    scale_shape_manual(values = c(21, 24, 22)) +
    scale_fill_manual(values =c('white','grey80','black')) +
    scale_y_continuous(breaks= scales::pretty_breaks(5)) +
    scale_x_discrete(labels=c("null_model_base_hazard_none" = expression(gamma),
                              "function_growth_comparison_base_hazard_none" =  expression(gamma~delta["t"]),
                              "function_growth_comparison_growth_hazard_none" = expression((alpha*"e"^{-beta~"X"["i"]})~delta["t"]),
                              "function_growth_comparison_base_growth_hazard_none" = expression((alpha*"e"^{-beta~"X"["i"]} + gamma)~delta["t"]),
                              "rho_combinations_base_growth_hazard_c" = expression((alpha*"e"^{-beta~"X"["i"]} + gamma*rho["s[i]"]^gamma[1])~delta["t"]),
                              "species_random_effects_base_growth_hazard_none" = expression((alpha[s]*"e"^{-beta[s]~"X"["i"]} + gamma[s])~delta["t"]))) +
    facet_grid(.~comparison, scales='free_x', drop=TRUE, space = "free_x") +
    partial_plot_theme(strips = TRUE) +
    theme(axis.text.x = element_text(angle=15, hjust = 1)) 
}

plot_fig2b <- function(logloss_summaries) {
  dat <- logloss_summaries %>%
    filter(growth_measure == "true_dbh_dt" &
             (comparison == "rho_combinations" |
                model_type == "function_growth_comparison_base_growth_hazard_none"))
  
  ggplot(dat, aes(x = model_type,y = mean)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), shape=22, fill='black', stroke = 0.5,size=0.4) +
    ylab('Logarithmic Loss') + 
    xlab(expression('Wood density effects on'~(alpha*"e"^{-beta~"X"["i"]} + gamma)~delta["t"])) +
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

plot_fig2 <- function(logloss_summaries) {
  p1 <- plot_fig2a(logloss_summaries)
  p2 <- plot_fig2b(logloss_summaries)
  plot_grid(p1,p2, ncol=1, labels=LETTERS[1:2], label_size = 7)
}

# Plot gamma vs wood density with mean trend line
plot_fig3 <- function(spp_params_covs, pred_mu_basehaz) {
  spp <- spp_params_covs[["gamma"]]
  
  breaks <- c(0.001,0.01,0.1, 1, 10)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  
  ggplot(spp, aes(x = wood_density,y = mean)) + 
    geom_ribbon(data = pred_mu_basehaz, aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.5, colour=NA) +
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), size=0.25, shape= 16) +
    geom_point(shape= 21, fill='red') +
    geom_line(data = pred_mu_basehaz, aes(x = wood_density, y= mean), col='lightgrey') +
    partial_plot_theme() +
    scale_y_log10(breaks= breaks, labels = labels) +
    ylab(expression("Baseline mortality rate"~(gamma))) +
    xlab(expression("wood density"~("g/cm"^3)))
}

plot_fig4 <- function(model, data) {
  p1 <- plot_spp_curves(model, data, hazard_curve = TRUE, xlab=NULL, ylab="Mortality rate")
  p2 <- plot_mu_curves(model, hazard_curve= TRUE, ylab=NULL, xlab=NULL) +
    theme(legend.position= c(0.8,0.8),
          legend.key.size =unit(0.25, "cm"), 
          legend.title=element_text(size=4),
          legend.text = element_text(size=4),
          legend.title.align =0.75)
  
  p3 <- plot_spp_curves(model, data, hazard_curve = FALSE)
  p4 <- plot_mu_curves(model, hazard_curve= FALSE, ylab=NULL)
  
  plot_grid(p1,p2,p3,p4, ncol=2, labels=LETTERS[1:4], label_size = 7)
}

# Proportion of variance explained
plot_fig5 <- function(param_prop_explained) {
  p1 <- plot_param_prop_explained(param_prop_explained, ylab="Proportion of variation explained")
  p2 <- plot_base_v_growth_prop_explained(param_prop_explained)
  plot_grid(p1,p2, ncol=2, labels=LETTERS[1:2], label_size = 7)
}

# params vs other covariates
plot_fig6 <- function(data) {
p1 <- plot_spp_param_by_covariate(data, "alpha", "mean_gap_index",ylab = expression("Low growth effect"~(alpha)), xlab =NULL) + ggtitle('Light requirement') 
p2 <- plot_spp_param_by_covariate(data, "alpha", "dbh_95",ylab = NULL, xlab =NULL) + ggtitle('Maximum size') 
p3 <- plot_spp_param_by_covariate(data, "gamma", "mean_gap_index",ylab = expression("Baseline mortality"~(gamma)), xlab =NULL)
p4 <- plot_spp_param_by_covariate(data, "gamma", "dbh_95",ylab = NULL, xlab = NULL)
p5 <- plot_spp_param_by_covariate(data, "alpha_gamma","mean_gap_index",ylab = expression("Low growth mortality"~(alpha+gamma)), xlab ='Gap index')
p6 <- plot_spp_param_by_covariate(data, "alpha_gamma", "dbh_95",ylab = NULL, xlab = NULL)
p7 <- plot_spp_param_by_covariate(data, "beta", "mean_gap_index",ylab = expression("Exponential decay"~(beta)), xlab ='Gap index')
p8 <- plot_spp_param_by_covariate(data, "beta", "dbh_95",ylab = NULL, xlab =expression('DBH'['max']~(cm)))
plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, ncol=2, labels=LETTERS[1:8], label_size = 7)
}
