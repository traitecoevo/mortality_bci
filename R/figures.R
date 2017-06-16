# Manuscript figures & sub functions

# PLOTTING THEME
# Plot theme
plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text() else element_blank()
  theme_classic(base_size = 7) + theme(strip.text = st,
                                       strip.background = sb,
                                       legend.position = legend.position,
                                       axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                                       axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                                       plot.margin = unit(c(3,3,3,3), "mm"))
}


#### FIGURE 1 ######

plot_fig1 <- function(tree1, tree2, panelc) {
  
  # Example base only hazard function
  base_only_haz <- function(x) { 
    1-exp(-(0*x +0.03))
  }
  
  # Example growth hazard function
  growth_haz <- function(x) {
    1-exp(-0.15 * exp(-10 * x))
  }
  
  # Example base-growth hazard function
  base_growth_haz <- function(x) {
    1-exp(-(0.13 * exp(-10 * x) + 0.03))
  }
  
  # Function to add labels to panels
  my_label <- function(text, x=-0.1) label(x, 1.3, text)
  
  # Plot layout
  layout(matrix(c(1,1,1,2,3,4,5,5,5,6,6,6), byrow=TRUE, ncol=3))
  par(mar=c(1,1,2.5,1), oma=c(1,1,1,1), cex=0.5)
  
  # Tree growth diagram
  plot(1:2, type='n', ann=FALSE, axes=FALSE, xlim=c(-1,1), ylim=c(-1,1))
  my_label("A) Repeat census data", x=-0.06)
  
  # note we are only plotting to last viewport, but calls to other two needed to make figure work.
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  fig.tree(tree1, tree2)
  popViewport(3)
  
  # Produce empty plot
  empty_plot <- function(){
    par(mar=c(2,2,3,1))
    plot(NA, ylim = c(0,0.14),  xlim=c(0,1), xaxs='i', axes=FALSE, ann=FALSE)
    axis(1, at = c(-1, 2), labels=NA)
    axis(2, at = c(-1, 2),labels=NA)
  }
  
  # Plot hazard curves
  hazard_plot <- function(f, text){
    x <- seq(0,1,length.out = 50)
    empty_plot()
    lines(x, f(x), type='l', col="red")
    text(0.1, 0.12, text, pos=4, xpd=NA)
  }
  
  # Plot base hazard
  hazard_plot(base_only_haz, expression(gamma))
  my_label("B) Alternative mortality functions", x=-0.36)
  mtext("Mortality rate", 2, line=1, cex=0.5)
  
  # Plot growth-dependent hazard
  hazard_plot(growth_haz, expression(alpha*"e"^{-beta~"X"}))
  mtext("Growth rate", 1, line =1, cex=0.5)
  
  # Example full baseline + growth-dependent hazard
  hazard_plot(base_growth_haz, expression(alpha*"e"^{-beta~"X"}+gamma))
  
  # Plot logloss curve
  par(mar=c(2.5,3,2,1))
  plot(NA, xlim = c(0, 1), ylim= c(0, 5), ann = FALSE, las=1)
  p <- seq(0,1, length.out=200)
  lines(p, -log(1-p), type='l', col="red")
  my_label("C) Penalty for incorrect prediction", x=-0.137)
  mtext("Probability of outcome", 1, line=2.1, cex=0.5)
  mtext("log loss", 2, line=2.1, cex=0.5)
  
  # Plot cross validation procedure
  par(mar=c(0.1,0.1,3,0.1))
  plot(NA, xlim = c(0, 1), ylim= c(0, 1), ann = FALSE, axes=FALSE)
  my_label("D) 10-fold cross validation", x =-0.03)
  
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)
  
  img <- readPNG(panelc)
  grid.raster(img, unit(0.4, "npc"),  y = unit(0, "npc"),
              just=c("bottom"), height=unit(1, "npc"))
  
  popViewport(3)
  text(0.85, 0.45,  expression(paste(bar("logloss"), " =", sum("logloss"[i])/10)), xpd=NA)
}

# FIGURE 1 sub functions #

# Tree plot
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

# position label at a fractional x/y position on a plot
label <- function(px, py, lab, ..., adj = c(0, 1)) {
  usr <- par("usr")
  x <- usr[1] + px * (usr[2] - usr[1])
  y <- usr[3] + py * (usr[4] - usr[3])
  
  if (par("ylog"))
    y <- 10^y
  if (par("xlog"))
    x <- 10^x
  
  text(x, y, lab, adj = adj, xpd=NA, ...)
}

#### FIGURE 2 ######
plot_fig2 <- function(logloss_summaries) {
  p1 <- plot_fig2a(logloss_summaries)
  p2 <- plot_fig2b(logloss_summaries)
  plot_grid(p1,p2, ncol=1, labels=LETTERS[1:2], label_size = 7)
}

## FIGURE 2 panel functions ##

plot_fig2a <- function(logloss_summaries) {
  dat <- logloss_summaries %>%
    filter(model_type == "rho_combinations_base_growth_hazard_c" |
             comparison %in% c("null_model","function_growth_comparison","species_random_effects")) %>%
    mutate(comparison = replace(comparison, comparison =="function_growth_comparison" & model=="base_hazard", "census"),
           comparison = factor(comparison, levels=c('null_model','census','function_growth_comparison','rho_combinations','species_random_effects'),
                               labels = c('Null','Census','Growth rate','WD','Species')))
  
  ggplot(dat, aes(x = model_type,y = mean, group = growth_measure, fill=growth_measure, shape = model)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(0.5), stroke = 0.5, size=0.4) +
    ylab('Logarithmic loss') + 
    xlab('Hazard function') +
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
    plot_theme(strips = TRUE) +
    theme(axis.text.x = element_text(angle=15, hjust = 1)) 
}

plot_fig2b <- function(logloss_summaries) {
  dat <- logloss_summaries %>%
    filter(growth_measure == "true_dbh_dt" &
             (comparison == "rho_combinations" |
                model_type == "function_growth_comparison_base_growth_hazard_none"))
  
  ggplot(dat, aes(x = model_type,y = mean)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), shape=22, fill='black', stroke = 0.5,size=0.4) +
    ylab('Logarithmic loss') + 
    xlab(expression('Wood density effects in the model'~(alpha*"e"^{-beta~"X"["i"]} + gamma)~delta["t"])) +
    scale_x_discrete(labels=c("function_growth_comparison_base_growth_hazard_none" = "none",
                              "rho_combinations_base_growth_hazard_a" = expression(alpha),
                              "rho_combinations_base_growth_hazard_b" = expression(beta),
                              "rho_combinations_base_growth_hazard_ab" = expression(alpha~"&"~beta),
                              "rho_combinations_base_growth_hazard_c" = expression(gamma),
                              "rho_combinations_base_growth_hazard_ac" = expression(alpha~"&"~gamma),
                              "rho_combinations_base_growth_hazard_bc" = expression(beta~"&"~gamma),
                              "rho_combinations_base_growth_hazard_abc" = expression(alpha~","~beta~","~gamma))) +
    plot_theme()
}

#### FIGURE 3 ######
# Plot gamma vs wood density with mean trend line
plot_fig3 <- function(spp_params_covs, pred_mu_basehaz) {
  spp <- spp_params_covs[["gamma"]]
  
  breaks <- c(0.001,0.01,0.1, 1, 10)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  
  ggplot(spp, aes(x = wood_density,y = mean)) + 
    geom_ribbon(data = pred_mu_basehaz, aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.5, colour=NA) +
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), size=0.25, shape= 16) +
    geom_point(shape= 21, fill='red') +
    geom_line(data = pred_mu_basehaz, aes(x = wood_density, y= mean), col='blue') +
    plot_theme() +
    scale_y_log10(breaks= breaks, labels = labels) +
    ylab(expression("Growth-independent mortality rate,"~(gamma))) +
    xlab(expression("Wood density"~("g cm"^-3)))
}
#### FIGURE 4 ######
# Show predicted species and mean curves.
plot_fig4 <- function(model, data) {
  p1 <- plot_fig4a(model, data, hazard_curve = TRUE, xlab=NULL, ylab=expression("Mortality rate"~("yr"^-1)))
  p2 <- plot_fig4b(model, hazard_curve= TRUE, ylab=NULL, xlab=NULL) +
    theme(legend.position= c(0.8,0.8),
          legend.key.size =unit(0.25, "cm"), 
          legend.title=element_text(size=4),
          legend.text = element_text(size=4),
          legend.title.align =0.75)
  
  p3 <- plot_fig4a(model, data, hazard_curve = FALSE)
  p4 <- plot_fig4b(model, hazard_curve= FALSE, ylab=NULL)
  
  plot_grid(p1,p2,p3,p4, ncol=2, labels=LETTERS[1:4], label_size = 7)
}

# FIGURE 4 panel functions#

# Plot species predicted mortality v growth curves (Fig4a)
plot_fig4a <- function(model, data, growth_range= c(0.03,0.5), hazard_curve = FALSE, 
                       ylab="Annual mortality probability", xlab=expression("Annual dbh growth"~("cm yr"^-1))) {
  
  
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
      scale_colour_gradient(expression("Wood density"~("g cm"^-3)),low="blue", high="red") +
      plot_theme()
  }
  else {
    ggplot(preds, aes(x = dbh_growth,y = inst_hazard, group = sp, colour = wood_density)) + 
      geom_line(alpha=0.4, size=0.2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_log10(breaks= breaks, labels = labels, limits=c(0.001,100)) +
      ylab(ylab) +
      xlab(xlab) +
      scale_colour_gradient(expression("Wood density"~("g cm"^-3)),low="blue", high="red") +
      plot_theme()
  }
}

# Plot average species curve (Fig4b)
plot_fig4b <- function(model,wood_density=c(0.3,0.8), growth_range = c(0.03,0.5), hazard_curve = FALSE, 
                       ylab="Annual mortality probability", xlab=expression("Annual dbh growth"~("cm yr"^-1))) {
  preds <- predict_mu_hazards(model,wood_density, growth_range, hazard_curve)
  
  breaks <- c(0.0001,0.001,0.01,0.1, 1, 10, 100)
  labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))
  
  p1 <- ggplot(preds, aes(x = dbh_growth,y = mean, group = type, colour = wood_density, fill=wood_density)) + 
    geom_line(size=0.3) +
    scale_color_gradient(name=expression("Wood density"~("g cm"^-3)),limits=c(0.197,NA),low='blue',high='red') +
    geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.4, colour=NA) +
    scale_fill_gradient(expression("Wood density"~("g cm"^-3)),limits=c(0.197,NA),low='blue',high='red') +
    scale_x_continuous(expand=c(0,0)) +
    xlab(xlab)
  
  if(hazard_curve ==FALSE) {
    p1 + ylab(ylab) + 
      scale_y_continuous(expand=c(0,0), limits=c(0,1)) + 
      plot_theme()
  }
  else {
    p1 + ylab(ylab) + 
      scale_y_log10(breaks= breaks, labels = labels, limits=c(0.001,100)) +
      plot_theme()
  }
}

#### FIGURE 5 ######
# Proportion of variance explained
plot_fig5 <- function(param_prop_explained) {
  p1 <- plot_fig5a(param_prop_explained, ylab="Proportion of variation")
  p2 <- plot_fig5b(param_prop_explained)
  plot_grid(p1,p2, ncol=2, labels=LETTERS[1:2], label_size = 7)
}

# FIG 5 PANEL FUNCTIONS #
# Plot proportions of predicted variance explained by wood density, species and census (fig5a)
plot_fig5a <- function(param_prop_explained, ylab=NULL, xlab=NULL) {
  dat <- filter(param_prop_explained, !param %in% c("baseline", "growth_dependent")) %>% droplevels() %>%
    mutate(param = factor(param, levels=c("wood_density","species","census")),
           level = ifelse(param %in% c("wood_density","species"), "species","census"),
           level = factor(level, levels=c("species","census")))
  ggplot(dat, aes(x=level, y=proportion)) +
    geom_bar(stat='identity', aes(fill=param), width=0.5) +
    scale_x_discrete(labels=c('species\n','census\n')) +
    scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
    scale_fill_manual(values = c("wood_density" ="darkgrey","species" ="black", "census"="black")) + 
    ylab(ylab) +
    xlab(xlab) +
    plot_theme(legend.position="none") 
}

# Plot proportions of predicted variance explained by baseline and growth dependent hazards
plot_fig5b <- function(param_prop_explained, ylab=NULL, xlab=NULL) {
  dat <- filter(param_prop_explained, param %in% c("baseline", "growth_dependent")) %>% droplevels()
  ggplot(dat, aes(x=param, y=proportion)) +
    geom_bar(stat='identity', fill='black', width=0.5) +
    scale_x_discrete(labels=c('growth-\ndependent','growth-\nindependent')) +
    scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
    ylab(ylab) +
    xlab(xlab) +
    plot_theme() 
}

# params vs other covariates
plot_fig6 <- function(data) {
  p1 <- plot_spp_param_by_covariate(data, "alpha", "mean_gap_index",ylab = expression("Low growth effect"~(alpha)), xlab =NULL) + ggtitle('Light requirement') 
  p2 <- plot_spp_param_by_covariate(data, "alpha", "dbh_95",ylab = NULL, xlab =NULL) + ggtitle('Maximum size') 
  p3 <- plot_spp_param_by_covariate(data, "gamma", "mean_gap_index",ylab = expression("Baseline mortality"~(gamma)), xlab =NULL)
  p4 <- plot_spp_param_by_covariate(data, "gamma", "dbh_95",ylab = NULL, xlab = NULL)
  p5 <- plot_spp_param_by_covariate(data, "alpha_gamma","mean_gap_index",ylab = expression("Low growth mortality"~(alpha+gamma)), xlab = NULL)
  p6 <- plot_spp_param_by_covariate(data, "alpha_gamma", "dbh_95",ylab = NULL, xlab = NULL)
  p7 <- plot_spp_param_by_covariate(data, "beta", "mean_gap_index",ylab = expression("Exponential decay"~(beta)), xlab ='Gap index')
  p8 <- plot_spp_param_by_covariate(data, "beta", "dbh_95",ylab = NULL, xlab =expression('DBH'['max']~(cm)))
  plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, ncol=2, labels=LETTERS[1:8], label_size = 7)
}


#FIG 6 PANEL FUNCTIONS #

# Plot other covariates by parameter
plot_spp_param_by_covariate <- function(data, focal_param, covariate ="mean_gap_index", ylab =NULL, xlab = NULL) {
  spp <- data[[focal_param]] %>%
    select_('sp', covariate, 'mean', '`2.5%`','`97.5%`') %>%
    filter(complete.cases(.))
  
  # Sets limit for beta in order to produce nice log axis
  if (focal_param =='beta') {
    ylim <- c(5,50)
    breaks <- c(5,10, 20, 40)
    labels <- breaks
  } else {
    ylim = c(5E-4, 2E1)
    breaks <- c(0.001,0.01,0.1, 1, 10, 100)
    labels <- sapply(log10(breaks), function(i) as.expression(bquote(10^ .(i))))
  }
  
  xbreaks <- c(0.001,0.01,0.1, 1, 10, 100)
  
  spp <- mutate(spp, 
                `2.5%`=ifelse(`2.5%` < ylim[1], ylim[1], `2.5%`),
                `97.5%`=ifelse(`97.5%` > ylim[2], ylim[2], `97.5%`))
  
  p1 <- ggplot(spp, aes_string(x = covariate,y = "mean")) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), size=0.1, shape= 16) +
    geom_point(shape= 21, fill='red', size=0.6) +
    geom_smooth(method='lm', size=0.5, alpha =0.7, se=FALSE) +
    plot_theme() +
    scale_y_log10(breaks= breaks, labels = labels, limits=ylim) +
    ylab(ylab) +
    xlab(xlab)
  
  if(covariate =="dbh_95") {
    fit <- data.frame(r2 = summary(lm(log10(mean)~log10(get(covariate)), data = data[[focal_param]]))$r.squared)
    p1 + scale_x_log10(breaks= xbreaks) +
                       annotate('text',x=Inf,y=0,label=paste("r2 =", signif(fit$r2,1)), vjust=-0.7, hjust=1, size=2)
  }
  else {
    fit <- data.frame(r2 = summary(lm(log10(mean)~get(covariate), data = data[[focal_param]]))$r.squared)
    p1 + annotate('text',x=Inf,y=0, label=paste("r2 =", signif(fit$r2,2)), vjust=-0.7, hjust=1, size=2)
  }
}

### SUPPLEMENTARY FIGURES

# Plot observed vs predicted true dbh and growth (used for figS1)
plot_figS1 <- function(data) {
  # Observed DBH @ t1 vs Prediction
  p1 <- ggplot(data = data,  aes(x = dbh_prev, y = true_dbh1)) + 
    geom_point(alpha = 0.4, size= 0.2) +
    geom_abline(intercept = 0, slope = 1,col='red', size= 0.2) +
    scale_x_continuous(expand=c(0,0), limit=c(NA, 160)) +
    scale_y_continuous(expand=c(0,0), limit=c(0, 160)) +
    ylab("Predicted DBH"["t1"]~("cm")) + 
    xlab("Observed DBH"["t1"]~("cm")) +
    plot_theme() +
    theme(axis.title=element_text(size=5.5))
  
  # Observed DBH @ t2 vs Prediction
  p2 <- ggplot(data = data,  aes(x = dbh, y = true_dbh2)) + 
    geom_point(alpha = 0.4,size= 0.2) +
    geom_abline(intercept = 0, slope = 1,col='red', size= 0.2) +
    scale_x_continuous(expand=c(0,0), limit=c(NA, 160)) +
    scale_y_continuous(expand=c(0,0), limit=c(0, 160)) +
    ylab("Predicted DBH"["t2"]~("cm")) + 
    xlab("Observed DBH"["t2"]~("cm")) +
    plot_theme() +
    theme(axis.title=element_text(size=5.5))
  
  # Observed DBH growth vs Prediction
  p3 <- ggplot(data = data,  aes(x = obs_dbh_dt, y = true_dbh_dt)) + 
    geom_point(alpha = 0.4, size= 0.2) +
    geom_abline(intercept = 0, slope = 1, col='red', size= 0.2) +
    scale_x_continuous(expand=c(0,0), limit=c(NA, 5)) +
    scale_y_continuous(expand=c(0,0), limit=c(0, 5)) +
    ylab(expression("Predicted DBH growth"~("cm yr"^-1))) +
    xlab(expression("Observed DBH growth"~("cm yr"^-1))) +
    plot_theme() +
    theme(axis.title=element_text(size=5.5))
  
  # Observed basal area growth vs Prediction
  p4 <- ggplot(data = data,  aes(x = obs_basal_area_dt, y = true_basal_area_dt)) + 
    geom_point(alpha = 0.4, size= 0.2) +
    geom_abline(intercept = 0, slope = 1, col='red', size= 0.2) +
    scale_x_continuous(expand=c(0,0), limit=c(NA, 300)) +
    scale_y_continuous(expand=c(0,0), limit=c(0, 300)) +
    ylab(expression("Predicted basal area growth"~("cm"^2~"yr"^-1))) +
    xlab(expression("Observed basal area growth"~("cm"^2~"yr"^-1))) +
    plot_theme() +
    theme(axis.title=element_text(size=5.5))
  
  plot_grid(p1, p2, p3, p4, ncol=2, labels=LETTERS[1:4], label_size = 7)
}

# Map of plot gap index with recruits overlayed for 1985 to 1990 and 1990 to 1995
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
                                            pch='+', col='red', alpha=0.4, cex=0.1),
                              data=list(recruit_gap_conditions=recruit_gap_conditions)))
  
}

# Plot species parameters (For figS3 to figS5)
plot_spp_params <- function(model, data, parameter,xlab ='Log effect size', title=NULL) {
  # Extract hyper distribution summary
  hyper <- summarise_hyper_params(model,data) %>%
    subset(., grepl(parameter, names(.))) %>%
    {
      hyper_param <- as.character(TRUE)
      species <- as.character('Hyper distribution')
      mean <- .[[paste0('mu_log_',parameter)]]$mean
      sd <- .[[paste0('sigma_log_',parameter)]]$mean
      `2.5%` <- qnorm(0.025,mean,sd)
      `97.5%` <- qnorm(0.975,mean,sd)
      cbind.data.frame(hyper_param,species,mean,sd,`2.5%`,`97.5%`, stringsAsFactors=FALSE)
    }
  # Extract species param summaries
  spp <- summarise_spp_params(model,data, TRUE) %>% 
    .[[paste0('log_',parameter)]] %>%
    mutate(hyper_param = as.character(FALSE),
           sp = as.character(sp),
           species = as.character(species))
  
  # Merge dataframes and prep for plotting
  dat <-bind_rows(hyper, spp) %>%
    mutate(
      species = as.factor(species)) %>%
    mutate(species = factor(species, levels=species[order(hyper_param,mean)], ordered=TRUE))
  
  #Plot 
  ggplot(dat, aes(x = mean,y = species, col=hyper_param)) + 
    geom_segment(aes(x=`2.5%`,y=species, xend=`97.5%`, yend=species), size=0.3) +
    geom_point(aes(x=mean, y=species), size=0.3) +
    scale_colour_manual(values = c("TRUE" ="red","FALSE" ="black")) +
    xlab(xlab) +
    ylab(NULL) +
    labs(title=title) +
    plot_theme() +
    theme(axis.text.y=element_text(size=3),
          axis.ticks = element_line(size= 0.3))
}