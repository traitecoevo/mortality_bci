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
             comparison %in% c("null_model","function_growth_comparison","species_random_effects"))
  
  ggplot(dat, aes(x = model_type,y = mean, group = growth_measure, fill=growth_measure, shape = model)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), position=position_dodge(.5)) +
    ylab('Logarithmic Loss') + 
    xlab(NULL) +
    scale_shape_manual(values = c(21, 24, 22)) +
    scale_fill_manual(values =c('white','grey80','black')) +
    scale_y_continuous(breaks= scales::pretty_breaks(5)) +
    scale_x_discrete(labels=c("null_model_base_hazard_none" = expression(gamma),
                              "function_growth_comparison_base_hazard_none" =  expression(gamma~"c"["t"]),
                              "function_growth_comparison_growth_hazard_none" = expression((alpha*"e"^{-beta~"X"["i"]})~"c"["t"]),
                              "function_growth_comparison_base_growth_hazard_none" = expression((alpha*"e"^{-beta~"X"["i"]} + gamma)~"c"["t"]),
                              "rho_combinations_base_growth_hazard_c" = expression((alpha*"e"^{-beta~"X"["i"]} + gamma*rho["s[i]"]^gamma[1])~"c"["t"]),
                              "species_random_effects_base_growth_hazard_none" = expression((alpha[s]*"e"^{-beta[s]~"X"["i"]} + gamma[s])~"c"["t"]))) +
    partial_plot_theme() +
    theme(axis.text.x = element_text(angle=45, hjust = 1)) 
  }


plot_fig2b <- function(logloss_summaries) {
  dat <- logloss_summaries %>%
    filter(growth_measure == "true_dbh_dt" &
             (comparison == "rho_combinations" |
                model_type == "function_growth_comparison_base_growth_hazard_none"))
  
  ggplot(dat, aes(x = model_type,y = mean)) + 
    geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), shape=22, fill='black') +
    ylab('Logarithmic Loss') + 
    xlab(expression('Wood density effects on'~(alpha*"e"^{-beta~"X"["i"]} + gamma)~"c"["t"])) +
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
plot_fig2 <- function(logloss_summaries) {
  p1 <- plot_fig2a(logloss_summaries)
  p2 <- plot_fig2b(logloss_summaries)
  plot_grid(p1,p2, ncol=1, labels=LETTERS[1:2], label_size = 7)
}

# Plot gamma vs wood density with mean trend line
plot_fig3 <- function(model, data) {
spp <- summarise_spp_params(model, data)$gamma
med <- predict_mu_baseline_hazard(model, data)

breaks <- c(0.001,0.01,0.1, 1, 10)
labels <- sapply(log10(breaks),function(i) as.expression(bquote(10^ .(i))))

ggplot(spp, aes(x = wood_density,y = mean)) + 
      geom_ribbon(data = med, aes(ymin = `2.5%`,ymax = `97.5%`), alpha=0.5, colour=NA) +
      geom_pointrange(aes(ymin = `2.5%`, ymax=`97.5%`), size=0.25, shape= 16) +
      geom_point(shape= 21, fill='red') +
      geom_line(data = med, aes(x = wood_density, y= mean), col='lightgrey') +
      partial_plot_theme() +
      scale_y_log10(breaks= breaks, labels = labels) +
     ylab("Instantaneous mortality rate") +
     xlab("Wood density")
}

plot_fig4 <- function(model, data) {
  p1 <- plot_mu_curves(model, hazard_curve= TRUE) + 
    ggtitle('High/Low wood density') + 
    theme(title = element_text(size=6))
  p2 <- plot_spp_curves(model, data, hazard_curve = TRUE) +
    partial_plot_theme(legend.position= c(0.8,0.65)) + 
    ggtitle('Species mortality curves') + 
    theme(title = element_text(size=6)) + 
    theme(legend.key.size =unit(0.4, "cm"))
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

