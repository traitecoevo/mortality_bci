my_cols <- function(){
  c(b_blue="#6baed6", 
    b_grey="#969696", 
    b_purple="#9e9ac8", 
    b_green="#74c476", 
    b_pink="#e377c2", 
    b_orange="#FD8D3C",
    b_darkgrey="#2D2D2D"
  )
}

per_x <- function(p) {
  u <- par("usr")
  z <- u[1] + p * (u[2] - u[1])
  if(par("xlog"))
   z <- 10^z
  z
}

per_y <- function(p) {
  u <- par("usr")
  z <- u[3] + p * (u[4] - u[3])
  if(par("ylog"))
   z <- 10^z
  z
}


# utility function for embedding png images at specified fractional sizes in R plots
# places the logo centred on a specified fraction of the the usr space, 
# and sizes appropriately (respects aspect ratio)
# works for any plot size, including logged axes
# logo: a png object obtained with readPNG
# px: is a vector with 2 values (start and end) specifying the relative x positions (i.e. between 0 and 1)
# py: is the initial (i.e. bottom) relative y position (i.e. between 0 and 1)
# log: should the x and/or y axes be logged? Default to FALSE; Alternative values are 'x', 'y' or 'xy'
proportionalPng  <-  function(logo, px, py, log = FALSE, ...) {
    if(!is.numeric(px) | !is.numeric(py) | length(px) != 2) {
      stop('wrong position coordinates [0,1]')
    }
    usr  <-  par('usr')
    pin  <-  par('pin')
    # first get proportions of coordinates right
    pxRg    <-  px[2] - px[1]
    xProp   <-  usr[1] + px * (usr[2] - usr[1]) # x range from relative px 
    yProp   <-  usr[3] + py * (usr[4] - usr[3]) # minimum y from relative py
    # now get aspect ratio to calculate maximum y
    pinRatio  <-  pin[2] / pin[1] # aspect ratio of actual plot region, depends on device and plot size
    dims      <-  dim(logo)[1:2] # number of x-y pixels for the logo (aspect ratio)
    AR        <-  dims[1] / dims[2]
    yProp     <-  c(yProp, usr[3] + (py + pxRg * AR / pinRatio) * (usr[4] - usr[3])) # maximum y from relative py correcting for x and plot ratios
    if (log == 'x') xProp <- 10^(xProp)
    if (log == 'y') yProp <- 10^(yProp)
    if (log == 'xy') {xProp <- 10^(xProp); yProp <- 10^(yProp)}
    rasterImage(logo, xProp[1], yProp[1], xProp[2], yProp[2], interpolate=TRUE, ...)
}


fig_talk_map <- function(BCI_training_full) {

  data <- BCI_training_full %>%
    filter(census == 5) %>%
    mutate(S = as.factor(dead_next_census), 
           sp = as.factor(sp))


  rescale <- function(x, r.out = c(0.4, 1)) {
    p <- (x - min(x)) / (max(x) - min(x))
    r.out[[1]] + p * (r.out[[2]] - r.out[[1]])
  }

  cols <- my_cols()[4:5]
  par(oma=rep(0,4), mar = rep(1,4))
  plot(data[["gx"]], data[["gy"]], pch =16, 
       cex=rescale(sqrt(data$dbh), c(1, 3)),, col = cols[data$S],
        xlim = c(0, 100), ylim = c(0, 30), xaxs = "i", yaxs = "i",
        ann=FALSE, axes=FALSE)
  for(i in 1:4) axis(i, labels=FALSE)
  box()
  legend("bottomright", c("Alive", "Dead"), pch=16, col  = cols, bg = "white", cex=2 )
}


fig_talk_survival_time <- function(option =3) {
  cols <- my_cols()[c("b_orange", "b_purple", "b_pink")]
  par(mfrow = c(1,2), oma=rep(0,4), mar = c(4,5,1,1))

  m <- c(1, 0.5, 2)
  time <- seq(0, 5, length.out = 50)

  plot(time, 0*time, type = "n", lwd = 3, col = cols, las=1, ylim=c(0, 5),
        xlab = "Time (yr)", 
        ylab = expression(paste("Instantaneous\nmortality rate (",yr^-1,")"))) 
  for(i in seq_len(min(3,option))) {
    lines(time, 0*time + m[i], lwd = 3, col = cols[i])
  }

  if(option > 3)
    text(per_x(0.6), per_y(0.85), expression(lambda("t")), cex=1.5)

  plot(time, 0*time, type = "n", lwd = 3, ylim=c(0,1),
        xlab = "Time (yr)", ylab = "Survival probability (0-1)", las=1)
  for(i in seq_len(min(3,option))) {
    lines(time, exp(-m[i]*time), lwd = 3, col = cols[i])
  }

  if(option > 3) {
    text(per_x(0.6), per_y(0.85), expression("S(t) = e"^{-integral(lambda("t'")~"dt'", 0, "t'")}), cex=1.5)
    text(per_x(0.62), per_y(0.7), expression(" = e"^{-lambda~"t'"}), cex=1.5)
  }
}



fig_talk_hazards <- function() {
  
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
  
  
  # Produce empty plot
  empty_plot <- function(){
    par(mar=c(2,2,3,1))
    plot(NA, ylim = c(0,0.14),  xlim=c(0,1), xaxs='i', yaxs='i', axes=FALSE, ann=FALSE)
    axis(1, at = c(-1, 2), labels=NA)
    axis(2, at = c(-1, 2),labels=NA)
  }
  
  # Plot hazard curves
  hazard_plot <- function(f, text){
    x <- seq(0,1,length.out = 50)
    empty_plot()
    lines(x, f(x), type='l', col=my_cols()[c("b_orange")], lwd=2)
    label(0.25, 1.0, text, cex=2)
  }
  
  # Plot layout
  par(mfrow = c(1,3), oma=c(1,2,0,2), mar = c(1,3,0,1))
  
  # Plot base hazard
  hazard_plot(base_only_haz, expression(paste(lambda," = ",gamma)))
  mtext("Mortality rate", 2, line=1, cex=1)
  
  # Plot growth-dependent hazard
  hazard_plot(growth_haz, expression(paste(lambda," = ",alpha*" e"^{-beta~"X"})))
  mtext("Growth rate", 1, line =1, cex=1)
  
  # Example full baseline + growth-dependent hazard
  hazard_plot(base_growth_haz, expression(paste(lambda," = ",gamma~~+~~alpha*" e"^{-beta~"X"})))
}

fig_talk_cross_val <- function(panelc) {
 
  # Function to add labels to panels
  my_label <- function(text, x=0.2) label(x, 1.1, text)
  
  # Plot layout
  par(mfrow = c(1,2), oma=c(1,2,0,2), mar = c(1,3,0,1))

  # Plot logloss curve
  par(mar=c(2.5,3,2,1))
  plot(NA, xlim = c(0, 1), ylim= c(0, 5), ann = FALSE, las=1)
  p <- seq(0,1, length.out=200)
  lines(p, -log(1-p), type='l', col=my_cols()[c("b_pink")], lwd=2)
  my_label("Penalty for incorrect prediction")
  mtext("Probability of outcome", 1, line=2.1, cex=1)
  mtext("Log loss, L", 2, line=2.1, cex=1)
  
  # Plot cross validation procedure
  plot(NA, xlim = c(0, 1), ylim= c(0, 1), ann = FALSE, axes=FALSE)
  my_label("10-fold cross validation")
  
  img <- readPNG(panelc)
  proportionalPng(readPNG(panelc), c(0,1), c(0.1,0.9))

  text(0.5, -0.2,  expression(paste(bar("L"), " =", sum("L"[i])/10)), xpd=NA)
  }


fig_talk_fig45 <- function(model, data, param_variance_explained) {
  
  p1 <- plot_fig4a(model, data, hazard_curve = FALSE) +
    theme(legend.position= c(0.9,0.8),
          legend.key.size =unit(0.25, "cm"), 
          legend.title=element_text(size=4),
          legend.text = element_text(size=4),
          legend.title.align =0.75)  

  p2 <- plot_fig5(param_variance_explained) +
    ylab("Proportion of variation") 

  plot_grid(p1,p2, ncol=1, rel_heights = c(0.65, 0.35))
}


# Plot species parameters (For figS3 to figS5)
fig_talk_spp_params <- function(model, data) {
  
  parameter <- "alpha"
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
           species = as.character(species)) %>% 
           sample_frac(0.2)
  
  # Merge dataframes and prep for plotting
  dat <-bind_rows(hyper, spp) %>%
    mutate(
      species = as.factor(species)) %>%
    mutate(species = factor(species, levels=species[order(hyper_param)], ordered=TRUE),
           `2.5%` = exp(`2.5%`),
           `97.5%` = exp(`97.5%`),
           mean = exp(`mean`),
           ) 

  
  #Plot 
  breaks <- c(0.001,0.01,0.1, 1, 10)
  
  ggplot(dat, aes(y = mean, x = species, col=hyper_param)) + 
    geom_segment(aes(y=`2.5%`,x=species, yend=`97.5%`, xend=species), size=0.5) +
    geom_point(aes(y=mean, x=species), size=0.5) +
    scale_colour_manual(values = c("TRUE" ="red","FALSE" ="black")) +
    ylab("Mortality rate") +
    xlab("Species") +
    plot_theme() +
    scale_y_log10(breaks= breaks, labels = breaks) +
    theme(axis.text.x=element_text(size=0),
          axis.ticks = element_line(size= 0.5))
}

