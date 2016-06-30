

#loads an RData file, and returns it
load_rdata <- function(file) {
  v <- load(file)
  get(v)
}

## Really ugly working around something I've not worked out how to do
## in remake (1 function -> n file outputs)
export_data <- function(data, filename) {
  filename_fmt <- sub("\\.rds$", "_%s.rds", filename)
  filename_sub <- sprintf(filename_fmt, seq_along(data))
  for (i in seq_along(data)) {
    saveRDS(data[[i]], filename_sub[[i]])
  }
}

# Convert df to list
df_to_list <- function(x) {
  attr(x, "out.attrs") <- NULL # expand.grid leaves this behind.
  unname(lapply(split(x, seq_len(nrow(x))), as.list))
}

# Drop last observation
drop_last <- function(x) {
  if(length(x) > 0)
    x[seq_len(length(x)-1)]
  else
    NULL
}

# Calculate logloss
logloss = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
  - (actual * log(predicted) + (1 - actual) * log(1 - predicted))
}
# Sum of squares
sum_squares <- function(x) {
  sum((x-mean(x))^2)
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

# Plot theme
partial_plot_theme <- function(legend.position = "none", strips = FALSE,...) {
  sb <- if(strips==TRUE) element_rect(fill='lightgrey') else element_blank()
  st <- if(strips==TRUE) element_text() else element_blank()
  theme_classic(base_size = 7) + theme(strip.text = st,
                                       strip.background = sb,
                                       legend.position = legend.position,
                                       axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                                       axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                                       plot.margin = unit(c(3,3,3,3), "mm"))
}