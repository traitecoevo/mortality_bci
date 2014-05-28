
to.dev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  dev(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  to.dev(expr, pdf, filename, ..., verbose=verbose)
}

# returns 10^x as expression. useful for labelling axes
powerTenExpression<-function(x){
  do.call(expression, lapply(x, function(i) bquote(10^.(i))))
}

#return last element in dataframe or vector
last <- function(x) { tail(x, n = 1) }

axis.log10 <- function(side=1, horiz=FALSE, labels=TRUE, wholenumbers=TRUE, labelEnds=TRUE,las=1) {
  fg <- par("fg")

  #get range on axis
  if(side ==1 | side ==3) {
    r <- par("usr")[1:2]   #upper and lower limits of x-axis
  } else {
    r <- par("usr")[3:4] #upper and lower limits of y-axis
  }

  #make pertty intervals
  at <- pretty(r)
  #drop ends if desirbale
  if(!labelEnds)
    at <- at[at > r[1] & at < r[2]]
  #restrict to whole numbers if desriable
  if(wholenumbers)
    at<-at[is.wholenumber(at)]

  #make labels
  if ( labels ) {
    lab <- do.call(expression, lapply(at, function(i) bquote(10^.(i))))
    axis(side, at=10^at, lab, col=if(horiz) fg else NA,
         col.ticks=fg, las=las)
  } else {
    axis(side, at=10^at, FALSE, col=if(horiz) fg else NA,
         col.ticks=fg, las=las)
  }
}

## Make colours semitransparent:
make.transparent <- function(col, opacity=0.5) {
  if (length(opacity) > 1 && any(is.na(opacity))) {
    n <- max(length(col), length(opacity))
    opacity <- rep(opacity, length.out=n)
    col <- rep(col, length.out=n)
    ok <- !is.na(opacity)
    ret <- rep(NA, length(col))
    ret[ok] <- Recall(col[ok], opacity[ok])
    ret
  } else {
    tmp <- col2rgb(col)/255
    rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
  }
}



## Position label at a fractional x/y position on a plot
label <- function(px, py, lab, ..., adj=c(0, 1)) {
  usr <- par("usr")
  x <- usr[1] + px*(usr[2] - usr[1])
  y <- usr[3] + py*(usr[4] - usr[3])

  if(par("ylog"))
    y <- 10^y
  if(par("xlog"))
    x <- 10^x

  text(x,y, lab, adj=adj, ...)
}

#returns up to 80 unique, nice colors, generated using http://tools.medialab.sciences-po.fr/iwanthue/
# Starts repeating after 80
niceColors<-function(n=80){
  cols<-rep(c("#75954F","#D455E9","#E34423","#4CAAE1","#451431","#5DE737","#DC9B94","#DC3788","#E0A732","#67D4C1","#5F75E2","#1A3125","#65E689","#A8313C","#8D6F96","#5F3819","#D8CFE4","#BDE640","#DAD799","#D981DD","#61AD34","#B8784B","#892870","#445662","#493670","#3CA374","#E56C7F","#5F978F","#BAE684","#DB732A","#7148A8","#867927","#918C68","#98A730","#DDA5D2","#456C9C","#2B5024","#E4D742","#D3CAB6","#946661","#9B66E3","#AA3BA2","#A98FE1","#9AD3E8","#5F8FE0","#DF3565","#D5AC81","#6AE4AE","#652326","#575640","#2D6659","#26294A","#DA66AB","#E24849","#4A58A3","#9F3A59","#71E764","#CF7A99","#3B7A24","#AA9FA9","#DD39C0","#604458","#C7C568","#98A6DA","#DDAB5F","#96341B","#AED9A8","#55DBE7","#57B15C","#B9E0D5","#638294","#D16F5E","#504E1A","#342724","#64916A","#975EA8","#9D641E","#59A2BB","#7A3660","#64C32A"),
            ceiling(n/80))
  cols[1:n]
}
