plotMarginals <- function(consfit, param=c("average", "sensitivity", "precision"), pal=palette(), xlim=NULL, ...){
  
  stopifnot(is(consfit,"ConsensusFit"))
  param <- match.arg(param)
  
  if(length(pal) < ncol(consfit@a_i)){
    stop("Palette size must be at least the number of platforms/conditions")
  }
  
  toplot <- switch(param, average = {consfit@a_i}, sensitivity = {consfit@b_i}, precision = {log(consfit@d_i)})
  
  if(is.null(xlim)){
    xlim=range(toplot)
  }
  
  densities <- apply(toplot, 2, density)
  maxdens <- max(unlist(lapply(densities, function (x) max(x$y))))
  
  plabel <- switch(param, average = {"a_i"}, sensitivity = {"b_i"}, precision = "Log (d_i)")
  plot(1, 1, xlim=xlim, ylim=c(-(maxdens/10), maxdens), type="n", 
       xlab=plabel, ylab="Density", main=paste0("Marginals: ", param))
  
  for (i in seq_len(ncol(toplot))){
    lines(densities[[i]], lwd=2, col=pal[i])
  }

  abline(h=0, col="gray")
  
  legend(min(toplot), 0, colnames(toplot), text.col=pal, horiz=TRUE, bty="n", ...)
  
}