plotOneFit <- function(multimeas, idx, pal=palette(), ...){
  
  if(length(pal) < length(multimeas@data)){
    stop("Palette size must be at least the number of platforms/conditions")
  }
  
  if(!idx %in% rownames(multimeas@data[[1]])){
    stop("Gene ID is not recognised. Check rownames of the data you passed to MultiMeasure().")
  }
  
  block <- getBlock(multimeas, idx)
  
  plotrange <- range(block)
  #Add 10% at bottom for legend
  plotrange[1] <- plotrange[1] - diff(plotrange)*0.1
  
  means <- colMeans(block)
  
  plot(means, seq_len(length(means)), ylim=plotrange, xlab="Sample means", ylab="Sample measurements", main=idx, type="n")
  
  for (i in seq_len(nrow(block))){
    
    points(means, block[i,], pch=16, col=pal[i], cex=1.3)
    fit.one <- lm(as.numeric(block[i,]) ~ means)
    abline(fit.one, col=pal[i])
    
  }
  
  legend(min(means), min(block), names(multimeas), text.col=pal, horiz=T, bty="n", ...)
  
}
