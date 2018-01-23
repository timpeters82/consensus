fitMandel <- function(multimeas, idx){
  
  block <- getBlock(multimeas, idx)
  xj <- colMeans(block)
  mui <- rowMeans(block)
  gammahatj <- xj - mean(xj)
  betahati <- colSums(apply(block, 1, function (x) x*gammahatj))/sum(gammahatj^2)
  
  fitted.nores <- mui + betahati %*% t(gammahatj)
  etaij <- block - fitted.nores
  
  ssr <- apply(etaij, 1, function (x) sum(x^2))
  vhatieta <- ssr/(ncol(block)-2)
  veta <- sum(vhatieta)/(nrow(block)-1)
  vmu <- var(mui)
  vbeta <- var(betahati)
  
  alphahat <- (sum((betahati - 1)*(mui-mean(mui))))/sum((mui-mean(mui))^2)
  residuals <- sapply(seq_len(nrow(block)), function (x) (betahati[x]-1) - alphahat*(mui[x]-mean(mui)))
  vdelta <- sum(residuals^2)/(nrow(block)-2)
  z0 <- mean(mui) - 1/alphahat
  
  if (idx %% 10000 == 0) message(paste0("Fit ", idx, " loci..."))
  
  return(list(a_i=mui, b_i=betahati, d_i=vhatieta, vd=veta, vdelta=vdelta, z0=z0))
}