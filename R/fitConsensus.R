fitConsensus <- function(multimeas){
  
  mandel_results <- lapply(seq_len(nrow(multimeas@data[[1]])), function (x) fitMandel(multimeas, x))
  
  a_i <- do.call(rbind, lapply(mandel_results, function (x) x$a_i))
  b_i <- do.call(rbind, lapply(mandel_results, function (x) x$b_i))
  d_i <- do.call(rbind, lapply(mandel_results, function (x) x$d_i))
  
  V_a <- rowVars(a_i)
  V_b <- rowVars(b_i)
  V_d <- unlist(lapply(mandel_results, function (x) x$vd))
  
  z0 <- unlist(lapply(mandel_results, function (x) x$z0))
  Vdelta <- unlist(lapply(mandel_results, function (x) x$vdelta))
  
  rownames(a_i) <- rownames(b_i) <- rownames(d_i) <- names(V_a) <- names(V_b) <- names(V_d) <- names(z0) <- names(Vdelta) <- rownames(multimeas@data[[1]])
  colnames(a_i) <- colnames(b_i) <- colnames(d_i) <- multimeas@names
  
  new("ConsensusFit", a_i=a_i, b_i=b_i, d_i=d_i, V_a=V_a, V_b=V_b, V_d=V_d, z0=z0, Vdelta=Vdelta)
  
}