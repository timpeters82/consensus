getBlock <- function(multimeas, idx){
  
  do.call(rbind, lapply(multimeas@data, function (x) x[idx,]))
  
}