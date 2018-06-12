plotMostDiscordant <- function(consfit, param=c("average", "sensitivity", "precision"), 
                               numloci=20, pal=colorRampPalette(brewer.pal(9, "RdYlGn"))){
  
  stopifnot(is(consfit,"ConsensusFit"))
  param <- match.arg(param)
  
  by <- switch(param, average = {length(consfit@V_a) - rank(consfit@V_a) + 1}, 
               sensitivity = {length(consfit@V_b) - rank(consfit@V_b) + 1}, 
               precision = {length(consfit@V_d) - rank(consfit@V_d) + 1})
  
  toplot <- switch(param, average = {consfit@a_i}, sensitivity = {consfit@b_i}, precision = {log(consfit@d_i)})
  toplot <- toplot[by %in% seq_len(numloci),]
  
  heatmap.2(toplot,
            density.info="none",  
            trace="none",         
            margins =c(12,8),     
            col=pal(100),       
            dendrogram="none",     
            key.xlab = param) 
  
}