setGeneric("MultiMeasure",  valueClass = "MultiMeasure", function(names=NA_character_, data.matrices=list()) {
  standardGeneric("MultiMeasure")
})

setGeneric("fitConsensus",  valueClass = "ConsensusFit", function(multimeas) {
  standardGeneric("fitConsensus")
})

setGeneric("plotOneFit", function(multimeas, idx, pal=palette()) {
  standardGeneric("plotOneFit")
})

setGeneric("plotMarginals", function(consfit, param=c("average", "sensitivity", "precision"), pal=palette()){
  standardGeneric("plotMarginals")
})

setGeneric("plotMostDiscordant", function(consfit, param=c("average", "sensitivity", "precision"), 
                                          numloci=20, pal=colorRampPalette(brewer.pal(9, "RdYlGn"))) {
  standardGeneric("plotMostDiscordant")
})
