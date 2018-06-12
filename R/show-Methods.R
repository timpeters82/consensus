setMethod("show", "MultiMeasure",
          function (object) cat(paste0("MultiMeasure object with ",
                                       length(object@data), " platforms/conditions, ",
                                       ncol(object@data[[1]]), " samples and ", 
                                       nrow(object@data[[1]]), " measured loci.\n"))
)

setMethod("show", "ConsensusFit",
          function (object) cat(paste0("ConsensusFit object with ",
                                       ncol(object@a_i), " platforms/conditions and ",
                                       nrow(object@a_i), " measured loci.\n"))
)

