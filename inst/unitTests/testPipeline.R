testpipeline <- function(){
  data(TCGA)
    # Test MultiMeasure()
  #########################################
  checkException(tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNA-Seq"), 
                                         data=list(U133A[-1,], Huex, Agilent, RNASeq)))
  badrowU133A <- badcolU133A <- U133A
  rownames(badrowU133A) <- paste0("dummy", 1:nrow(badrowU133A))
  colnames(badcolU133A) <- paste0("dummy", 1:ncol(badcolU133A))
  
  checkException(tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNA-Seq"), 
                                         data=list(badrowU133A, Huex, Agilent, RNASeq)))
  
  checkException(tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNA-Seq"), 
                                         data=list(badcolU133A, Huex, Agilent, RNASeq)))
  
  checkException(tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNA-Seq"), 
                                        data=list(U133A, Huex, Agilent)))
  
  checkException(tcga_mm <- MultiMeasure(names=c("U133A", "Huex"), 
                                         data=list(U133A, Huex)))
  
  ###############################################################
  
  tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNA-Seq"), 
                          data=list(U133A, Huex, Agilent, RNASeq))
  
  #Test fitConsensus
  ##################################################################################
  checkException(fitConsensus(1))
  ###################################################################################
  
  fit <- fitConsensus(tcga_mm)
  
  #Test plotOneFit()
  #########################################################################
  checkException(plotOneFit(1, "TP53", brewer.pal(n = 4, name = "Dark2")))
  checkException(plotOneFit(tcga_mm, "NOTAGENE", brewer.pal(n = 4, name = "Dark2")))
  checkException(plotOneFit(tcga_mm, "NOTAGENE", brewer.pal(n = 3, name = "Dark2")))
  #########################################################################
  
  #Test plotMarginals()
  #########################################################################
  checkException(plotMarginals(1, "average", brewer.pal(n = 4, name = "Dark2")))
  checkException(plotMarginals(fit, "publishability", brewer.pal(n = 4, name = "Dark2")))
  checkException(plotMarginals(fit, "average", brewer.pal(n = 3, name = "Dark2")))
  #########################################################################
  
  #Test plotMostDiscordant()
  #########################################################################
  checkException(plotMostDiscordant(1, "sensitivity", 15))
  checkException(plotMostDiscordant(fit, "publishability", 15))
  checkException(plotMostDiscordant(fit, "sensitivity", -2))
  #########################################################################
}