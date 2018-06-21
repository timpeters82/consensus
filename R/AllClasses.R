

checkMM <- function(object) {
  
  length_names <- length(object@names)
  length_data <- length(object@data)
  
  if (length_names != length_data) {
    stop(paste0("Length of names vector *must* be equal to number of data matrices provided. \n
                Matrices: ", length_data, ". Names provided: ", length_names))
  }
  
  if (length_data < 3) {
    stop("Must be at least 3 matrices for a consensus fit.")
  }
  
  dims <- lapply(object@data, dim)
  names(dims) <- object@names
  dimcheck <- outer(dims, dims, Vectorize(all.equal))
  dimcheck[dimcheck!="TRUE"] <- "FALSE"
  dimcheck <- matrix(as.logical(dimcheck), nrow(dimcheck), ncol(dimcheck))
  rownames(dimcheck) <- colnames(dimcheck) <- object@names
  if (!all(dimcheck)){
    message("Error: Not all matrix dimensions are identical across platforms. Dimension equalities:")
    print(dimcheck)
    stop()
  }
  
  columnnames <- lapply(object@data, colnames)
  names(columnnames) <- object@names
  colcheck <- outer(columnnames, columnnames, Vectorize(all.equal))
  colcheck[colcheck!="TRUE"] <- "FALSE"
  colcheck <- matrix(as.logical(colcheck), nrow(colcheck), ncol(colcheck))
  rownames(colcheck) <- colnames(colcheck) <- object@names
  if (!all(colcheck)){
    message("Error: Not all sample names (colnames) are identical across platforms. Equalities:")
    print(colcheck)
    stop()
  }
  
  locusnames <- lapply(object@data, rownames)
  names(locusnames) <- object@names
  rowcheck <- outer(locusnames, locusnames, Vectorize(all.equal))
  rowcheck[rowcheck!="TRUE"] <- "FALSE"
  rowcheck <- matrix(as.logical(rowcheck), nrow(rowcheck), ncol(rowcheck))
  rownames(rowcheck) <- colnames(rowcheck) <- object@names
  if (!all(rowcheck)){
    message("Error: Not all locus names (rownames) are identical across platforms. Equalities:")
    print(rowcheck)
    stop()
  }
  
  TRUE
  }


setClass("MultiMeasure", 
         representation(names="character", data = "list"),
         prototype(names=NA_character_, data=list()),
         validity = checkMM)



setClass("ConsensusFit",
         representation(a_i="matrix", b_i="matrix", d_i="matrix", V_a="numeric", V_b="numeric", 
                        V_d="numeric", z0="numeric", Vdelta="numeric"))

