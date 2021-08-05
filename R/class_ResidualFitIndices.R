#' An S4 class to represent the set of residual fit indicies
#'
#' @slot sampleMoments 
#' @slot impliedMoments 
#' @slot RMR 
#' @slot SRMR 
#' @slot CRMR
#' 
#' @rdname ResidualFitIndices
#' 
#' @export
#' 
setClass("ResidualFitIndices", 
         slots = c(sampleMoments = "list",
                   impliedMoments = "list",
                   RMR  = "ResidualFitIndex",
                   SRMR = "ResidualFitIndex",
                   CRMR = "ResidualFitIndex"))

setMethod(f = "initialize", signature = "ResidualFitIndices",
          definition = function(.Object) {
            .Object@sampleMoments = list(yBar = matrix(NA_real_), S = matrix(NA_real_))
            .Object@impliedMoments = list(muHat = matrix(NA_real_), SigmaHat = matrix(NA_real_))
            .Object@RMR  = new("ResidualFitIndex")
            .Object@SRMR = new("ResidualFitIndex") 
            .Object@CRMR = new("ResidualFitIndex")
            return(.Object)
          }
)

setMethod(f = "print", 
          signature = "ResidualFitIndices", 
          definition = function(x, ...) {
  
    cat("Residual Fit Indices\n",
        "  RMR:  ", round(x@RMR@index$total, 3), "\n",      
        "  SRMR: ", round(x@SRMR@index$total, 3), "\n",      
        "  CRMR: ", round(x@CRMR@index$total, 3), "\n")    

})


setMethod(f = "show", 
          signature = "ResidualFitIndices", 
          definition = function(object) {
  
    cat("Residual Fit Indices\n",
        "  RMR:  ", round(object@RMR@index$total, 3), "\n",      
        "  SRMR: ", round(object@SRMR@index$total, 3), "\n",      
        "  CRMR: ", round(object@CRMR@index$total, 3), "\n")    

})

#' @rdname ResidualFitIndices 

setGeneric(name = "details", def = function(object, comp = c("Total", "Covariance", "Variance", "Mean", "Total")) {
 standardGeneric("details")
 }
)

#' @rdname ResidualFitIndices
#' 
#' @param object R object of type \code{ResidualFitIndices}.
#' @param comp Character indicating the components to include.
#' 
#' @note \code{comp} can be "Total" for overall fit indices, "Cov" for
#'  covariance elements (off diagonals), "Var" for variance components (diagonal), and "Mean" 
#'  means. 
#' 
#' @export 
#' 
setMethod(f = "details", 
          signature = "ResidualFitIndices", 
          function(object, comp = c("Total", "Covariance", "Variance", "Mean", "Total")) {


  if ("Total" %in% comp) {
  
    cat("Total\n")
    cat("  RMR:  ", round(object@RMR@index$total, 3), "\n")      
    cat("  SRMR: ", round(object@SRMR@index$total, 3), "\n")      
    cat("  CRMR: ", round(object@CRMR@index$total, 3), "\n")   
  
  }  

  if ("Covariance" %in% comp) {
  
    cat("Covariance (off-diagonal)\n")
    cat("  RMR:  ", round(object@RMR@index$cov, 3), "\n")      
    cat("  SRMR: ", round(object@SRMR@index$cov, 3), "\n")      
    cat("  CRMR: ", round(object@CRMR@index$cov, 3), "\n")   
  
  }

  if ( !("Variance" %in% comp) | 
      all(diag(object@sampleMoments$S) == diag(object@impliedMoments$SigmaHat))) {
  
    cat("Variances not included \n\n")

  } else if("Variance" %in% comp) {

    cat("Variance\n")
    cat("  RMR:  ", round(object@RMR@index$var, 3), "\n")      
    cat("  SRMR: ", round(object@SRMR@index$var, 3), "\n")      
  
  }  
  
  if ("Mean" %in% comp & !is.null(object@sampleMoments$yBar)) {

    cat("Mean\n")
    cat("  RMR:  ", round(object@RMR@index$mean, 3), "\n")      
    cat("  SRMR: ", round(object@SRMR@index$mean, 3), "\n")      
    cat("  CRMR: ", round(object@CRMR@index$mean, 3), "\n\n")   
  
  }  else if ("Mean" %in% comp & is.null(object@sampleMoments$yBar)) {

    cat("Means not specified \n\n")
  
  }
})


