setMethod("show", "ResidualFitIndices", function(object) {
  
    cat("Residual Fit Indices\n")
    cat("  RMR:  ", round(object@RMR@index$total, 3), "\n")      
    cat("  SRMR: ", round(object@SRMR@index$total, 3), "\n")      
    cat("  CRMR: ", round(object@CRMR@index$total, 3), "\n")      

})
