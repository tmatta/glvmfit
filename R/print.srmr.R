sprint.srmr <- function(obj, 
                       comp = c("Total", "Correlation", "Variance", "Mean")) {
  
  if ("Total" %in% comp) {
  
    cat("Total\n")
    cat("  SRMR: ", obj$srmr[1], "\n")      
    cat("  p:    ", obj$size[1], "\n")
    cat("  SSSR: ", obj$sssr[1], "\n\n")
  
  }

  if ("Correlation" %in% comp) {
  
    cat("Correlation\n")
    cat("  SRMR: ", obj$srmr[2], "\n")      
    cat("  p:    ", obj$size[2], "\n")
    cat("  SSSR: ", obj$sssr[2], "\n\n")
  
  }

  if ("Variance" %in% comp & obj$size[3] > 0) {
  
    cat("Variance\n")
    cat("  SRMR: ", obj$srmr[3], "\n")      
    cat("  p:    ", obj$size[3], "\n")
    cat("  SSSR: ", obj$sssr[3], "\n\n")
  
  }  else if ("Variance" %in% comp & obj$size[3] == 0) {
  
    cat("Variances not specified \n\n")
  
  }
  
  if ("Mean" %in% comp & obj$size[4] > 0) {

    cat("Mean\n")
    cat("  SRMR: ", obj$srmr[4], "\n")      
    cat("  p:    ", obj$size[4], "\n")
    cat("  SSSR: ", obj$sssr[4], "\n\n")
  
  }  else if ("Mean" %in% comp & obj$size[4] == 0) {

    cat("Means not specified \n\n")
  
  }


}
