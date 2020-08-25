#' Print
#'
#' Prints output of SRMR class
#'
#' @param object R object of class srmr
#' @param comp components to print
#' 
#' 
#' @examples
#' Sigma <- matrix(c(1.022, .550,  .622, .550, .928, .783, .622, .783, 1.150), 
#'                     nrow = 3)
#' S <- matrix(c(.770, .545, .515, .545, 1.003, .890, .515, .890, 1.211), 
#'             nrow = 3)
#' ybar <- c(2.516, 4.041, 5.021)
#' mu <- c(2.825, 3.877, 4.929)
#'
#' print(srmr(S = S,  Sigma = Sigma, ybar = ybar, mu = mu))
#' 
print.srmr <- function(object, 
                       comp = c("Total", "Correlation", "Variance", "Mean")) {
  
  if ("Total" %in% comp) {
  
    cat("Total\n")
    cat("  SRMR: ", object$srmr[1], "\n")      
    cat("  p:    ", object$size[1], "\n")
    cat("  SSSR: ", object$sssr[1], "\n\n\n")
  
  }

  if ("Correlation" %in% comp) {
  
    cat("Correlation\n")
    cat("  SRMR: ", object$srmr[2], "\n")      
    cat("  p:    ", object$size[2], "\n")
    cat("  SSSR: ", object$sssr[2], "\n\n")
  
  }

  if ("Variance" %in% comp & object$size[3] > 0) {
  
    cat("Variance\n")
    cat("  SRMR: ", object$srmr[3], "\n")      
    cat("  p:    ", object$size[3], "\n")
    cat("  SSSR: ", object$sssr[3], "\n\n")
  
  }  else if ("Variance" %in% comp & object$size[3] == 0) {
  
    cat("Variances not specified \n\n")
  
  }
  
  if ("Mean" %in% comp & object$size[4] > 0) {

    cat("Mean\n")
    cat("  SRMR: ", object$srmr[4], "\n")      
    cat("  p:    ", object$size[4], "\n")
    cat("  SSSR: ", object$sssr[4], "\n\n")
  
  }  else if ("Mean" %in% comp & object$size[4] == 0) {

    cat("Means not specified \n\n")
  
  }


}
