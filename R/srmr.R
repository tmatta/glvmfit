#' Standardized Root Mean Residual 
#'
#' Computes the square root of the discrepancy between the sample covariance and mean 
#' and the model-implied covariance and mean.  
#'
#' @param S sample covariance matrix
#' @param Sigma model-implied covariance matrix
#' @param ybar sample mean vector
#' @param mu model-implied mean vector
#' @param verbose when \code{TRUE} prints SRMR components to the console
#' 
#' @section Details:
#' \code{S}, \code{Sigma}, \code{ybar}, and \code{mu} must be of the same dimensions. 
#' 
#' If the sum of the diagonal elements of \code{S} equal 1 such that \code{S} is a correlation matrix, 
#' the variance component of SRMR is not included
#'  
#' @examples
#' Sigma <- matrix(c(1.022, .550,  .622, .550, .928, .783, .622, .783, 1.150), 
#'                     nrow = 3)
#' S <- matrix(c(.770, .545, .515, .545, 1.003, .890, .515, .890, 1.211), 
#'             nrow = 3)
#' ybar <- c(2.516, 4.041, 5.021)
#' mu <- c(2.825, 3.877, 4.929)
#'
#' srmr(S = S,  Sigma = Sigma, ybar = ybar, mu = mu)
#' 
#' @export

srmr <- function(S, Sigma, ybar = NULL, mu = NULL, verbose = FALSE) {

  if (nrow(S) != ncol(S)) stop("S is not a square matrix")
  if (nrow(Sigma) != ncol(Sigma)) stop("Sigma is not a square matrix")
  if (sum(dim(S)) != sum(dim(Sigma))) stop("S and Sigma are not the same size")

  #----------------------------------------------------------------------------#
  # compute SRMR for correlation
  #----------------------------------------------------------------------------#
  dim_S <- nrow(S)
  n_cor <- (dim_S * (dim_S-1)) / 2
  R <- stats::cov2cor(S)
  lt_R <- R[lower.tri(R)]
  Rho <- stats::cov2cor(Sigma)
  lt_Rho <- Rho[lower.tri(Rho)] 

  sq_resid_cor <- (lt_R - lt_Rho)^2
  sum_sq_resid_cor <- sum(sq_resid_cor)

  srmr_cor <- sqrt(sum_sq_resid_cor / n_cor)

  #----------------------------------------------------------------------------#
  # compute SRMR for variance
  #----------------------------------------------------------------------------#
  diag_S <- diag(S)
  diag_Sigma <- diag(Sigma)

  # If S is a correlation, nvar is 0
  if (sum(diag(S)) == nrow(S)){
  
    n_var <- 0
    sum_std_sq_resid_var <- NA
    srmr_var <- NA

  } else {
    
    n_var <- dim_S
    std_sq_resid_var <- ((diag_S - diag_Sigma) / diag_S)^2
    sum_std_sq_resid_var <- sum(std_sq_resid_var)
  
    srmr_var <- sqrt(sum_std_sq_resid_var / n_var)
  } 

  
  #----------------------------------------------------------------------------#
  # compute SRMR for mean
  #----------------------------------------------------------------------------# 

  if (is.null(ybar)) {
  
    n_mean <- 0
    sum_std_sq_resid_mean <- NA
    srmr_mean <- NA

  } else {

    if (length(ybar) != length(mu)) stop("ybar and mu are not the same size")
    if (length(ybar) != nrow(S)) stop("ybar/mu and S/Sigma are not of the same dimesnion")

    n_mean <- length(ybar)

    # compute the sum of standardized residucals for mean
    sqrt_diag_S <- sqrt(diag_S)
    sqrt_diag_Sigma <- sqrt(diag_Sigma)

    std_sq_resid_mean <- ((ybar / sqrt_diag_S) - (mu / sqrt_diag_Sigma))^2
    sum_std_sq_resid_mean <- sum(std_sq_resid_mean)

    srmr_mean <- sqrt(sum_std_sq_resid_mean / n_mean) 
  }

  #----------------------------------------------------------------------------#
  # compute SRMR for total structure
  #----------------------------------------------------------------------------# 
  numer <- sum(c(sum_sq_resid_cor, sum_std_sq_resid_var, sum_std_sq_resid_mean), na.rm = TRUE)

  denom <- sum(c(n_cor, n_var, n_mean))
 
  srmr_total <- sqrt(numer / denom)

  #----------------------------------------------------------------------------#
  # package it up
  #----------------------------------------------------------------------------# 
  srmr_df <- data.frame("comp" = c("Total", "Correlation", "Variance", "Mean"),
                        "size" = c(denom, n_cor, n_var, n_mean),
                        "sssr" = c(numer, sum_sq_resid_cor, sum_std_sq_resid_var, sum_std_sq_resid_mean),
                        "srmr" = c(srmr_total, srmr_cor, srmr_var, srmr_mean))

  if (verbose == TRUE) {
    cat("\nNumber of elements",
        "\n  Total:       ", denom,
        "\n  Correlation: ", n_cor,
        "\n  Variance:    ", n_var,
        "\n  Mean:        ", n_mean, "\n",
        "\nSum of standarized squared residuals",
        "\n  Total:       ", numer,
        "\n  Correlation: ", sum_sq_resid_cor,
        "\n  Variance:    ", sum_std_sq_resid_var,
        "\n  Mean:        ", sum_std_sq_resid_mean, "\n",       
        "\nSRMR",
        "\n  Total:       ", srmr_total,      
        "\n  Correlation: ", srmr_cor,
        "\n  Variance:    ", srmr_var,
        "\n  Mean:        ", srmr_mean, "\n\n") 
  }

  return(srmr_df)

}

