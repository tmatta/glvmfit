#' Root Mean Residual 
#'
#' Computes the square root of the discrepancy between the sample covariance and mean 
#' and the model-implied covariance and mean.  
#'
#' @param S sample covariance matrix
#' @param Sigma model-implied covariance matrix
#' @param ybar sample mean vector
#' @param mu model-implied mean vector
#' @param lavaan_object is a fitted model of class \code{lavaan}
#' @param exo boolean argument indicating if model has exogenous covariates
#' 
#' @return A list including the SRMR component names, sum of squared residuals 
#'         for each component, and the SRMR for each component.
#' 
#' @section Details:
#' \code{S}, \code{Sigma}, \code{ybar}, and \code{mu} must be of the same dimensions. 
#' 
#' If the sum of the diagonal elements of \code{S} equals the sum of the diagonal elements of \code{Sigma} 
#' the variance component of SRMR is not included
#'  
#' If the sum of the sample means \code{yhat} equals the sum of the model-implied means \code{mu} 
#' the mean component of SRMR is not included
#' 
#' @examples
#' Sigma <- matrix(c(1.022, .550,  .622, .550, .928, .783, .622, .783, 1.150), 
#'                     nrow = 3)
#' S <- matrix(c(.770, .545, .515, .545, 1.003, .890, .515, .890, 1.211), 
#'             nrow = 3)
#' ybar <- c(2.516, 4.041, 5.021)
#' mu <- c(2.825, 3.877, 4.929)
#'
#' rmr(S = S,  Sigma = Sigma, ybar = ybar, mu = mu)
#' 
#' @export

rmr <- function(S = NULL, Sigma = NULL, ybar = NULL, mu = NULL, 
                 lavaan_object = NULL, exo = TRUE) {

  if (!is.null(lavaan_object)) {

    moment_list <- get_moments(lavaan_object, exo)

    S     <- moment_list[["S"]]
    Sigma <- moment_list[["Sigma"]]
    ybar  <- moment_list[["ybar"]]
    mu    <- moment_list[["mu"]]

  }

  #if (nrow(S) != ncol(S)) stop("S is not a square matrix")
  if (nrow(Sigma) != ncol(Sigma)) stop("Sigma is not a square matrix")
  if (sum(dim(S)) != sum(dim(Sigma))) stop("S and Sigma are not the same size")

  #----------------------------------------------------------------------------#
  # compute SRMR for covariance
  #----------------------------------------------------------------------------#
  dim_S <- nrow(S)

  P_cov <- (dim_S * (dim_S-1)) / 2

  dev_vcov <- S - Sigma

  delta_cov <- dev_vcov[lower.tri(dev_vcov)]^2
  rmr_cov <- sqrt(delta_cov / P_cov)

  #----------------------------------------------------------------------------#
  # compute SRMR for variance
  #----------------------------------------------------------------------------#
  if (sum(diag(S)) == sum(diag(Sigma))) {
  
    P_var <- 0
    rmr_var <- 0

  } else {
    
    P_var <- dim_S
    delta_var <- diag(dev_vcov)^2
    rmr_var <- sqrt(delta_var / P_var)

  } 

  #----------------------------------------------------------------------------#
  # compute SRMR for mean
  #----------------------------------------------------------------------------# 
  if (is.null(ybar) | sum(ybar) == sum(mu)) {
  
    P_mean <- 0
    rmr_mean <- NA

  } else {

    if (length(ybar) != length(mu)) stop("ybar and mu are not the same size")
    if (length(ybar) != nrow(S)) stop("ybar/mu and S/Sigma are not of the same dimension")

    P_mean <- length(ybar)

    # residual mean vector
    delta_mean <- (ybar - mu)^2
    rmr_mean <- sqrt(delta_mean / P_mean)
  }

  #----------------------------------------------------------------------------#
  # compute SRMR for total structure
  #----------------------------------------------------------------------------# 
  delta <- sum(c(delta_cov, delta_var, delta_mean), na.rm = TRUE)

  P <- sum(c(P_cov, P_var, P_mean))
    
  rmr_total <- sqrt(delta / P)

  #----------------------------------------------------------------------------#
  # package it up
  #----------------------------------------------------------------------------# 
  out_labs <- c("Total", "Covariance", "Variance", "Mean")
  size_out <- c(P, P_cov, P_var, P_mean)
  ssr_out <- c(delta, delta_cov, delta_var, delta_mean)
  rmr_out <- c(rmr_total, rmr_cov, rmr_var, rmr_mean)
  
  names(size_out) <- names(ssr_out) <- names(rmr_out) <- out_labs

  rmr <- list("size" = size_out,
              "ssr" = ssr_out,
              "rmr" = rmr_out)

  attr(rmr, "class") <- "rmr"

  return(rmr)

}

