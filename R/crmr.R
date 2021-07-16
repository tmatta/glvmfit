#' Standardized Root Mean Residual 
#'
#' Computes the square root of the discrepancy between the sample standardized covariance and mean 
#' and the model-implied standardized covariance and mean.  
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
#' srmr(S = S,  Sigma = Sigma, ybar = ybar, mu = mu)
#' 
#' @export

srmr <- function(S = NULL, Sigma = NULL, ybar = NULL, mu = NULL, 
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
  # compute CRMR for covariance
  #----------------------------------------------------------------------------#
  dim_S <- nrow(S)
  R <- cov2cor(S)
  Rho <- cov2cor(Sigma)
    
  P_cor <- (dim_S * (dim_S-1)) / 2

  dev_cor <- R - Rho

  delta_cor <- dev_cor[lower.tri(dev_cor)]^2

  crmr_cor <- sqrt(delta_cov / P_cor)

  #----------------------------------------------------------------------------#
  # compute CRMR for variance
  #----------------------------------------------------------------------------#
  if (sum(diag(S)) != sum(diag(Sigma))) {
    warning("S is not a correlation matrix nor is Sigma saturated!\nThe CRMR does not include misfit of the variances")
  }

  #----------------------------------------------------------------------------#
  # compute CRMR for mean
  #----------------------------------------------------------------------------# 
  if (is.null(ybar) | sum(ybar) == sum(mu)) {
  
    P_mean <- 0
    crmr_mean <- NA

  } else {

    if (length(ybar) != length(mu)) stop("ybar and mu are not the same size")
    if (length(ybar) != nrow(S)) stop("ybar/mu and S/Sigma are not of the same dimension")

    P_mean <- length(ybar)

    # standardized residual mean vector
    std_samp_mean <- ybar / sqrt(diag(S))
    std_impld_mean <- mu / sqrt(diag(Sigma))

    delta_mean <- (std_samp_mean - std_impld_mean)^2
    crmr_mean <- sqrt(delta_mean / P_mean)
  }

  #----------------------------------------------------------------------------#
  # compute SRMR for total structure
  #----------------------------------------------------------------------------# 
  delta <- sum(c(delta_cor, delta_mean), na.rm = TRUE)

  P <- sum(c(P_cor, P_mean))
    
  crmr_total <- sqrt(delta / P)

  #----------------------------------------------------------------------------#
  # package it up
  #----------------------------------------------------------------------------# 
  out_labs <- c("Total", "Correlation", "Mean")
  size_out <- c(P, P_cor, P_mean)
  sssr_out <- c(delta, delta_cor, delta_mean)
  srmr_out <- c(crmr_total, crmr_cor, srmr_mean)
  
  names(size_out) <- names(ssr_out) <- names(rmr_out) <- out_labs

  srmr <- list("size" = size_out,
               "ssr" = ssr_out,
               "crmr" = crmr_out)

  attr(crmr, "class") <- "crmr"

  return(crmr)

}

