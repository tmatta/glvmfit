#' Residual fit indices 
#'
#' Computes the RMR, SRMR, and CRMR.  
#'
#' @param S sample covariance matrix
#' @param Sigma model-implied covariance matrix
#' @param ybar sample mean vector
#' @param mu model-implied mean vector
#' @param lavaan_object is a fitted model of class \code{lavaan}
#' @param exo boolean argument indicating if model has exogenous covariates
#' 
#' @return An S4 object
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
#' resid_fit(S = S,  Sigma = Sigma, ybar = ybar, mu = mu)
#' 
#' @export
#' 
resid_fit <- function(S = NULL, Sigma = NULL, ybar = NULL, mu = NULL, 
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
  if (nrow(S) != ncol(S)) stop("S is not a square matrix")
  if (sum(dim(S)) != sum(dim(Sigma))) stop("S and Sigma are not the same size")

  if (!is.null(ybar) & !is.null(mu)) {

    if (length(ybar) != length(mu)) stop("ybar and mu are not the same size")
    if (length(ybar) != nrow(S)) stop("ybar/mu are not of the same dimension")
    if (length(mu) != nrow(Sigma)) stop("S/Sigma are not of the same dimension")

  }

  # P --------------------------------------------------------------------------
  P_mean <- ifelse(is.null(ybar), 0, length(ybar))

  P_var <- ifelse(all(diag(S) == diag(Sigma)), 0, nrow(S))

  P_lt <- (nrow(S) * (nrow(S) - 1)) / 2

  P <- sum(P_mean, P_var, P_lt)

  # RMR ------------------------------------------------------------------------
  raw_dev_mean <- ybar - mu
  ss_raw_dev_mean <- sum(raw_dev_mean^2)

  raw_dev_vcov <- S - Sigma
  ss_raw_dev_lt <- sum_sq_lt(raw_dev_vcov) 
  ss_raw_dev_var <- sum(diag(raw_dev_vcov)^2)

  ss_raw_dev_total <- sum(ss_raw_dev_lt, ss_raw_dev_var, ss_raw_dev_mean)

  rmr_mean <- resid_index(ssr = ss_raw_dev_mean, P = P_mean) 
  rmr_var  <- resid_index(ssr = ss_raw_dev_var, P = P_var) 
  rmr_cov  <- resid_index(ssr = ss_raw_dev_lt, P = P_lt) 
  rmr_total <- resid_index(ssr = ss_raw_dev_total, P = P)

  rmr_obj <- new("ResidualFitIndex")
  rmr_obj@type  <- "RMR"
  rmr_obj@resid <- list(mean  = raw_dev_mean, 
                     vcov  = raw_dev_vcov)
  rmr_obj@ssr   <- list(total = ss_raw_dev_total,
                        mean  = ss_raw_dev_mean, 
                        var   = ss_raw_dev_var, 
                        cov   = ss_raw_dev_lt)
  rmr_obj@size   <- list(total = P, 
                         mean  = P_mean, 
                         var   = P_var, 
                         cov   = P_lt)
  rmr_obj@index <- list(total = rmr_total, 
                        mean  = rmr_mean, 
                        var   = rmr_var,
                        cov   = rmr_cov)

  # SRMR -----------------------------------------------------------------------
  D <- diag(sqrt(diag(S)))
  invD <- solve(D)

  std_dev_mean <- invD %*% (ybar - mu) 
  ss_std_dev_mean <- sum(std_dev_mean^2)

  std_dev_vcov <- invD %*% raw_dev_vcov %*% invD
  ss_std_dev_lt <- sum_sq_lt(std_dev_vcov) 
  ss_std_dev_var <- sum(diag(std_dev_vcov)^2)

  ss_std_dev_total <- sum(ss_std_dev_lt, ss_std_dev_var, ss_std_dev_mean)
  srmr_mean  <- resid_index(ssr = ss_std_dev_mean, P = P_mean) 
  srmr_var   <- resid_index(ssr = ss_std_dev_var, P = P_var) 
  srmr_cov   <- resid_index(ssr = ss_std_dev_lt, P = P_lt) 
  srmr_total <- resid_index(ssr = ss_std_dev_total, P = P)

  srmr_obj <- new("ResidualFitIndex")
  srmr_obj@type  <- "SRMR"
  srmr_obj@resid <- list(mean  = std_dev_mean, 
                      vcov  = std_dev_vcov)
  srmr_obj@ssr   <- list(total = ss_std_dev_total,
                      mean  = ss_std_dev_mean, 
                      var   = ss_std_dev_var, 
                      cov   = ss_std_dev_lt)
  srmr_obj@size   <- list(total = P, 
                       mean  = P_mean, 
                       var   = P_var, 
                       cov   = P_lt)
  srmr_obj@index <- list(total = srmr_total, 
                      mean  = srmr_mean, 
                      var   = srmr_var,
                      cov   = srmr_cov)

  # CRMR -----------------------------------------------------------------------
  std_samp_mean <- ybar / sqrt(diag(S))
  std_impld_mean <- mu / sqrt(diag(Sigma))
  dev_std_mean <- std_samp_mean - std_impld_mean
  ss_dev_std_mean <- sum(dev_std_mean^2)
  crmr_mean  <- resid_index(ssr = ss_dev_std_mean, P = P_mean) 

  Rho <- cov2cor(Sigma)
  R <- cov2cor(S)
  dev_std_vcov <- R - Rho
  ss_dev_std_lt <- sum_sq_lt(dev_std_vcov) 
  ss_dev_std_total <- sum(ss_dev_std_lt, ss_dev_std_mean)
  crmr_cov   <- resid_index(ssr = ss_dev_std_lt, P = P_lt) 

  crmr_total <- resid_index(ssr = ss_dev_std_total, P = sum(c(P_mean, P_lt)))

  crmr_obj <- new("ResidualFitIndex")
  crmr_obj@type  <- "CRMR"
  crmr_obj@resid <- list(mean  = dev_std_mean, 
                      vcov  = dev_std_vcov)
  crmr_obj@ssr   <- list(total = ss_dev_std_total,
                      mean  = ss_dev_std_mean, 
                      cov   = ss_dev_std_lt)
  crmr_obj@size   <- list(total = sum(c(P_mean, P_lt)), 
                       mean  = P_mean, 
                       cov   = P_lt)
  crmr_obj@index <- list(total = crmr_total, 
                      mean  = crmr_mean, 
                      cov   = crmr_cov)

  # Final fit object to return -------------------------------------------------
  resid_fit_obj <- new(Class = "ResidualFitIndices")

  resid_fit_obj@RMR = rmr_obj
  resid_fit_obj@SRMR = srmr_obj
  resid_fit_obj@CRMR = crmr_obj

  return(resid_fit_obj)

}