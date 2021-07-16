#' An S4 class to represent a residual fit indices.
#'
#' @slot type A length-one numeric vector
#' @slot resid A length-one numeric vector
#' @slot ssr A length-one numeric vector
#' @slot size A length-one numeric vector
#' @slot index
#' 
setClass("ResidualFitIndex", 
         slots = c(type  = "character",
                   resid = "list",
                   ssr   = "list",
                   size  = "list",
                   index = "list"),
         prototype = list(type = NA_character_,
                          resid = list(vcov = matrix(NA_real_), mean = matrix(NA_real_)),
                          ssr   = list(total = NA_real_, mean = NA_real_, var = NA_real_, cov = NA_real_),
                          size   = list(total = NA_integer_, mean = NA_integer_, var = NA_integer_, cov = NA_integer_),
                          index = list(total = NA_real_, mean = NA_real_, var = NA_real_, cov = NA_real_)))

#' An S4 class to represent the set of residual fit indicies
#'
#' @slot RMR 
#' @slot SRMR 
#' @slot CRMR
#' 
setClass("ResidualFitIndices", 
         slots = c(sampleMoments = "list",
                   impliedMoments = "list",
                   RMR  = "ResidualFitIndex",
                   SRMR = "ResidualFitIndex",
                   CRMR = "ResidualFitIndex"),
         prototype = list(sampleMoments = list(yBar = matrix(NA_real_), S = matrix(NA_real_)),
                          impliedMoments = list(muHat = matrix(NA_real_), SigmaHat = matrix(NA_real_)),
                          RMR = new("ResidualFitIndex"), 
                          SRMR = new("ResidualFitIndex"), 
                          CRMR = new("ResidualFitIndex"))
         )

