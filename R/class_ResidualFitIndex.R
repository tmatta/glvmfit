#' An S4 class to represent a residual fit indices.
#'
#' @slot type A length-one numeric vector
#' @slot resid A length-one numeric vector
#' @slot ssr A length-one numeric vector
#' @slot size A length-one numeric vector
#' @slot index
#' 
setClass("ResidualFitIndex", 
         slots = c(
           type  = "character",
           resid = "list",
           ssr   = "list",
           size  = "list",
           index = "list"
         )
)

setMethod(f = "initialize", signature = "ResidualFitIndex",
         definition = function(.Object) {
            .Object@type  = NA_character_
            .Object@resid = list(vcov = matrix(NA_real_), mean = matrix(NA_real_))
            .Object@ssr   = list(total = NA_real_, mean = NA_real_, var = NA_real_, cov = NA_real_)
            .Object@size  = list(total = NA_integer_, mean = NA_integer_, var = NA_integer_, cov = NA_integer_)
            .Object@index = list(total = NA_real_, mean = NA_real_, var = NA_real_, cov = NA_real_)
            return(.Object)
 }
)
