#' An S4 class to represent a residual fit indices.
#'
#' @slot Type A length-one numeric vector
#' @slot Index A length-one numeric vector
#' @slot Discrepency A length-one numeric vector
#' @slot Size A length-one numeric vector
#' 
setClass("Moments",
         representation(type = "character",
                        vcov = "matrix",
                        mean = "vector",
                        dim = "integer"))

