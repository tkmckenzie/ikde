#' Mapping between Stan and R operators
#' 
#' @details 
#' A list of Stan operators and associated R distribution functions.
#' 
#' @export

stan.operator.to.r.operator <- list(" * " = " %*% ",
                                    " .* " = "*",
                                    " ./ " = "/")
