#' Mapping between Stan and R operators
#' 
#' @details 
#' A list of Stan operators (regex) and associated R operators.
#' 
#' @export

stan.operator.to.r.operator <- list("(?<!\\.)\\*" = "%stan*%",
                                    "\\.\\*" = "*",
                                    " ./ " = "/",
                                    "Phi" = "pnorm")
