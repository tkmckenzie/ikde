#' Creates variable declarations for Stan programs
#' 
#' @param var.name String indicating the name of the variable
#' @param var.type String indicating the variable type and any restrictions
#' @param var.dim String or number indicating the dimension of the variable
#' 
#' @return A string for the resulting variable declaration.
#' 
#' @details
#' This method allows for easy creation of variable declarations, accounting for semantic
#' differences in declaring vectors vs. arrays.
#' 
#' If var.dim is a string, it should be in the form of dimension declarations in a Stan program
#' (e.g., "[N, k]").
#' 
#' @examples
#' create.declaration("foo", "real<lower=0>", 5)
#' create.declaration("bar", "vector", "[k]")
#' create.declaration("mat", "matrix", "[N, k]")
#'
#' @export

create.declaration <-
  function(var.name, var.type, var.dim){
    if (!is.character(var.name)) stop("var.name must be a string.")
    if (!is.character(var.type)) stop("var.type must be a string.")
    if (!(is.character(var.dim) | is.numeric(var.dim))) stop("var.dim must be a string or number.")
    
    declaration <- var.type
    if (grepl("^(int|real)", var.type)){
      if (var.dim == 1){
        declaration <- paste0(declaration, " ", var.name, ";")
      } else{
        if (is.numeric(var.dim)){
          declaration <- paste0(declaration, " ", var.name, "[", as.character(var.dim), "];")
        } else{
          declaration <- paste0(declaration, " ", var.name, var.dim, ";")
        }
      }
    } else{
      if (is.numeric(var.dim)){
        declaration <- paste0(declaration, "[", as.character(var.dim), "] ", var.name, ";")
      } else{
        declaration <- paste0(declaration, var.dim, " ", var.name, ";")
      }
    }
    
    return(declaration)
  }
