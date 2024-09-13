#' Get y value from a quadratic equation
#'
#' @param a the coefficient of x^2
#' @param b the coefficient of x
#' @param c the constant
#' @param x the x value
#'
#' @return the y value of the quadratic equation at the given x value
#' @export
#'
#' @examples
#' myquadratic(1, 2, 3, 4)
myquadratic <- function(a, b, c, x){
  y = a*x^2 + b*x + c
  return(y)
}

