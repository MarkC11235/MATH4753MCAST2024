#' Confidence interval for the mean
#'
#' @param x a vector of quantitative data
#'
#' @return a vector of length 2 with the lower and upper bounds of the confidence interval
#' @export
#'
#' @examples
#' myci(x=1:10)
myci <- function(x){
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- stats::sd(x)

  alpha <- 0.05
  t_alpha_2 <- stats::qt(1-alpha/2, df=n-1)

  CI <- mean_x + c(-1,1)*t_alpha_2*sd_x/sqrt(n)
  return(CI)
}
