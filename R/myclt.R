#' Makes iter samples of n random numbers from a uniform distribution
#' and makes a histogram of the means of the samples.
#'
#' @param n the number of elements in each sample
#' @param iter the number of samples to take
#' @param a the lower bound of the uniform distribution
#' @param b the upper bound of the uniform distribution
#'
#' @return a vector of the means of the samples
#'
#' @export
#'
#' @examples
#' myclt(10, 1000)
#' myclt(10, 1000, 0, 10)
#' myclt(30, 1000, 0, 10)
myclt <- function(n, iter, a = 0, b = 5){
  y=stats::runif(n*iter,a,b) # A
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # B

  sm=apply(data,2,mean) # C, now use the mean function instead of sum

  h=graphics::hist(sm,plot=FALSE)
  graphics::hist(sm, col=grDevices::rainbow(length(h$mids)), freq=FALSE, main="Distribution of the mean of uniforms")
  sm
}

