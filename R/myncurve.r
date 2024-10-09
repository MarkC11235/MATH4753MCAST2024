#' Plotting a normal distribution curve and finding the area under the curve for a given range. Also prints the area on the graph and prints the mean, std, and area as a list to the command line.
#'
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of the normal distribution
#' @param a upper limit of the range
#'
#' @export
#'
#' @examples
#' myncurve(0, 1, 2)
#' myncurve(0, 1, 1)
#' myncurve(5, 2, 3)
myncurve = function(mu = 0, sigma = 1, a = 0){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve = seq(-100, a, length = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(-1000, xcurve, a), c(0, ycurve, 0), col = "lightblue")

  area = round(pnorm(a, mean = mu, sd = sigma), 4)

  text(a, 0.1, paste("P(Y <= ", a, ") = ", area))

  list(mu = mu, sigma = sigma, area = area)
}

