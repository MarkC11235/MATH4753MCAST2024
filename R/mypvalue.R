#' @title Calculate the p-value for a t-test
#'
#' @param t0 the t value to test
#' @param x the vector of quantiles to plot
#' @param xmax the maximum x value to plot
#' @param n the degrees of freedom
#' @param alpha the alpha level for the test
#'
#' @return a list with the quantiles and the p-value
#' @export
#'
#' @examples
#' mypvalue(2.5)
#' mypvalue(2.5, xmax=5)
#' mypvalue(2.5, xmax=5, n=30)
#' mypvalue(2.5, xmax=5, n=30, alpha=0.01)
mypvalue=function(t0, x, xmax=4,n=20, alpha=0.05){
  #calculate alpha/2
  va=round(stats::pt(-t0,df=n-1),4)
  pv=2*va

  # plot the t dist
  graphics::curve(stats::dt(x,df=n-1),xlim=c(-xmax,xmax),ylab="T Density",xlab=expression(t),
        main=substitute(paste("P-value=", pv, " alpha=", alpha)))


  # set up points on the polygon to the right
  xcurve=seq(t0,xmax,length=1000)
  ycurve=stats::dt(xcurve,df=n-1)

  # set up points to the left
  xlcurve=seq(-t0,-xmax,length=1000)
  ylcurve=stats::dt(xcurve,df=n-1)

  # Shade in the polygon defined by the line segments
  graphics:: polygon(c(t0,xcurve,xmax),c(0,ycurve,0),col="green")
  graphics::polygon(c(-t0,xlcurve,-xmax),c(0,ylcurve,0),col="green")

  # make quantiles
  q=stats::qt(1-alpha/2,n-1)
  graphics::abline( v=c(q,-q),lwd=2) # plot the cut off t value
  graphics::axis(3,c(q,-q),c(expression(abs(t[alpha/2])),expression(-abs(t[alpha/2]))))


  # Annotation
  graphics::text(0.5*(t0+xmax),max(ycurve),substitute(paste(area, "=",va)))
  graphics::text(-0.5*(t0+xmax),max(ycurve),expression(area))

  return(list(q=q,pvalue=pv))
}
