#' Bootstrapping function
#'
#' @param iter number of bootstrap samples
#' @param x a vector of data
#' @param fun the function to be used to calculate the statistic
#' @param alpha the confidence level
#' @param cx the position of the confidence interval
#' @param ... additional arguments to be passed to the hist function
#'
#' @return a vector that contains the confidence interval, the function used to calculate the statistic, and the data
#' @export
#'
#' @examples
#' myboot2(10000,1:10,"mean",0.05,1.5)
#' myboot2(10000,1:10,"median",0.05,1.5)
#' myboot2(10000,1:10,"sd",0.05,1.5)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=stats::quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=graphics::hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  graphics::abline(v=pte,lwd=3,col="Black")# Vertical line
  graphics::segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  graphics::text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  graphics::text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  graphics::text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
