#' @title Normal distribution plot
#' @description \code{testNormalPlot} Plot normal distribution
#'
#' @importFrom stats rnorm
#' @importFrom stats dnorm
#' @importFrom graphics curve
#' @importFrom graphics hist
#' @param av1 mean value
#' @param sd1 standard deviation
#' @return Average and Standard Deviation
#' @export
#' @examples
#' # testNormalPlot(50, 10)

testNormalPlot <- function(av1, sd1){

  # x-axix settings
  xmn <- av1-sd1*5
  xmx <- av1+sd1*5

  n <- 1000
  x1 <- seq(xmn, xmx, length=n)
  mx1 <- max( dnorm(x1,av1,sd1) )
  mx <- mx1 * 1.1


  # draw the normal distributions
  hist( rnorm(10000, mean = av1, sd = sd1) , freq = FALSE)
  curve(dnorm(x,av1,sd1),xmn,xmx,col = "red",lwd=1,xlab="", ylab="", ylim=c(0,mx),add = TRUE)
  return(list(Average=av1, StandardDev=sd1))
}

