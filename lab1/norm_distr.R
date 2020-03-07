norm.distr <- function(num) {
 x11()

 x <- seq(-5, 5, by=0.01)
 data <- rnorm(num, 0, 1)
 hist(data, 
	freq=FALSE, 
	main=paste("Normal distribution, ", toString(num), "elements"),
	xlim=c(-5, 5),
	ylim=c(0, 0.7))

 curve(dnorm(x, 0, 1), add=TRUE) 
}