cauchy.distr <- function(num) {
 x11()

 data <- rcauchy(num, 0, 1)
 hist(data, 
	breaks=num,
	freq=FALSE, 
	main=paste("Cauchy distribution, ", toString(num), "elements"),
	xlim=c(-20, 20),
	ylim=c(0, 0.4))

 x <- seq(-20, 20, by=0.01)
 curve(dcauchy(x, 0, 1), add=TRUE) 
}