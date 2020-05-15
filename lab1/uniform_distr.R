unif.distr <- function(num) {
 x11()

 data <- runif(num, -sqrt(3), sqrt(3))
 hist(data, 
	 
	freq=FALSE, 
	main=paste("Uniform distribution, ", toString(num), "elements"),
	xlim=c(-sqrt(3)-1, sqrt(3)+1),
	ylim=c(0, 0.8))

 x <- seq(-sqrt(3), sqrt(3), by=0.01)
 curve(dunif(x, -sqrt(3), sqrt(3)), add=TRUE) 
}