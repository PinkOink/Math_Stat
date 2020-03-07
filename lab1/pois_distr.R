pois.distr <- function(num) {
 x11()

 data <- rpois(num, 10)
 hist(data, 
	 
	freq=FALSE, 
	main=paste("Poisson distribution, ", toString(num), "elements"),
	xlim=c(0, 20),
	ylim=c(0, 0.4))

 x <- seq(0, 20, by=1)
 lines(x, dpois(x, 10)) 
}