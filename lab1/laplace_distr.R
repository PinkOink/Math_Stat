library(LaplacesDemon)

lapl.distr <- function(num) {
 x11()

 data <- rlaplace(num, 0, 1)
 hist(data, 
	freq=FALSE, 
	main=paste("Laplace distribution, ", toString(num), "elements"), 
	xlim=c(-5,5),
	ylim=c(0,0.6))

 x <- seq(-5, 5, by=0.01)
 curve(dlaplace(x, 0, 1), add=TRUE) 
}