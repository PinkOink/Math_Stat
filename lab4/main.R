library(LaplacesDemon)

nums <- c(20, 60, 100)

x <- seq(-4, 4, by=0.01)
x_puas <- seq(6, 14, by=0.01)


for(num in nums) {
  dataNorm <- rnorm(num, 0, 1)
  dataPois <- rpois(num, 10)
  dataLapl <- rlaplace(num, 0, 1)
  dataCauch <- rcauchy(num, 0, 1)
  dataUnif <- runif(num, -sqrt(3), sqrt(3))
  
  plot(x, pnorm(x, 0, 1), type = 'l', ylab = "F(X)", xlab = "x", main = paste("Normal, n=", num), col = "blue") 
  lines(ecdf(dataNorm), verticals=TRUE, do.points=FALSE)
  plot(x_puas, ppois(x_puas, 10), type = 'l', ylab = "F(X)", xlab = "x", main = paste("Pois, n=", num), col = "blue") 
  lines(ecdf(dataPois), verticals=TRUE, do.points=FALSE)
  plot(x, plaplace(x, 0, 1), type = 'l', ylab = "F(X)", xlab = "x", main = paste("Laplace, n=", num), col = "blue") 
  lines(ecdf(dataLapl), verticals=TRUE, do.points=FALSE)
  plot(x, pcauchy(x, 0, 1), type = 'l', ylab = "F(X)", xlab = "x", main = paste("Cauchy, n=", num), col = "blue") 
  lines(ecdf(dataCauch), verticals=TRUE, do.points=FALSE)
  plot(x, punif(x, -sqrt(3), sqrt(3)), type = 'l', ylab = "F(X)", xlab = "x", main = paste("Unif, n=", num), col = "blue") 
  lines(ecdf(dataUnif), verticals=TRUE, do.points=FALSE)
  
  for(coef in c(0.5, 1, 2)){
    kdenNorm <- density(dataNorm, kernel = "gaussian", bw = "nrd0", adjust = coef)
    kdenPois <- density(dataPois, kernel = "gaussian", bw = "nrd0", adjust = coef)
    kdenLapl <- density(dataLapl, kernel = "gaussian", bw = "nrd0", adjust = coef)
    kdenCauch <- density(dataCauch, kernel = "gaussian", bw = "nrd0", adjust = coef)
    kdenUnif <- density(dataUnif, kernel = "gaussian", bw = "nrd0", adjust = coef)
    
    plot(kdenNorm, ylim = c(0, 1), xlim = c(-4, 4), main = paste("Normal, coef=", coef, ", n=", num), xlab = "x", ylab = "f(x)")
    curve(dnorm(x, 0, 1), add=TRUE, col = "blue") 
    plot(kdenPois, ylim = c(0, 1), xlim = c(6, 14), main = paste("Pois, coef=", coef, ", n=", num), xlab = "x", ylab = "f(x)")
    lines(seq(6, 14, by=1), dpois(seq(6, 14, by=1), 10), col = "blue") 
    plot(kdenLapl, ylim = c(0, 1), xlim = c(-4, 4), main = paste("Laplace, coef=", coef, ", n=", num), xlab = "x", ylab = "f(x)")
    curve(dlaplace(x, 0, 1), add=TRUE, col = "blue") 
    plot(kdenCauch, ylim = c(0, 1), xlim = c(-4, 4), main = paste("Cauchy, coef=", coef, ", n=", num), xlab = "x", ylab = "f(x)")
    curve(dcauchy(x, 0, 1), add=TRUE, col = "blue") 
    plot(kdenUnif, ylim = c(0, 1), xlim = c(-4, 4), main = paste("Unif, coef=", coef, ", n=", num), xlab = "x", ylab = "f(x)")
    curve(dunif(x, -sqrt(3), sqrt(3)), add=TRUE, col = "blue") 
  }
}