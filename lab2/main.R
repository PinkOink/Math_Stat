library(LaplacesDemon)

options("scipen"=100, "digits"=3)

nums <- c(10, 100, 1000)
times <- 1000

for(num in nums){
  means <- matrix(0, 5 , times)
  meds <- matrix(0, 5 , times)
  zR <- matrix(0, 5 , times)
  zQ <- matrix(0, 5 , times)
  zTr <- matrix(0, 5 , times)
  
  rownames(means) <- c("Norm", "Pois", "Lapl", "Cauchy", "Unif")
  rownames(meds) <- c("Norm", "Pois", "Lapl", "Cauchy", "Unif")
  rownames(zR) <- c("Norm", "Pois", "Lapl", "Cauchy", "Unif")
  rownames(zQ) <- c("Norm", "Pois", "Lapl", "Cauchy", "Unif")
  rownames(zTr) <- c("Norm", "Pois", "Lapl", "Cauchy", "Unif")
  
  for (i in 1:times){
    dataNorm <- rnorm(num, 0, 1)
    dataPois <- rpois(num, 10)
    dataLapl <- rlaplace(num, 0, 1)
    dataCauch <- rcauchy(num, 0, 1)
    dataUnif <- runif(num, -sqrt(3), sqrt(3))
    
    means["Norm", i] <- mean(dataNorm)
    means["Pois", i] <- mean(dataPois)
    means["Lapl", i] <- mean(dataLapl)
    means["Cauchy", i] <- mean(dataCauch)
    means["Unif", i] <- mean(dataUnif)
    
    meds["Norm", i] <- median(dataNorm)
    meds["Pois", i] <- median(dataPois)
    meds["Lapl", i] <- median(dataLapl)
    meds["Cauchy", i] <- median(dataCauch)
    meds["Unif", i] <- median(dataUnif)
    
    zR["Norm", i] <- (min(dataNorm) + max(dataNorm)) / 2
    zR["Pois", i] <- (min(dataPois) + max(dataPois)) / 2
    zR["Lapl", i] <- (min(dataLapl) + max(dataLapl)) / 2
    zR["Cauchy", i] <- (min(dataCauch) + max(dataCauch)) / 2
    zR["Unif", i] <- (min(dataUnif) + max(dataUnif)) / 2
    
    zQ["Norm", i] <- (quantile(dataNorm, 0.25) + quantile(dataNorm, 0.75)) / 2
    zQ["Pois", i] <- (quantile(dataPois, 0.25) + quantile(dataPois, 0.75)) / 2
    zQ["Lapl", i] <- (quantile(dataLapl, 0.25) + quantile(dataLapl, 0.75)) / 2
    zQ["Cauchy", i] <- (quantile(dataCauch, 0.25) + quantile(dataCauch, 0.75)) / 2
    zQ["Unif", i] <- (quantile(dataUnif, 0.25) + quantile(dataUnif, 0.75)) / 2
    
    zTr["Norm", i] <- mean(dataNorm, trim = 0.25)
    zTr["Pois", i] <- mean(dataPois, trim = 0.25)
    zTr["Lapl", i] <- mean(dataLapl, trim = 0.25)
    zTr["Cauchy", i] <- mean(dataCauch, trim = 0.25)
    zTr["Unif", i] <- mean(dataUnif, trim = 0.25)
  }
  
  meanStatE <- round(c(mean(means["Norm",]),
                 mean(means["Pois",]),
                 mean(means["Lapl",]),
                 mean(means["Cauchy",]),
                 mean(means["Unif",])), digits=5)
  medStatE <- round(c(mean(meds["Norm",]),
                mean(meds["Pois",]),
                mean(meds["Lapl",]),
                mean(meds["Cauchy",]),
                mean(meds["Unif",])), digits=5)
  zRStatE <- round(c(mean(zR["Norm",]),
               mean(zR["Pois",]),
               mean(zR["Lapl",]),
               mean(zR["Cauchy",]),
               mean(zR["Unif",])), digits=5)
  zQStatE <- round(c(mean(zQ["Norm",]),
               mean(zQ["Pois",]),
               mean(zQ["Lapl",]),
               mean(zQ["Cauchy",]),
               mean(zQ["Unif",])), digits=5)
  zTrStatE <- round(c(mean(zTr["Norm",]),
                mean(zTr["Pois",]),
                mean(zTr["Lapl",]),
                mean(zTr["Cauchy",]),
                mean(zTr["Unif",])), digits=5)
  
  dataEFrame <- data.frame(
    mean = meanStatE,
    med = medStatE,
    zR = zRStatE,
    zQ = zQStatE,
    zTr = zTrStatE,
    row.names = c("Norm", "Pois", "Lapl", "Cauchy", "Unif"))
  print(num)
  print(dataEFrame)
  
  meanStatD <- round(c(mean(means["Norm",]^2) - (mean(means["Norm",]))^2,
                mean(means["Pois",]^2) - (mean(means["Pois",]))^2,
                mean(means["Lapl",]^2) - (mean(means["Lapl",]))^2,
                mean(means["Cauchy",]^2) - (mean(means["Cauchy",]))^2,
                mean(means["Unif",]^2) - (mean(means["Unif",]))^2), digits=5)
  medStatD <- round(c(mean(meds["Norm",]^2) - (mean(meds["Norm",]))^2,
                mean(meds["Pois",]^2) - (mean(meds["Pois",]))^2,
                mean(meds["Lapl",]^2) - (mean(meds["Lapl",]))^2,
                mean(meds["Cauchy",]^2) - (mean(meds["Cauchy",]))^2,
                mean(meds["Unif",]^2) - (mean(meds["Unif",]))^2), digits=5)
  zRStatD <- round(c(mean(zR["Norm",]^2) - (mean(zR["Norm",]))^2,
               mean(zR["Pois",]^2) - (mean(zR["Pois",]))^2,
               mean(zR["Lapl",]^2) - (mean(zR["Lapl",]))^2,
               mean(zR["Cauchy",]^2) - (mean(zR["Cauchy",]))^2,
               mean(zR["Unif",]^2) - (mean(zR["Unif",]))^2), digits=5)
  zQStatD <- round(c(mean(zQ["Norm",]^2) - (mean(zQ["Norm",]))^2,
              mean(zQ["Pois",]^2) - (mean(zQ["Pois",]))^2,
              mean(zQ["Lapl",]^2) - (mean(zQ["Lapl",]))^2,
              mean(zQ["Cauchy",]^2) - (mean(zQ["Cauchy",]))^2,
              mean(zQ["Unif",]^2) - (mean(zQ["Unif",]))^2), digits=5)
  zTrStatD <- round(c(mean(zTr["Norm",]^2) - (mean(zTr["Norm",]))^2,
              mean(zTr["Pois",]^2) - (mean(zTr["Pois",]))^2,
              mean(zTr["Lapl",]^2) - (mean(zTr["Lapl",]))^2,
              mean(zTr["Cauchy",]^2) - (mean(zTr["Cauchy",]))^2,
              mean(zTr["Unif",]^2) - (mean(zTr["Unif",]))^2), digits=5)
  
  
  dataDFrame <- data.frame(
    mean = meanStatD,
    med = medStatD,
    zR = zRStatD,
    zQ = zQStatD,
    zTr = zTrStatD,
    row.names = c("Norm", "Pois", "Lapl", "Cauchy", "Unif"))
  print(num)
  print(dataDFrame)
}