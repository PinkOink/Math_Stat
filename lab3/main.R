library(LaplacesDemon)

nums <- c(20, 100)
times <- 1000

dataNormList <- list()
dataPoisList <- list()
dataLaplList <- list()
dataCauchList <- list()
dataUnifList <- list()

for (i in 1:length(nums)) {
  num <- nums[i]
  
  dataNorm <- rnorm(num, 0, 1)
  dataPois <- rpois(num, 10)
  dataLapl <- rlaplace(num, 0, 1)
  dataCauch <- rcauchy(num, 0, 1)
  dataUnif <- runif(num, -sqrt(3), sqrt(3))
  
  dataNormList[[i]] <- dataNorm
  dataPoisList[[i]] <- dataPois
  dataLaplList[[i]] <- dataLapl
  dataCauchList[[i]] <- dataCauch
  dataUnifList[[i]] <- dataUnif
}

boxplot(dataNormList, ylab = "size", xlab = "x", names = nums, horizontal = TRUE, main = "Normal", outline = TRUE)
boxplot(dataPoisList, ylab = "size", xlab = "x", names = nums, horizontal = TRUE, main = "Poisson", outline = TRUE)
boxplot(dataLaplList, ylab = "size", xlab = "x", names = nums, horizontal = TRUE, main = "Laplace", outline = TRUE)
boxplot(dataCauchList, ylab = "size", xlab = "x", names = nums, horizontal = TRUE, main = "Cauchy (with outlines)", outline = TRUE)
boxplot(dataCauchList, ylab = "size", xlab = "x", names = nums, horizontal = TRUE, main = "Cauchy (without outlines)", outline = FALSE)
boxplot(dataUnifList, ylab = "size", xlab = "x", names = nums, horizontal = TRUE, main = "Uniform", outline = TRUE)



for (num in nums) {
  outletRate <- matrix(nrow = 5, ncol = times)
  rownames(outletRate)<- c("Norm", "Pois", "Lapl", "Cauchy", "Unif")
  
  for (i in 1:times) {
    dataNorm <- rnorm(num, 0, 1)
    dataPois <- rpois(num, 10)
    dataLapl <- rlaplace(num, 0, 1)
    dataCauch <- rcauchy(num, 0, 1)
    dataUnif <- runif(num, -sqrt(3), sqrt(3))
    
    outNorm <- boxplot(dataNorm, plot = FALSE)
    outPois <- boxplot(dataPois, plot = FALSE)
    outLapl <- boxplot(dataLapl, plot = FALSE)
    outCauch <- boxplot(dataCauch, plot = FALSE)
    outUnif <- boxplot(dataUnif, plot = FALSE)
    
    outletRate["Norm", i] <- length(outNorm$out) / num
    outletRate["Pois", i] <- length(outPois$out) / num
    outletRate["Lapl", i] <- length(outLapl$out) / num
    outletRate["Cauchy", i] <- length(outCauch$out) / num
    outletRate["Unif", i] <- length(outUnif$out) / num
  }

  rate <- c(mean(outletRate["Norm",]),
            mean(outletRate["Pois",]),
            mean(outletRate["Lapl",]),
            mean(outletRate["Cauchy",]),
            mean(outletRate["Unif",]))
  dataFrame <- data.frame(rate, row.names = c("Norm", "Pois", "Lapl", "Cauchy", "Unif"))
  print(num)
  print(dataFrame)
}

LQ <- qnorm(0.25, 0, 1)
UQ <- qnorm(0.75, 0, 1)
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- UQ + 1.5 * (UQ - LQ)
P <- pnorm(X1) + (1 - pnorm(X2))
print(P)