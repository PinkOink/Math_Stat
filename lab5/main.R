library(mvtnorm)
library(mixtools)

nums <- c(20, 60, 100)
cor <- c(0, 0.5, 0.9)
times <- 1000

dim <- 2
means <- rep(0, dim)

coefs <- matrix(data=0, nrow=3, ncol=times)
rownames(coefs)<- c("pearson", "spearman", "square")

for (num in nums) {
  for (p in cor) {
    cov_mtx <- matrix(data=c(1, p, p, 1), nrow=2, ncol=2)
    for (i in 1:times) {
      x <- rmvnorm(n=num, mu=rep(0, dim), sigma=cov_mtx)
      
      coefs["pearson", i] <- cor(x=x, method="pearson")[1,2]
      coefs["spearman", i] <- cor(x=x, method="spearman")[1,2]
      
      n1 <- length(x[x[,1] >= 0 & x[,2] >= 0]) / 2
      n2 <- length(x[x[,1] < 0 & x[,2] >= 0]) / 2
      n3 <- length(x[x[,1] < 0 & x[,2] < 0]) / 2
      n4 <- length(x[x[,1] >= 0 & x[,2] < 0]) / 2
      coefs["square", i] <- ((n1 + n3) - (n2 + n4)) / num 
    }
    
    rate <- c(mean(coefs["pearson",]),
              mean(coefs["pearson",] ^ 2),
              var(coefs["pearson",]),
              mean(coefs["spearman",]),
              mean(coefs["spearman",] ^ 2),
              var(coefs["spearman",]),
              mean(coefs["square",]),
              mean(coefs["square",] ^ 2),
              var(coefs["square",]))
    dataFrame <- data.frame(rate, row.names = c("Pearson E(z)", 
                                                "Pearson E(z^2)", 
                                                "Pearson D(z)",
                                                "Spearman E(z)", 
                                                "Spearman E(z^2)", 
                                                "Spearman D(z)",
                                                "Square E(z)", 
                                                "Square E(z^2)", 
                                                "Square D(z)"))
    print(num)
    print(p)
    print(dataFrame)
    
    
    png(filename=paste("myplot", num, p, ".png"))
    plot(x, xlim=c(-3,3), ylim=c(-3,3), main = paste("p =", p, ", n =", num), xlab = "x", ylab = "y")
    ellipse(mu=means, sigma=cov_mtx)
    dev.off()
  }
}

print("Mixed")
for (num in nums) {
  cov_mtx_pos <- matrix(data=c(1, 0.9, 0.9, 1), nrow=2, ncol=2)
  cov_mtx_neg <- matrix(data=c(10, -0.9, -0.9, 10), nrow=2, ncol=2)
  
  for (i in 1:times) {
    x_1 <- rmvnorm(n=num, mu=rep(0, dim), sigma=cov_mtx_pos)
    x_2 <- rmvnorm(n=num, mu=rep(0, dim), sigma=cov_mtx_neg)
    x <- 0.9 * x_1 + 0.1 * x_2
    
    coefs["pearson", i] <- cor(x=x, method="pearson")[1,2]
    coefs["spearman", i] <- cor(x=x, method="spearman")[1,2]
    
    n1 <- length(x[x[,1] >= 0 & x[,2] >= 0]) / 2
    n2 <- length(x[x[,1] < 0 & x[,2] >= 0]) / 2
    n3 <- length(x[x[,1] < 0 & x[,2] < 0]) / 2
    n4 <- length(x[x[,1] >= 0 & x[,2] < 0]) / 2
    coefs["square", i] <- ((n1 + n3) - (n2 + n4)) / num 
  }
  
  rate <- c(mean(coefs["pearson",]),
            mean(coefs["pearson",] ^ 2),
            var(coefs["pearson",]),
            mean(coefs["spearman",]),
            mean(coefs["spearman",] ^ 2),
            var(coefs["spearman",]),
            mean(coefs["square",]),
            mean(coefs["square",] ^ 2),
            var(coefs["square",]))
  dataFrame <- data.frame(rate, row.names = c("Pearson E(z)", 
                                              "Pearson E(z^2)", 
                                              "Pearson D(z)",
                                              "Spearman E(z)", 
                                              "Spearman E(z^2)", 
                                              "Spearman D(z)",
                                              "Square E(z)", 
                                              "Square E(z^2)", 
                                              "Square D(z)"))
  print(num)
  print(p)
  print(dataFrame)
}