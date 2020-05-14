library(moments)

# prepare vars
size <- c(20, 100)
gamma <- 0.95
alpha <- 1 - gamma
quant <- 1 - alpha / 2

for (n in size) {
  print(n)
  
  # generate data and get sample mean and var
  data <- rnorm(n)
  m <- mean(data)
  s <- sd(data)
  
  
  # for norm distribution
  # calculate error for mean confidence interval
  error <- s * qt(quant, df=n-1) / sqrt(n - 1)
  
  # get mean confidence interval
  mean_left_reg <- m - error
  mean_right_reg <- m + error
  
  # calculate error for var confidence interval
  left_err <- sqrt(n) / sqrt(qchisq(quant, df=n-1))
  right_err <- sqrt(n) / sqrt(qchisq(alpha / 2, df=n-1))
  
  # get var confidence interval
  s_left_reg <- s * left_err
  s_right_reg <- s * right_err
  
  
  # asymptote method for any disctribution
  # calculate error for mean confidence interval
  error <- qnorm(quant) * s / sqrt(n)
  
  # get mean confidence interval
  mean_left_as <- m - error
  mean_right_as <- m + error
  
  # calculate error for var confidence interval
  U <- qnorm(quant) * sqrt((kurtosis(data) + 2) / n)
  left_err <- s * (1 - U / 2)
  right_err <- s * (1 + U / 2)
  
  # get var confidence interval
  s_left_as <- s * left_err
  s_right_as <- s * right_err
  
  
  # print stuff
  print(data.frame(c(m, s), row.names = c("Mean", "Var")))
  
  m <- matrix(c(mean_left_reg, mean_right_as,
                s_left_reg, s_right_reg,
                mean_left_as, mean_right_as,
                s_left_as, s_right_as), ncol=2, byrow=TRUE)
  colnames(m) <- c("Left", "Right")
  df <- data.frame(m, row.names=c("Mean Reg",
                           "Var Reg",
                           "Mean As",
                           "Var As"))
  print(df)
}