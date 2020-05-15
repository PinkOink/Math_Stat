# prepare data
size <- 100
alpha <- 0.05
data <- rnorm(size)

# calculate mean and sigma
m <- mean(data)
s <- var(data)

# prepare hist data
h <- hist(data)

# count interval probs
p <- pnorm(h$breaks, mean=m, sd=s)
p_interval <- c()
for (i in 2:(length(p)-2)) {
  p_interval[i] <- p[i + 1] - p[i]
}
p_interval[1] <- p[2]
p_interval[length(p_interval) + 1] <- 1 - p[length(p) - 1]

# chi-test
chi <- qchisq(p=0.95,df=length(h$breaks) - 2)
chi_test <- chisq.test(h$counts, p=p_interval)

# print stuff
print("mean")
print(m)

print("sigma")
print(s)

df <- data.frame(interval=h$breaks[2:length(h$breaks)], 
                 ni=chi_test$observed, 
                 pi=p_interval, 
                 npi=chi_test$expected,
                 ni_npi=chi_test$observed - chi_test$expected,
                 lastcol=chi_test$residuals ^2)

print(df)

print("chi_expected")
print(chi)

print("chi")
print(chi_test$statistic)

print("n_i")
print(sum(chi_test$observed))

print("p_i")
print(sum(p_interval))

print("n p_i")
print(sum(chi_test$expected))

print("n_i - n p_i")
print(sum(chi_test$observed - chi_test$expected))

print("last col")
print(sum(chi_test$residuals ^2))